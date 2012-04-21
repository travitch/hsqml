{-# LANGUAGE TemplateHaskell #-}
-- | This module contains the TemplateHaskell helper to define
-- QObjects, along with supporting data types and helpers used in the
-- TH expansion.
module Graphics.QML.Internal.TH (
  -- * Types
  ClassDefinition(..),
  QPointer,

  -- * Functions
  defClass,
  defSignal
  ) where

import Data.Bits
import Data.IORef
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as M

import Foreign.Marshal.Alloc ( allocaBytes, alloca )
import Foreign.Ptr ( Ptr, castPtr, nullPtr )
import Foreign.Storable ( Storable, peekElemOff, pokeElemOff, sizeOf )
import Language.Haskell.TH
import System.IO.Unsafe ( unsafePerformIO )

import Graphics.QML.Internal.Core
import Graphics.QML.Internal.Classes ( hsqmlEmitSignal )

{-# NOINLINE signalNameMap #-}
signalNameMap :: IORef (Map String Name)
signalNameMap = unsafePerformIO $ newIORef M.empty

signalNameRecord :: Name -> Name -> IO ()
signalNameRecord mangledName realName =
  modifyIORef signalNameMap (M.insert (nameBase mangledName) realName)

signalNameLookup :: Name -> IO Name
signalNameLookup n = do
  m <- readIORef signalNameMap
  case M.lookup (nameBase n) m of
    Nothing -> error ("Name not available in signalNameMap: " ++ show n)
    Just realName -> return realName

-- | This function takes a declarative class description (via the
-- 'ClassDefinition' type) and converts it into an instance
-- declaration and definitions of all of the requested signals.
defClass :: ClassDefinition -> Q [Dec]
defClass cd = do
  let clsName = className cd
  -- This is the instance declaration of the form:
  --
  -- > instance MetaObject <Type> where
  -- >   classDefinition = cd
  --
  -- Since we want to use the argument in the generated code, we have
  -- to lift it into an Exp in the Q monad.
  --
  -- The Marhsal instance for the type will handle actually using this
  -- definition to create and register the type inside of Qt.
  let tdef = translateDef cd
      clsDef = valD (varP 'classDefinition) (normalB tdef) []
      itype = appT (conT ''MetaObject) (conT clsName)

  instanceDec <- instanceD (cxt []) itype [clsDef]

  return $! [instanceDec]

translateDef :: ClassDefinition -> ExpQ
translateDef pcd = do
  definedSignals <- signalsForType (className pcd)
  let (majV, minV) = classVersion pcd
      cdName = 'InternalClassDef
      uriField = fieldExp '_classURI (litE (stringL (classURI pcd)))
      verField = fieldExp '_classVersion (tupE [mkIntLit majV, mkIntLit minV])
      sigField = fieldExp '_classSignals (trSigs definedSignals)
      methField = fieldExp '_classMethods (trMethods (classMethods pcd))
      propField = fieldExp '_classProperties (trProperties (classProperties pcd))
      consField = fieldExp '_classConstructor (varE (classConstructor pcd))
      accField = fieldExp '_classSelfAccessor (varE (classSelfAccessor pcd))

      flds = [ uriField
             , verField
             , sigField
             , methField
             , propField
             , consField
             , accField
             ]

  recConE cdName flds
  where
    mkIntLit :: Int -> ExpQ
    mkIntLit = litE . integerL . fromIntegral

trProperties :: [ProtoClassProperty] -> ExpQ
trProperties ps = listE (map trProp ps)
  where
    trProp (PProperty n g s fs) = do
      -- Look at the type of the getter (since it must exist) to infer
      -- the type of the property.
      VarI _ gt _ _ <- reify g
      let (_, propTypeIO) = splitTypes gt
          propType = removeIOWrapper propTypeIO
          flag = foldr (.|.) 0 fs
          c1 = appE (conE 'Property) (litE (stringL n))
          c2 = appE c1 (typeToTypeNameExp propType)
          c3 = appE c2 (mkUniformFunc g)
          c4 = appE c3 (maybeMkUniformFunc s)
          flagEx = appE (varE 'fromIntegral) (litE (integerL (fromIntegral flag)))
      appE c4 flagEx

    mkUniformFunc n = do
      let mfuncName = mkName "marshalFunc0"
          mfunc = varE mfuncName
          dec = defMarshalFunc 0
      letE [dec] (appE mfunc (varE n))

    maybeMkUniformFunc Nothing = conE 'Nothing
    maybeMkUniformFunc (Just n) = do
      let mfunc = varE 'hsqmlMarshalMutator
      appE (conE 'Just) (appE mfunc (varE n))

-- | Translate a ProtoMethod to an Exp representing its equivalent
-- Method.
--
-- >
-- > Method { methodName = n, methodFunc = defMethodN mref
-- >        , methodTypes = [ mTypeOf (undefined :: t1), ..] }
trMethods :: [ProtoClassMethod] -> Q Exp
trMethods ms = listE (map trMeth ms)
  where
    trMeth (PMethod qname mref) = do
      -- Figure out the type of the function that the user gave us.
      VarI _ t _ _ <- reify mref
      let (argTypes, rTypeIO) = splitTypes t
          rType = removeIOWrapper rTypeIO
          c1 = appE (conE 'Method) (litE (stringL qname))
          -- Use tail on argTypes so that we can drop the this pointer
          -- (which isn't counted in qt method descriptions).
          mtypes = listE $! map typeToTypeNameExp (rType : tail argTypes)
          c2 = appE c1 mtypes
          mfunc = mkUniformFunc mref (tail argTypes)
      appE c2 mfunc

    mkUniformFunc fname ts = do
      let mfuncName = mkName ("marshalFunc" ++ show (length ts))
          mfunc = varE mfuncName
          dec = defMarshalFunc (length ts)
      letE [dec] (appE mfunc (varE fname))

typeToTypeNameExp :: Type -> ExpQ
typeToTypeNameExp t = appE (varE 'mTypeOf) uv
  where
    uv = sigE (varE 'undefined) (return t)

-- | Given the Type of a function, split it into the list of argument
-- types and the return type.  The list of argument types is built up
-- in reverse, so reverse it before returning.
--
-- This attempts to give useful errors when encountering a type it
-- can't handle.  Notably, type variables (which are introduced by a
-- forall at this level) are not supported.
splitTypes :: Type -> ([Type], Type)
splitTypes ty =
  let (rt : rest) = split' [] ty
  in (reverse rest, rt)
  where
    split' _ (ForallT _ _ _) =
      error ("Type variables are not supported in methods signatures: "
             ++ pprint ty ++ ".  Fix these types with a type signature.")
    split' acc (AppT (AppT ArrowT t) rest) = split' (t : acc) rest
    split' acc t = t : acc

removeIOWrapper :: Type -> Type
removeIOWrapper (AppT _ inner) = inner
removeIOWrapper t = error ("Illegal type (not wrapped in IO): " ++ pprint t)

-- | Convert a ProtoSignal to an Exp representing a Signal to be
-- spliced into the ClassDefinition
trSigs :: [Info] -> ExpQ
trSigs = listE . map trSig
  where
    -- | Convert the Info object from a Signal into a Signal
    -- constructor call
    trSig (VarI sigMangledName sigTy _ _) = do
      sigName <- runIO $ signalNameLookup sigMangledName
      let (ts, _) = splitTypes sigTy
          c1 = appE (conE 'Signal) (litE (stringL (nameBase sigName)))
          tns = listE (map trType (tail ts))
      appE c1 tns
    trSig i = error ("Expected variable info: " ++ show i)
    -- Convert a type to a runtime type in terms of mTypeOf
    trType :: Type -> ExpQ
    trType t =
      let uv = sigE (varE 'undefined) (return t)
      in appE (varE 'mTypeOf) uv

--
-- Signal definitions
--

-- | Define a signal
--
-- > defSignal ''ClassName "signalName" [''Int, ''Int, ''String]
--
-- defines a signal for ClassName that takes three arguments.
--
-- This function actually defines two things.  The first is the
-- internal actual definition of the signal and the second is the
-- user-facing version that references it.
--
-- > qmlInternalSignal[ClassName]_N = ...
-- > signalName = qmlInternalSignal[ClassName]_N
defSignal :: Name -> String -> [Name] -> Q [Dec]
defSignal clsName sigName argTypes = do
  currentSignals <- signalsForType clsName
  let signo = length currentSignals
  buildSignal clsName signo (mkName sigName) argTypes

mkFunType :: [Name] -> TypeQ
mkFunType = foldr addT iot
  where
    addT t acc = appT (appT arrowT (conT t)) acc
    iot = appT (conT ''IO) (tupleT 0)

-- | Builds a function to emit a signal.  It is of the form:
--
-- > signalName :: tt -> t1 -> t2 -> .. -> IO ()
-- > signalName self v0 v1 v2 ... =
-- >   allocaBytes sz marshalAndCall
-- >   where
-- >     sz = (length vs) * sizeof(nullPtr)
-- >     marshalAndCall p0 = ...
--
-- The extra accessor is to get the pointer to the underlying QObject
-- instead of the Haskell-side user data.  This pointer is required
-- for the signal dispatch.
buildSignal :: Name -> Int -> Name -> [Name] -> Q [Dec]
buildSignal clsName signo sigName argTypes = do
  -- This is the internal signal name, tagged with its number for
  -- lookup later.
  selfName <- newName "self"
  let sigInternalName = mkName (signalBaseName clsName ++ show signo)
  let sigTy = appT (appT arrowT (conT clsName)) (mkFunType argTypes)

      -- Make a list of variables self v0..vn where self will be the
      -- this ptr
      ixs :: [Int]
      ixs = [0..]
      argVars = take (length argTypes) $ map (\i -> mkName ("v" ++ show i)) ixs
      cpatt = varP selfName : map varP argVars

  -- The body of the signal needs to eventually call hsqmlEmitSignal;
  -- however, we want to safely use a stack allocated array so we call
  -- through allocaBytes
  szName <- newName "sz"
  marshalAndCallName <- newName "marshalAndCall"
  let szRef = varE szName
      marshalAndCall = varE marshalAndCallName
      body0 = appE (varE 'allocaBytes) szRef
      body1 = appE body0 marshalAndCall

      -- | The size is the sum of all of the sizes of the arguments
      -- (we need this to compute the size of the buffer to allocate).
      -- Add an extra slot for a NULL pointer for the return address.
      -- This is always unused in signals.
      --
      -- > (nArgs + 1) * qmlStorableSizeOf (undefined :: QPointer)
      sizeOfFunc = varE 'sizeOf
      undefVal = varE 'undefined
      ptrType = conT ''QPointer
      ptrSize = appE sizeOfFunc (sigE undefVal ptrType)
      mulOp = varE '(*)
      nSlots = litE (integerL (fromIntegral (1 + (length argTypes))))
      szBody = infixApp ptrSize mulOp nSlots
      szDef = valD (varP szName) (normalB szBody) []

      argsWithTypes = zip argVars argTypes
      mshDef = mkMarshalAndCall signo marshalAndCallName (varE selfName) argsWithTypes

  internalSig <- sigD sigInternalName sigTy
  internalDef <- funD sigInternalName [clause cpatt (normalB body1) [szDef, mshDef]]
  externSig <- sigD sigName sigTy
  externDef <- funD sigName [clause [] (normalB (varE sigInternalName)) []]

  runIO $ signalNameRecord sigInternalName sigName
  return [internalSig, internalDef, externSig, externDef]


-- | Make a function that takes a pointer to an allocated array of
-- pointers.  Fills the array with pointers to allocad memory and then
-- passes the filled array to hsqmlEmitSignal.
--
-- >     marshalAndCall p0 = do
-- >       alloca $ \x0 -> do
-- >         let x0t = (x0 :: Ptr t0)
-- >         marshal x0t v0
-- >         pokeElemOff p0 0 x0t
-- >         alloca $ \x1 -> do
-- >           let x1t = (x1 :: Ptr t1)
-- >           marshal x1t v1
-- >           pokeElemOff p0 1 x1t
-- >           ..
-- >           hsqmlEmitSignal (_classSelfAccessor self) signum p0
--
-- Note the extra let bindings in each alloca.  These are needed to
-- fix the type of the allocated pointer (since alloca allocates a
-- pointer to a particular type).  We can't use a simple type
-- annotation on the lambda argument without forcing users to enable
-- ScopedTypeVariables in each source file, so this is the simplest
-- approach.
mkMarshalAndCall :: Int -> Name -> ExpQ -> [(Name, Name)] -> DecQ
mkMarshalAndCall signo mname self vs = do
  p0Name <- newName "vec"
  -- Start by generating the innermost statement (the call to
  -- hsqmlEmitSignal) and then just wrap.
  let ixs :: [Int]
      ixs = [0..]
      body = doE [foldr (wrapInArgMarshal p0Name) (mkEmit p0Name) (zip ixs vs)]
      defClause = clause [varP p0Name] (normalB body) []
  funD mname [defClause]
  where
    mshlFunc = varE 'marshal
    allocaFunc = varE 'alloca
    pokeFunc = varE 'pokeElemOff
    castFunc = varE 'castPtr

    wrapInArgMarshal p0 (argno, (argName, argTyName)) innerExp = do
      xN <- newName ("x" ++ show argno)
      xNt <- newName ("x" ++ show argno ++ "t")
      let ptrTy = appT (conT ''Ptr) (conT argTyName)
          argSig = sigD xNt ptrTy
          argBind = valD (varP xNt) (normalB (varE xN)) []
          letBind = letS [argSig, argBind]
          mar = mkMshl xNt argName
          poke = mkPoke p0 (argno + 1) xNt
          doBlock = doE [ letBind, mar, poke, innerExp ]
      noBindS $ appE allocaFunc (lam1E (varP xN) doBlock)

    -- | > marshal p v
    mkMshl p v =
      let castedPtr = appE castFunc (varE p)
      in noBindS $ appE (appE mshlFunc castedPtr) (varE v)
    mkPoke p0 argno xN =
      let ix = litE (integerL (fromIntegral argno))
          castedPtr = appE castFunc (varE xN)
      in noBindS $ appE (appE (appE pokeFunc (varE p0)) ix) castedPtr
    -- | > hsqmlEmitSignal self signo p0
    mkEmit p0Name =
      let emit = varE 'hsqmlEmitSignal
          sigEx = litE (integerL (fromIntegral signo))
          accFunc = appE (varE '_classSelfAccessor) (varE 'classDefinition)
          selfToPtr = appE accFunc self
      in noBindS $ appE (appE (appE emit selfToPtr) sigEx) (varE p0Name)

-- | Builds a marshaller from Haskell function with the given arity to
-- a UniformFunc (which can be called by Qt).
defMarshalFunc :: Int -> DecQ
defMarshalFunc i = do
  r <- newName "r"
  pv <- newName "pv"
  f <- newName "f"
  p0 <- newName "p0"
  pr <- newName "pr"
  v0 <- newName "v0"
  let vNs = map (\ix -> varE (mkName ("v" ++ show ix))) [1..i]
      name = mkName ("marshalFunc" ++ show i)
      fpatt = [varP f, varP p0, varP pv]
      peekPr = bindS (varP pr) (appE (appE peekOff (varE pv)) (litE (integerL 0)))
      unmarThis = bindS (varP v0) (appE unmar (varE p0))
      peeks = map (mkPeek pv) [1..i]
      unmars = map mkUnm [1..i]
      -- The call with all of the unmarshaled params
      call = bindS (varP r) $ foldl' appE (appE (varE f) (varE v0)) vNs
      ret = noBindS $ (appE (appE marRet (varE pr)) (varE r))

      body = doE $! concat [[peekPr, unmarThis], peeks, unmars, [call, ret]]
      cls = clause fpatt (normalB body) []

  funD name [cls]
  where
    marRet = varE 'hsqmlMarshalRet
    peekOff = varE 'peekElemOff
    unmar = varE 'unmarshal
    mkPeek pv ix =
      let p = mkName ("p" ++ show ix)
      in bindS (varP p) (appE (appE peekOff (varE pv)) (litE (integerL (fromIntegral i))))
    mkUnm ix =
      let v = mkName ("v" ++ show ix)
          p = mkName ("p" ++ show ix)
      in bindS (varP v) (appE unmar (varE p))

tryReify :: Name -> Q (Maybe Info)
tryReify name = reify name >>= (return . Just)

signalBaseName :: Name -> String
signalBaseName clsName = "qmlInternalSignal[" ++ nameBase clsName ++ "]_"

-- | We use a naming convention for the private name of each signal:
--
--   qmlInternalSignal[ClassName]_N = realSigDef
--   signalName = qmlInternalSignal[ClassName]_N
--
-- where N is in [0..] (and is the internally used signal number).
-- This convention lets us look up which signals are defined for the
-- class so far using lookupValueName.
signalsForType :: Name -> Q [Info]
signalsForType clsName = do
  let sigNumbers :: [Int]
      sigNumbers = [0..]
  mapUntilM checkName sigNumbers
  where
    baseName = signalBaseName clsName
    checkName n =
      let sigName = mkName (baseName ++ show n)
      in recover (return Nothing) (tryReify sigName)

-- | Applies f to each element in the input list until f returns
-- Nothing or there are no more list elements.  Returns the list of
-- Just values
mapUntilM :: (Monad m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapUntilM f = go []
  where
    go acc [] = return acc
    go acc (e:es) = do
      r <- f e
      case r of
        Nothing -> return acc
        Just r' -> go (r' : acc) es


hsqmlMarshalMutator :: (Marshallable a, Marshallable b)
                       => (a -> b -> IO ())
                       -> UniformFunc
hsqmlMarshalMutator f p0 pv = do
  p1 <- peekElemOff pv 0
  v0 <- unmarshal p0
  v1 <- unmarshal p1
  f v0 v1


hsqmlMarshalRet :: (Marshallable tt) => Ptr () -> tt -> IO ()
hsqmlMarshalRet ptr obj
  | ptr == nullPtr = return ()
  | otherwise      = marshal ptr obj

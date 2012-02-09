{-# LANGUAGE TemplateHaskell #-}
module Graphics.QML.Internal.TH where

import Data.Bits
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Language.Haskell.TH

import Graphics.QML.Internal.Classes
import Graphics.QML.Internal.Core

data ProtoClassProperty =
  PProperty { pPropertyName :: String
            , pPropertyReadFunc :: Name
            , pPropertyWriteFunc :: Maybe Name
            , pPropertyFlags :: [CUInt]
            }

data Property =
  Property { propertyName :: String
           , propertyType :: TypeName
           , propertyReadFunc :: UniformFunc
           , propertyWriteFunc :: Maybe UniformFunc
           , propertyFlags :: CUInt
           }


data ProtoClassMethod =
  PMethod { pMethodName  :: String -- ^ The name of the 'Method'
          , pMethodFunc  :: Name
          }

data Method =
  Method { methodName  :: String -- ^ The name of the 'Method'
         , methodTypes :: [TypeName] -- ^ Gets the 'TypeName's which
                                    -- comprise the signature of a
                                    -- 'Method'.  The head of the list
                                    -- is the return type and the tail
                                    -- the arguments.
         , methodFunc  :: UniformFunc
         }

data ProtoSignal =
  PSignal { pSignalName :: String
          , pSignalArgTypes :: [Name]
          }

data Signal =
  Signal { signalName :: String
         , signalArgTypes :: [TypeName]
         }

data ProtoClassDefinition = ProtoClassDef {
  pClassName :: Name,
  pClassVersion :: (Int, Int),
  pClassURI :: String,
  pClassProperties :: [ProtoClassProperty],
  pClassMethods :: [ProtoClassMethod],
  pClassSignals :: [ProtoSignal],
  pClassConstructor :: Name
  }

data ClassDefinition tt = ClassDef {
  classVersion :: (Int, Int),
  classURI :: String,
  classProperties :: [Property],
  classMethods :: [Method],
  classSignals :: [Signal]
  }

defClass :: ProtoClassDefinition -> Q [Dec]
defClass cd = do
  let clsName = pClassName cd
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
  tdef <- translateDef cd
  let clsDef = ValD (VarP (mkName "classDefinition")) (NormalB tdef) []
      itype = AppT (ConT (mkName "MetaObject")) (ConT clsName)
      instanceDec = InstanceD [] itype [clsDef]

  -- The only other thing we need to do is define functions for each
  -- signal.
  --
  -- > signalName :: tt -> t1 -> t2 -> .. -> IO ()
  sigDefs <- mapM (buildSignal clsName) (zip [0..] (pClassSignals cd))

  return $! instanceDec : concat sigDefs

translateDef :: ProtoClassDefinition -> Q Exp
translateDef pcd = do
  tms <- trMethods (pClassMethods pcd)
  tprops <- trProperties (pClassProperties pcd)
  let (majV, minV) = pClassVersion pcd
      cdName = mkName "ClassDef"
      uriField = (mkName "classURI", LitE (StringL (pClassURI pcd)))
      verField = (mkName "classVersion", TupE [mkIntLit majV, mkIntLit minV])
      sigField = (mkName "classSignals", trSigs (pClassSignals pcd))
      methField = (mkName "classMethods", tms)
      propField = (mkName "classProperties", tprops)

      flds = [uriField, verField, sigField, methField, propField]

  return $! RecConE cdName flds
  where
    mkIntLit :: Int -> Exp
    mkIntLit = LitE . IntegerL . fromIntegral

trProperties :: [ProtoClassProperty] -> Q Exp
trProperties ps = mapM trProp ps >>= (return . ListE)
  where
    trProp (PProperty n g s fs) = do
      -- Look at the type of the getter (since it must exist) to infer
      -- the type of the property.
      VarI _ gt _ _ <- reify g
      let (_, propType) = splitTypes gt
          flag = foldr (.|.) 0 fs
          c1 = AppE (ConE (mkName "Property")) (LitE (StringL n))
          c2 = AppE c1 (typeToTypeNameExp propType)
          c3 = AppE c2 (mkUniformFunc g)
          c4 = AppE c3 (maybeMkUniformFunc s)
          flagEx = AppE (VarE (mkName "fromIntegral")) (LitE (WordPrimL (fromIntegral flag)))
          c5 = AppE c4 flagEx
      return c5
    mkUniformFunc n = AppE (VarE (mkName "marshalFunc0")) (VarE n)
    maybeMkUniformFunc Nothing = ConE (mkName "Nothing")
    maybeMkUniformFunc (Just n) =
      let f = AppE (VarE (mkName "marshalFunc0")) (VarE n)
      in AppE (ConE (mkName "Just")) f



-- | Translate a ProtoMethod to an Exp representing its equivalent
-- Method.
--
-- >
-- > Method { methodName = n, methodFunc = defMethodN mref
-- >        , methodTypes = [ mTypeOf (undefined :: t1), ..] }
trMethods :: [ProtoClassMethod] -> Q Exp
trMethods ms = mapM trMeth ms >>= return . ListE
  where
    trMeth (PMethod qname mref) = do
      -- Figure out the type of the function that the user gave us.
      VarI _ t _ _ <- reify mref
      let (argTypes, rType) = splitTypes t
          c1 = AppE (ConE (mkName "Method")) (LitE (StringL qname))
          -- Use tail on argTypes so that we can drop the this pointer
          -- (which isn't counted in qt method descriptions).
          mtypes = ListE $! map typeToTypeNameExp (rType : tail argTypes)
          c2 = AppE c1 mtypes
          mfunc = mkUniformFunc mref (tail argTypes)
      return $! AppE c2 mfunc
    mkUniformFunc fname ts =
      let defMeth = mkName ("defMethod" ++ show (length ts))
      in AppE (VarE defMeth) (VarE fname)

typeToTypeNameExp :: Type -> Exp
typeToTypeNameExp t =
  let uv = SigE (VarE (mkName "undefined")) t
  in AppE (VarE (mkName "mTypeOf")) uv

-- | Given the Type of a function, split it into the list of argument
-- types and the return type.  The list of argument types is built up
-- in reverse, so reverse it before returning.
splitTypes :: Type -> ([Type], Type)
splitTypes ty =
  let (rt : rest) = split' [] ty
  in (reverse rest, rt)
  where
    -- This case is the innermost part of the type (the return value),
    -- so we are done
    split' acc (AppT ArrowT t) = split' acc t
    split' acc (AppT t t') = split' (t : acc) t'
    split' _ t = error ("Illegal type in method declaration: " ++ pprint t)

-- | Convert a ProtoSignal to an Exp representing a Signal to be
-- spliced into the ClassDefinition
trSigs :: [ProtoSignal] -> Exp
trSigs = ListE . map trSig
  where
    -- | Convert a ProtoSignal descriptor to a Signal descriptor; this
    -- mostly involves translating the named types to TypeNames.
    trSig (PSignal name ts) =
      let c1 = AppE (ConE (mkName "Signal")) (LitE (StringL name))
          tns = ListE (map trType ts)
      in AppE c1 tns
    -- | Take a type name and make an expression of type TypeName:
    --
    -- > mkTypeOf (undefined :: tt)
    --
    -- where @tt@ is the name of the type passed in.
    trType :: Name -> Exp
    trType name =
      let uv = SigE (VarE (mkName "undefined")) (ConT name)
      in AppE (VarE (mkName "mTypeOf")) uv

mkFunType :: [Name] -> Type
mkFunType = foldr addT (AppT ArrowT iot)
  where
    addT t acc = AppT ArrowT $ AppT (ConT t) acc
    iot = AppT (ConT (mkName "IO")) (TupleT 0)

-- | Builds a function to emit a signal.  It is of the form:
--
-- > signalName :: tt -> t1 -> t2 -> .. -> IO ()
-- > signalName self v0 v1 v2 ... =
-- >   allocaBytes sz marshalAndCall
-- >   where
-- >     sz = sum [mSizeOf (undefined :: t1), mSizeOf (undefined :: t2), ..]
-- >     marshalAndCall p0 = do
-- >       marshal p0 v0
-- >       let p1 = plusPtr (mSizeOf (undefined :: t1)) p0
-- >       marshal p1 v1
-- >       let p2 = plusPtr (mSizeOf (undefined :: t2)) p1
-- >       marshal p2 v2
-- >       ..
-- >       hsqmlEmitSignal self signum p0
buildSignal :: Name -> (Int, ProtoSignal) -> Q [Dec]
buildSignal clsName (signum, (PSignal name ts)) = do
  let sigTy = AppT (ConT clsName) $ mkFunType ts
      sig = SigD (mkName name) sigTy

      -- Make a list of variables self v0..vn where self will be the
      -- this ptr
      ixs :: [Int]
      ixs = [0..]
      argVars = take (length ts) $ map (\i -> VarP (mkName ("v" ++ show i))) ixs
      cpatt = VarP (mkName "self") : argVars

  -- The body of the signal needs to
  szName <- newName "sz"
  marshalAndCallName <- newName "marshalAndCall"
  let body0 = AppE (VarE (mkName "allocaBytes")) (VarE szName)
      body1 = AppE body0 (VarE marshalAndCallName)

  let c1 = Clause cpatt (NormalB body1) []
      fdef = FunD (mkName name) [c1]
  return [sig, fdef]

{-

defClass ClassDef {
  className = "GraphNode",
  classVersion = (1,0),
  classURI = "net.nochair",
  classProperties = [
    mkROProperty "nodeName" (readIORef . nodeName) writeNodeName
  ]

defClass "GraphNode" (1,0) "net.nochair" [|parseClass|




|]

-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
{-# LANGUAGE CPP #-}
-- | Facilities for defining new object types which can be marshalled between
-- Haskell and QML.
--
-- To define a new object type that is accessible from QML, create a
-- new data type and make it an instance of the 'MetaObject' class.
-- The 'UserData' associated type is the type of the bundle of data
-- accessible from methods and from which properties are derived.
--
-- Properties and methods are added to the MetaObject through the
-- 'classDef' method.  Note that the type being made into a MetaObject
-- must be an instance of Typeable
--
-- Example:
--
-- > data HSType = HSType { hsTypeContent :: IORef String
-- >                      , hsTypeStatic :: String
-- >                      }
-- >             deriving (Typeable)
-- >
-- > instance MetaObject HSType where
-- >  type UserData HSType = HSType
-- >  classDef = do
-- >    defPropertyRW "hsTypeContent" (readIORef . hsTypeContent) (writeIORef . hsTypeContent)
-- >    defPropertyRO "hsTypeStatic" (return . hsTypeStatic)
-- >    return ()
--
-- This example creates a new type, HSType, with two properties.  The
-- first is a mutable String.  The second is an immutable (read-only)
-- string.  These strings will automatically be converted to QStrings
-- for use in QML.
module Graphics.QML.Types.Classes (
  -- * Classes
  MetaObject (..),
  ClassDefinition(..),
  QPointer,

  -- * Primary Helpers
  defClass,
  registerTypes,

  -- * Context Objects
  allocateContextObject,

  -- * Methods
  defMethod,

  -- * Signals
  defSignal,

  -- * Properties
  defPropertyRO,
  defPropertyRW,
) where

import Graphics.QML.Internal.Core
import Graphics.QML.Internal.Classes
import Graphics.QML.Internal.TH

import Data.Char ( chr )
import Control.Monad
import Control.Monad.Trans.State
import Data.Bits
import Data.List ( foldl' )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe
import Data.IORef
import Data.Typeable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Array
import Language.Haskell.TH ( Name )
#if __GLASGOW_HASKELL__ < 704
import System.FilePath ( takeExtension )
#endif
import System.IO.Unsafe
import Text.Printf

import Debug.Trace
debug = flip trace

--
-- MetaObject
--

-- | This is the default Marshallable instance provided for all
-- MetaObjects.  It should be sufficient for anything...
instance (MetaObject tt) => Marshallable tt where
  marshal ptr obj = do
    (HsQMLObjectHandle hndl) <- hsqmlCreateObject obj $ classData $ (metaClass :: MetaClass tt)
    poke (castPtr ptr) hndl
  unmarshal ptr = hsqmlGetHaskell $ HsQMLObjectHandle $ castPtr ptr
  mSizeOf _ = sizeOf nullPtr
  mTypeOf _ = classType (metaClass :: MetaClass tt)

--
-- DefClass
--

-- | Represents a QML class which wraps the type @tt@.  The
-- HsQMLClassHandle is a wrapper around a C++ object.  TypeName is
-- derived from Typeable.
data MetaClass tt = MetaClass { classType :: TypeName
                              , classData :: HsQMLClassHandle
                              }


{-# NOINLINE metaClassDb #-}
-- | This is a global lookup table mapping keys to MetaClass objects.
-- The Int keys are computed from types using Data.Typeable (allowing
-- information recovery from the existential).
metaClassDb :: forall a. IORef (Map TypeKey (MetaClass a))
metaClassDb = unsafePerformIO $ newIORef Map.empty

{-# NOINLINE metaClass #-}
-- | Creates a new metaclass and adds it to the global 'metaClassDb'.
-- This is not thread safe.  To make it thread safe, switch the global
-- IORef to an MVar.
metaClass :: forall tt. (MetaObject tt) => MetaClass tt
metaClass = unsafePerformIO $ do
  let typ  = typeOf (undefined :: tt)
      def  = classDefinition :: InternalClassDefinition tt
      (major, minor) = _classVersion def
      uri = _classURI def
      ctor = _classConstructor def
  (name, key) <- examineTypeInfo typ
  db  <- readIORef metaClassDb
  case Map.lookup key db of
    Just mClass -> return mClass
    Nothing     -> do
      mClass <- createClass name def
      writeIORef metaClassDb $ Map.insert key mClass db

      let placementFunc place = do
            udata <- ctor place
            spudata <- newStablePtr udata
            let pudata = castStablePtrToPtr spudata
            withHsQMLClassHandle (classData mClass) $ \h -> do
              hsqmlAllocateInPlace place pudata h
      ctorPtr <- marshalPlacementFunc placementFunc

      -- Now register this class with the equivalent of
      -- qmlRegisterType so that instances can be created from QML.
      withHsQMLClassHandle (classData mClass) $ \h -> do
        hsqmlRegisterType ctorPtr uri major minor name h

      return mClass

-- | This is a helper to convert a DefClass (defined in MetaObject
-- instance declarations) into a MetaClass (which will later be stored
-- into the global metaClassDb).
--
-- This function does the work of moc and then registers the resulting
-- type with the QObject runtime system.
createClass :: forall tt. (MetaObject tt)
               => String -- ^ The class name (derived from Typeable usually)
               -> InternalClassDefinition tt
               -> IO (MetaClass tt)
createClass name (InternalClassDef _ _ properties methods signals _ _) = do
  -- This is the moc step; metaData is equivalent to the
  -- qt_meta_data_<TYPE> array that moc produces.  metaStrData is
  -- equivalent to qt_meta_stringdata_<TYPE>.
  let moc@(MOCOutput metaData metaStrData) =
        compileClass name signals methods properties

  -- Convert all of the class description stuff into C-compatible
  -- types (arrays of Storable types).
  metaDataPtr <- newArray metaData `debug` show moc
  metaStrDataPtr <- newArray metaStrData
  methodsPtr <- mapM (marshalFunc . methodFunc) methods >>= newArray
  pReads <- mapM (marshalFunc . propertyReadFunc) properties
  let trWr p = case propertyWriteFunc p of
        Nothing -> return nullFunPtr
        Just w -> marshalFunc w
  pWrites <- mapM trWr properties
  -- The array of accessors and mutators is arranged such that the
  -- read and write functions for a given property are adjacent in the
  -- array, so we do the interleaving step here.
  propertiesPtr <- newArray $ interleave pReads pWrites

  -- Create a QMetaObject wrapper around the class description we just
  -- built.
  hndl <- hsqmlCreateClass metaDataPtr metaStrDataPtr methodsPtr propertiesPtr
  return $ case hndl of
    Just hndl' -> MetaClass (TypeName name) hndl'
    Nothing    -> error "Failed to create QML class."

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : ys `interleave` xs

--
-- Method
--

defMethod :: String -> Name -> ProtoClassMethod
defMethod = PMethod

--
-- Property
--

-- | Defines a named read-only property using an impure
-- accessor function.
defPropertyRO :: String -> Name -> ProtoClassProperty
defPropertyRO name g =
  PProperty { pPropertyName = name
            , pPropertyReadFunc = g
            , pPropertyWriteFunc = Nothing
            , pPropertyFlags = [pfScriptable, pfReadable, pfStored]
            }

-- | Defines a named read-write property using a pair of
-- impure accessor and mutator functions.
--
-- Note: the original code used marshalFunc1 for the property writing
-- function.  This makes sense logically, but the layout of the args
-- array (the pv argument in the function marshallers) for property
-- writes is unusual.  Instead of being [retVal, arg1, arg2, ..], it
-- is just [arg1] since there is never a return value for property
-- writes.  This means that the writer function has to be marshalled
-- with marshalFunc0 here.
defPropertyRW :: String -> Name -> Name -> ProtoClassProperty
defPropertyRW name g s =
  PProperty { pPropertyName = name
            , pPropertyReadFunc = g
            , pPropertyWriteFunc = Just s
            , pPropertyFlags = [pfScriptable, pfReadable, pfWritable, pfStored]
            }

--
-- Marshaling functions
--


--
-- MOC static data construction
--

-- | The construction takes place in this State monad.  The data and
-- stringdata arrays (lists) are built up in *reverse* for efficiency.
-- Be sure to reverse them after extracting them from the State monad.
--
-- The mStrDataMap is used to record the index of each sub-string
-- embedded in the stringdata; if a string is already present, this
-- table caches its index so it doesn't need to be re-inserted.
--
-- The low-level details of the meta-object format are described here:
--
--   http://dev.libqxt.org/libqxt/wiki/Meta_Object_Format
data MOCState =
  MOCState { mData :: [CUInt] -- ^ The (reversed) int-based index table
           , mDataLen :: Int  -- ^ Length of the index table
           , mDataMethodsIdx :: Maybe Int -- ^ Starting index of the methods sub-index
           , mDataPropsIdx :: Maybe Int -- ^ Starting index of the properties sub-index
           , mStrData :: [CChar] -- ^ The (reversed) stringdata
           , mStrDataLen :: Int -- ^ The length of the stringdata
           , mStrDataMap :: Map String CUInt -- ^ The unique map
           }
  deriving (Show)

-- | The result of running MOC is the metadata array and the metadata
-- string (wherein all of the strings describing the class are
-- embedded).
data MOCOutput = MOCOutput [CUInt] [CChar]

instance Show MOCOutput where
  show = showMOC

showMOC :: MOCOutput -> String
showMOC (MOCOutput arr str) =
  showMethods (map fromIntegral arr) strMap 4 5 `debug` show strMap
  where
    ixs :: [Int]
    ixs = [0..]
    -- Split the data string on nulls and record the integer index
    -- where each string starts.
    (_, _, strMap) = foldl' extractStrings (0, "", Map.empty) (zip ixs str)

showMethods :: [Int] -> Map Int String -> Int -> Int -> String
showMethods arr m ixN ixStart =
  concat $ showMethod m ints `debug` show ints
  where
    n = fromIntegral $ arr !! ixN
    s = fromIntegral $ arr !! ixStart
    ints = take (5 * n) $ drop s arr

showMethod :: Map Int String -> [Int] -> [String]
showMethod _ [] = []
showMethod m (sigN : paramsN : typeN : _ : flags : rest) =
  s : showMethod m rest
  where
    s = printf "%s(%s) : %s : %d\n" name params ty flags
    name = m Map.! fromIntegral sigN
    params = m Map.! fromIntegral paramsN
    ty = m Map.! fromIntegral typeN
showMethod _ _ = error "Unexpected method arity"

extractStrings :: (Integral a)
                  => (Int, String, Map Int String)
                  -> (Int, a)
                  -> (Int, String, Map Int String)
extractStrings (start, s, m) (ix, cchar) =
  case cchar == 0 of
    True -> (ix+1, "", Map.insert start (reverse s) m)
    False -> (start, chr (fromIntegral cchar) : s, m)


newMOCState :: MOCState
newMOCState = MOCState [] 0 Nothing Nothing [] 0 Map.empty

-- | Write an Int into the mData table
writeInt :: CUInt -> State MOCState ()
writeInt int = do
  st <- get
  let md = mData st
      mdLen = mDataLen st
  put st { mData = int : md
         , mDataLen = mdLen + 1
         }
  return ()

-- | Write a string into the stringdata section and record its index
-- in the mData table.  If the string already exists, simply look up
-- its cached index.
writeString :: String -> State MOCState ()
writeString str = do
  st <- get
  let msd    = mStrData st
      msdLen = mStrDataLen st
      msdMap = mStrDataMap st
  case (Map.lookup str msdMap) of
    Just idx -> writeInt idx
    Nothing  -> do
      let idx = fromIntegral msdLen
          msd' = 0 : (map castCharToCChar (reverse str) ++ msd)
          msdLen' = msdLen + length str + 1
          msdMap' = Map.insert str idx msdMap
      put st { mStrData = msd'
             , mStrDataLen = msdLen'
             , mStrDataMap = msdMap'
             }
      writeInt idx `debug` printf "String %s @ %d" str ((fromIntegral idx) :: Int)

-- | Append a 'Method' into the methods index in the mData table.
-- Each entry has 5 components: signature, parameters, type, tag,
-- flags.
--
-- This implementation currently makes every Method public.
--
-- FIXME: This seems to be missing the tag - it isn't really clear
-- what the tag is supposed to be, but it should probably be
-- included...
writeMethod :: Method -> State MOCState ()
writeMethod m = do
  idx <- get >>= return . mDataLen
  writeString $ methodSignature m
  writeString $ methodParameters m
  writeString $ typeName $ head $ methodTypes m
  -- This is the tag field; it isn't clear exactly what it is supposed
  -- to be, but making it equal to the type seems to work.
  writeString $ typeName $ head $ methodTypes m
  writeInt (mfAccessPublic .|. mfMethodMethod)
  st <- get
  put st { mDataMethodsIdx = mplus (mDataMethodsIdx st) (Just idx) }
  return ()

-- | Almost the same as writeMethod; the flags are a bit different.
-- They should be refactored into a common function eventually...
writeSignal :: Signal -> State MOCState ()
writeSignal s = do
  idx <- get >>= return . mDataLen
  writeString $ signalSignature s
  writeString $ signalParameters s
  writeString "" -- $ typeName $ head $ signalArgTypes s
  writeString "" -- $ typeName $ head $ signalArgTypes s
  writeInt (mfAccessProtected .|. mfMethodSignal)
  st <- get
  put st { mDataMethodsIdx = mplus (mDataMethodsIdx st) (Just idx) }
  return ()


-- | Write a property into the property table of mData.  Each
-- component has three fields: name, type, flags.
writeProperty :: Property -> State MOCState ()
writeProperty p = do
  idx <- get >>= return . mDataLen
  writeString $ propertyName p
  writeString $ typeName $ propertyType p
  -- The property flags are ORed with something and shifted left 24
  -- bits.  According to the moc source, the value is
  -- qvariant_nameToType; I'm not sure exactly what is going on there
  -- and it doesn't seem to affect functionality.  This could be
  -- changed a bit later... there are some hard-coded values in moc
  -- and otherwise there is some computation by QMetaType::type.
  writeInt (propertyFlags p) -- flags
  st <- get
  put st { mDataPropsIdx = mplus (mDataPropsIdx st) (Just idx) }
  return ()

-- | This does the actual work of moc inside of a State monad.
--
-- FIXME: The format is currently hard-coded as 5 (qt 4.7).  qt 4.8 is
-- at 6.
compileClass :: String -> [Signal] -> [Method] -> [Property] -> MOCOutput
compileClass name ss ms ps =
  let enc = flip execState newMOCState $ do
        writeInt 5                           -- Revision (Qt 4.7)
        writeString name                     -- Class name
        writeInt 0 >> writeInt 0             -- Class info
        writeInt $ fromIntegral $ (length ms + length ss) -- Methods
        writeInt $ fromIntegral $
          fromMaybe 0 $ mDataMethodsIdx enc  -- Methods (data index)
        writeInt $ fromIntegral $ length ps  -- Properties
        writeInt $ fromIntegral $
          fromMaybe 0 $ mDataPropsIdx enc    -- Properties (data index)
        writeInt 0 >> writeInt 0             -- Enums
        writeInt 0 >> writeInt 0             -- Constructors
        writeInt 0                           -- Flags
        writeInt $ fromIntegral (length ss)  -- Signals
        mapM_ writeSignal ss
        mapM_ writeMethod ms
        mapM_ writeProperty ps
        writeInt 0
  in MOCOutput (reverse $ mData enc) (reverse $ mStrData enc)

foldr0 :: (a -> a -> a) -> a -> [a] -> a
foldr0 _ x [] = x
foldr0 f _ xs = foldr1 f xs

-- | Method signatures are the method name followed by a
-- comma-separated list of parameter types enclodes in a set of
-- parens.
methodSignature :: Method -> String
methodSignature method =
  let paramTypes = tail $ methodTypes method
  in (showString (methodName method) . showChar '(' .
       foldr0 (\l r -> l . showChar ',' . r) id
         (map (showString . typeName) paramTypes) . showChar ')') ""

signalSignature :: Signal -> String
signalSignature sig =
  let ts = signalArgTypes sig
  in (showString (signalName sig) . showChar '(' .
       foldr0 (\l r -> l . showChar ',' . r) id
         (map (showString . typeName) ts) . showChar ')') ""

-- | moc stores the method parameter list as a comma-separated (with
-- no spaces) list of the parameter names.
methodParameters :: Method -> String
methodParameters method =
  replicate (flip (-) 2 $ length $ methodTypes method) ','

-- Signals do not have a this parameter
signalParameters :: Signal -> String
signalParameters sig =
  replicate (flip (-) 1 $ length $ signalArgTypes sig) ','

-- | This is a special helper to allocate the Context object for a QML
-- program.  This is necessary because of the C++ wrapper object that
-- needs to be assocated with each Haskell QML object.
--
-- For objects that are instantiated by the QML runtime system, this
-- wrapper is automatically generated.  Since the context object is
-- allocated in Haskell, this helper builds the wrapper.
--
-- To use it, feed it a QML object that is partially instantiated,
-- lacking only the QPointer to its C++ wrapper object.
--
-- > ctx <- allocateContextObject $ HaskellType "foo" "bar"
-- > createEngine $ defaultEngineConfig { contextObject = Just ctx }
-- > runEngines
allocateContextObject :: forall a . (MetaObject a) => (QPointer -> a) -> IO a
allocateContextObject partialObject = do
  -- Note that this function is defined here because it needs access
  -- to the metaClass database, and that should not be exposed at all.
  let mc :: MetaClass a
      mc = metaClass

  cobj <- withHsQMLClassHandle (classData mc) $ \h -> do
    hsqmlAllocateContextObject h
  let hobj = partialObject cobj
  hobjSPtr <- newStablePtr hobj
  hsqmlSetHaskell cobj (castStablePtrToPtr hobjSPtr)
  return hobj

-- TH Helpers


#if __GLASGOW_HASKELL__ >= 704
type TypeKey = TypeRep

examineTypeInfo :: TypeRep -> IO (String, TypeKey)
examineTypeInfo typ = do
  return (tyConName (typeRepTyCon typ), typ)

#else
type TypeKey = Int

examineTypeInfo :: TypeRep -> IO (String, TypeKey)
examineTypeInfo typ = do
  key <- typeRepKey typ
  -- This includes the Module name; we need to drop that to make a
  -- valid QML identifier.  takeExtension leaves the leading ., so
  -- drop it using tail.
  let name = tail $ takeExtension $ tyConString $ typeRepTyCon typ
  return (name, key)
#endif

{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances #-}
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
  MetaObject ( classDef ),
  UserData,
  DefClass,

  -- * Methods
  defMethod0,
  defMethod1,
  defMethod2,
  defMethod3,

  -- * Properties
  defPropertyRO,
  defPropertyRW,
) where

import Graphics.QML.Internal.Core
import Graphics.QML.Internal.Classes

import Control.Monad
import Control.Monad.Trans.State
import Data.Bits
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.IORef
import Data.Typeable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Numeric

import Debug.Trace
debug = flip trace

--
-- MetaObject
--

-- | The class 'MetaObject' allows Haskell types to be accessed as objects
-- from within QML.
--
-- A 'Marshallable' instance is provided automatically for all instances of
-- this class, however, 'defClass' must be used to define an object's class
-- members before marshalling any values.
--
-- FIXME: Provide an alternate version of this based on
-- TemplateHaskell to push the Haskell-side of the metaclass creation
-- to compile time (like moc).
class (Typeable tt) => MetaObject tt where
  -- | The type of user data carried by all objects of this class
  type UserData tt
  -- | The definition of the methods and properties of this MetaObject
  -- (these are the methods and properties visible in QML and to other
  -- QObjects).
  classDef :: DefClass tt ()

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
metaClassDb :: forall a. IORef (IntMap (MetaClass a))
metaClassDb = unsafePerformIO $ newIORef IntMap.empty

{-# NOINLINE metaClass #-}
-- | Creates a new metaclass and adds it to the global 'metaClassDb'.
-- This is not thread safe.  To make it thread safe, switch the global
-- IORef to an MVar.
metaClass :: forall tt. (MetaObject tt) => MetaClass tt
metaClass = unsafePerformIO $ do
  let typ  = typeOf (undefined :: tt)
      name = tyConString $ typeRepTyCon typ
      def  = classDef :: DefClass tt ()
  key <- typeRepKey typ
  db  <- readIORef metaClassDb
  case IntMap.lookup key db of
    Just mClass -> return mClass
    Nothing     -> do
      mClass <- createClass (name ++ showInt key "") def
      writeIORef metaClassDb $ IntMap.insert key mClass db
      return mClass

-- | This is a helper to convert a DefClass (defined in MetaObject
-- instance declarations) into a MetaClass (which will later be stored
-- into the global metaClassDb).
--
-- This function does the work of moc and then registers the resulting
-- type with the QObject runtime system.
createClass :: forall tt. (MetaObject tt)
               => String -- ^ The class name (derived from Typeable usually)
               -> DefClass tt () -- ^ The 'classDef' for this type
               -> IO (MetaClass tt)
createClass name (DefClass methods properties _) = do
  -- This is the moc step; metaData is equivalent to the
  -- qt_meta_data_<TYPE> array that moc produces.  metaStrData is
  -- equivalent to qt_meta_stringdata_<TYPE>.
  let (MOCOutput metaData metaStrData) = compileClass name methods properties

  -- Convert all of the class description stuff into C-compatible
  -- types (arrays of Storable types).
  metaDataPtr <- newArray metaData
  metaStrDataPtr <- newArray metaStrData
  methodsPtr <- mapM (marshalFunc . methodFunc) methods >>= newArray
  pReads <- mapM (marshalFunc . propertyReadFunc) properties
  pWrites <- mapM (fromMaybe (return nullFunPtr) . fmap marshalFunc . propertyWriteFunc) properties
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

-- | This type is used to monadically build up the list of properties
-- and methods.  This should be expanded later to handle Constructors.
-- Maybe Constructors are just Methods with a flag?
data DefClass tt a = DefClass [Method tt] [Property tt] a

instance Monad (DefClass tt) where
  (DefClass ms ps v) >>= f =
    let (DefClass ms' ps' v') = f v in DefClass (ms'++ms) (ps'++ps) v'
  return v = DefClass [] [] v

--
-- Method
--

-- | Represents a named method which can be invoked from QML on an object of
-- type @tt@.
data Method tt =
  Method { methodName  :: String -- ^ The name of the 'Method'
         , methodTypes :: [TypeName] -- ^ Gets the 'TypeName's which
                                    -- comprise the signature of a
                                    -- 'Method'.  The head of the list
                                    -- is the return type and the tail
                                    -- the arguments.
         , methodFunc  :: UniformFunc
         }

-- | Base helper to monadically define a 'Method'
defMethod :: Method tt -> DefClass tt ()
defMethod m = DefClass [m] [] ()

-- FIXME: Replace these with TemplateHaskell

-- | Defines a named method using an impure nullary function.
defMethod0 :: forall tt tr. (MetaObject tt, Marshallable tr)
              => String -- ^ Name of the method
              -> (tt -> IO tr) -- ^ The Haskell function to call.  The
                             -- object is the parameter.
              -> DefClass tt ()
defMethod0 name f =
  defMethod Method { methodName = name
                   , methodTypes = [ mTypeOf (undefined :: tr) ]
                   , methodFunc = marshalFunc0 m
                   }
  where
    m p0 pr = unmarshal p0 >>= f >>= marshalRet pr

-- | Defines a named method using an impure unary function.
defMethod1 :: forall tt t1 tr. (MetaObject tt, Marshallable t1, Marshallable tr)
              => String -- ^ Name of the method
              -> (tt -> t1 -> IO tr) -- ^ The Haskell function to call
              -> DefClass tt ()
defMethod1 name f =
  defMethod Method { methodName = name
                   , methodTypes = [ mTypeOf (undefined :: tr) , mTypeOf (undefined :: t1) ]
                   , methodFunc = marshalFunc1 m
                   }
  where
    m p0 p1 pr = do
      v0 <- unmarshal p0
      v1 <- unmarshal p1
      f v0 v1 >>= marshalRet pr

-- | Defines a named method using an impure binary function.
defMethod2 :: forall tt t1 t2 tr.
  (MetaObject tt, Marshallable t1, Marshallable t2, Marshallable tr)
  => String -- ^ Method name
  -> (tt -> t1 -> t2 -> IO tr) -- ^ Haskell function to call
  -> DefClass tt ()
defMethod2 name f =
  defMethod Method { methodName = name
                   , methodTypes = [ mTypeOf (undefined :: tr)
                                   , mTypeOf (undefined :: t1)
                                   , mTypeOf (undefined :: t2)
                                   ]
                   , methodFunc = marshalFunc2 m
                   }
  where
    m p0 p1 p2 pr = do
      v0 <- unmarshal p0
      v1 <- unmarshal p1
      v2 <- unmarshal p2
      f v0 v1 v2 >>= marshalRet pr

-- | Defines a named method using an impure function taking 3 arguments.
defMethod3 :: forall tt t1 t2 t3 tr.
  (MetaObject tt, Marshallable t1, Marshallable t2, Marshallable t3, Marshallable tr)
  => String
  -> (tt -> t1 -> t2 -> t3 -> IO tr)
  -> DefClass tt ()
defMethod3 name f =
  defMethod Method { methodName = name
                   , methodTypes = [ mTypeOf (undefined :: tr)
                                   , mTypeOf (undefined :: t1)
                                   , mTypeOf (undefined :: t2)
                                   , mTypeOf (undefined :: t3)
                                   ]
                   , methodFunc = marshalFunc3 m
                   }
  where
    m p0 p1 p2 p3 pr = do
      v0 <- unmarshal p0
      v1 <- unmarshal p1
      v2 <- unmarshal p2
      v3 <- unmarshal p3
      f v0 v1 v2 v3 >>= marshalRet pr

--
-- Property
--

-- | Represents a named property which can be accessed from QML on an object
-- of type @tt@.
data Property tt =
  Property { propertyName :: String
           , propertyType :: TypeName
           , propertyReadFunc :: UniformFunc
           , propertyWriteFunc :: Maybe UniformFunc
           , propertyFlags :: [CUInt]
           }

-- | Helper to monadically define properties
defProperty :: Property tt -> DefClass tt ()
defProperty p = DefClass [] [p] ()

-- | Defines a named read-only property using an impure
-- accessor function.
defPropertyRO :: forall tt tr. (MetaObject tt, Marshallable tr)
                 => String -- ^ Property name
                 -> (tt -> IO tr) -- ^ Property accessor
                 -> DefClass tt ()
defPropertyRO name g =
  defProperty Property { propertyName = name
                       , propertyType = mTypeOf (undefined :: tr)
                       , propertyReadFunc = marshalFunc0 accessor
                       , propertyWriteFunc = Nothing
                       , propertyFlags = [pfScriptable, pfReadable, pfStored]
                       }
  where
    accessor p0 pr = unmarshal p0 >>= g >>= marshal pr

-- | Defines a named read-write property using a pair of
-- impure accessor and mutator functions.
defPropertyRW :: forall tt tr. (MetaObject tt, Marshallable tr)
                 => String -- ^ Property name
                 -> (tt -> IO tr) -- ^ Property accessor
                 -> (tt -> tr -> IO ()) -- ^ Property mutator
                 -> DefClass tt ()
defPropertyRW name g s =
  defProperty Property { propertyName = name
                       , propertyType = mTypeOf (undefined :: tr)
                       , propertyReadFunc = marshalFunc0 accessor
                       , propertyWriteFunc = Just (marshalFunc1 writer)
                       , propertyFlags = [pfScriptable, pfReadable, pfWritable, pfStored]
                       }
  where
    accessor p0 pr = unmarshal p0 >>= g >>= marshal pr
    writer p0 p1 _ = do
      v0 <- unmarshal p0
      v1 <- unmarshal p1
      s v0 v1

--
-- Marshaling functions
--

marshalFunc0 :: (Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc0 f p0 pv = do
  pr <- peekElemOff pv 0
  f p0 pr

marshalFunc1 :: (Ptr () -> Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc1 f p0 pv = do
  pr <- peekElemOff pv 0
  p1 <- peekElemOff pv 1
  f p0 p1 pr

marshalFunc2 ::
  (Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc2 f p0 pv = do
  pr <- peekElemOff pv 0
  p1 <- peekElemOff pv 1
  p2 <- peekElemOff pv 2
  f p0 p1 p2 pr

marshalFunc3 ::
  (Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO ()) -> UniformFunc
marshalFunc3 f p0 pv = do
  pr <- peekElemOff pv 0
  p1 <- peekElemOff pv 1
  p2 <- peekElemOff pv 2
  p3 <- peekElemOff pv 3
  f p0 p1 p2 p3 pr

marshalRet :: (Marshallable tt) => Ptr () -> tt -> IO ()
marshalRet ptr obj
  | ptr == nullPtr = return ()
  | otherwise      = marshal ptr obj

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
writeString :: String -> State MOCState CUInt
writeString str = do
  st <- get
  let msd    = mStrData st
      msdLen = mStrDataLen st
      msdMap = mStrDataMap st
  case (Map.lookup str msdMap) of
    Just idx -> writeInt idx >> return idx
    Nothing  -> do
      let idx = fromIntegral msdLen
          msd' = 0 : (map castCharToCChar (reverse str) ++ msd)
          msdLen' = msdLen + length str + 1
          msdMap' = Map.insert str idx msdMap
      put st { mStrData = msd'
             , mStrDataLen = msdLen'
             , mStrDataMap = msdMap'
             }
      writeInt idx
      return idx

-- | Append a 'Method' into the methods index in the mData table.
-- Each entry has 5 components: signature, parameters, type, tag,
-- flags.
--
-- This implementation currently makes every Method public.
--
-- FIXME: This seems to be missing the tag - it isn't really clear
-- what the tag is supposed to be, but it should probably be
-- included...
writeMethod :: Method tt -> State MOCState ()
writeMethod m = do
  idx <- get >>= return . mDataLen
  _ <- writeString $ methodSignature m
  _ <- writeString $ methodParameters m
  _ <- writeString $ typeName $ head $ methodTypes m
  writeInt mfAccessPublic
  st <- get
  put st { mDataMethodsIdx = mplus (mDataMethodsIdx st) (Just idx) }
  return ()

-- | Write a property into the property table of mData.  Each
-- component has three fields: name, type, flags.
writeProperty :: Property tt -> State MOCState ()
writeProperty p = do
  idx <- get >>= return . mDataLen
  _ <- writeString $ propertyName p
  tyIdx <- writeString $ typeName $ propertyType p
  -- The property flags are ORed with something and shifted left 24
  -- bits.  According to the moc source, the value is
  -- qvariant_nameToType; I'm not sure exactly what is going on there
  -- and it doesn't seem to affect functionality.  This could be
  -- changed a bit later... there are some hard-coded values in moc
  -- and otherwise there is some computation by QMetaType::type.
  let flags = foldr (.|.) pfInvalid (propertyFlags p)
  writeInt flags
  st <- get
  put st { mDataPropsIdx = mplus (mDataPropsIdx st) (Just idx) }
  return ()

-- | This does the actual work of moc inside of a State monad.
--
-- FIXME: The format is currently hard-coded as 5 (qt 4.7).  qt 4.8 is
-- at 6.
compileClass :: String -> [Method tt] -> [Property tt] -> MOCOutput
compileClass name ms ps =
  let enc = flip execState newMOCState $ do
        writeInt 5                           -- Revision (Qt 4.7)
        _ <- writeString name                     -- Class name
        writeInt 0 >> writeInt 0             -- Class info
        writeInt $ fromIntegral $ length ms  -- Methods
        writeInt $ fromIntegral $
          fromMaybe 0 $ mDataMethodsIdx enc  -- Methods (data index)
        writeInt $ fromIntegral $ length ps  -- Properties
        writeInt $ fromIntegral $
          fromMaybe 0 $ mDataPropsIdx enc    -- Properties (data index)
        writeInt 0 >> writeInt 0             -- Enums
        writeInt 0 >> writeInt 0             -- Constructors
        writeInt 0                           -- Flags
        writeInt 0                           -- Signals
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
methodSignature :: Method tt -> String
methodSignature method =
  let paramTypes = tail $ methodTypes method
  in (showString (methodName method) . showChar '(' .
       foldr0 (\l r -> l . showChar ',' . r) id
         (map (showString . typeName) paramTypes) . showChar ')') ""

-- | moc stores the method parameter list as a comma-separated (with
-- no spaces) list of the parameter names.
methodParameters :: Method tt -> String
methodParameters method =
  replicate (flip (-) 2 $ length $ methodTypes method) ','

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}
-- | These are the types used everywhere with few dependencies
module Graphics.QML.Internal.Core (
  -- * Types
  MetaObject(..),
  ClassDefinition(..),
  Method(..),
  Property(..),
  Signal(..),
  Marshallable(..),
  TypeName(..),
  InternalClassDefinition(..),
  ProtoClassProperty(..),
  ProtoClassMethod(..),
  ProtoSignal(..),
  PlacementFunc,
  UniformFunc,
  QPointer,

  -- * Functions
  withMarshal
  ) where

import Data.Typeable
import Foreign.C.Types ( CUInt )
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Language.Haskell.TH ( Name )

-- | A wrapper around names of Types.
newtype TypeName = TypeName { typeName :: String }

data ProtoClassProperty =
  PProperty { pPropertyName :: String
            , pPropertyReadFunc :: Name
            , pPropertyWriteFunc :: Maybe Name
            , pPropertyFlags :: [CUInt]
            }

data ProtoClassMethod =
  PMethod { pMethodName  :: String -- ^ The name of the 'Method'
          , pMethodFunc  :: Name
          }

data ProtoSignal =
  PSignal { pSignalName :: String
          , pSignalArgTypes :: [Name]
          }

-- | The class 'Marshallable' allows Haskell types to be used from QML.
class Marshallable a where
  marshal   :: Ptr () -> a -> IO ()
  unmarshal :: Ptr () -> IO a
  mSizeOf   :: a -> Int
  mTypeOf   :: a -> TypeName

withMarshal :: (Marshallable a) => a -> (Ptr b -> IO c) -> IO c
withMarshal m f =
  allocaBytes (mSizeOf m) (\ptr -> marshal (castPtr ptr) m >> f ptr)

type UniformFunc = Ptr () -> Ptr (Ptr ()) -> IO ()
type PlacementFunc = Ptr () -> IO ()

type QPointer = Ptr ()

-- | The class 'MetaObject' allows Haskell types to be accessed as objects
-- from within QML.
--
-- A 'Marshallable' instance is provided automatically for all instances of
-- this class, however, 'defClass' must be used to define an object's class
-- members before marshalling any values.
class (Typeable tt) => MetaObject tt where
  classDefinition :: InternalClassDefinition tt

data Property =
  Property { propertyName :: String
           , propertyType :: TypeName
           , propertyReadFunc :: UniformFunc
           , propertyWriteFunc :: Maybe UniformFunc
           , propertyFlags :: CUInt
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

data Signal =
  Signal { signalName :: String
         , signalArgTypes :: [TypeName]
         }

data ClassDefinition = ClassDef {
  className :: Name,
  classVersion :: (Int, Int),
  classURI :: String,
  classProperties :: [ProtoClassProperty],
  classMethods :: [ProtoClassMethod],
  classConstructor :: Name,
  classSelfAccessor :: Name
  }

data InternalClassDefinition tt = InternalClassDef {
  _classVersion :: (Int, Int),
  _classURI :: String,
  _classProperties :: [Property],
  _classMethods :: [Method],
  _classSignals :: [Signal],
  _classConstructor :: QPointer -> IO tt,
  _classSelfAccessor :: tt -> QPointer
  }

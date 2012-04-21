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

-- | Created by 'defPropertyRO' and 'defPropertyRW'; specifies the
-- properties available in a class.
data ProtoClassProperty =
  PProperty { pPropertyName :: String
            , pPropertyReadFunc :: Name
            , pPropertyWriteFunc :: Maybe Name
            , pPropertyFlags :: [CUInt]
            }

-- | Created by 'defMethod'; specifies the methods available in a
-- class.
data ProtoClassMethod =
  PMethod { pMethodName  :: String -- ^ The name of the 'Method'
          , pMethodFunc  :: Name
          }

-- | The user-visible description of a Signal; see 'defSignal'
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

-- | The internal representation of class Properties.  These are
-- constructed in TH code based on the 'classProperties' provided by
-- the user.
data Property =
  Property { propertyName :: String
           , propertyType :: TypeName
           , propertyReadFunc :: UniformFunc
           , propertyWriteFunc :: Maybe UniformFunc
           , propertyFlags :: CUInt
           }

-- | The internal representation of class Methods.  These are
-- constructed in TH code based on the 'classMethods' provided by the
-- user.
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

-- | The user-visible class definition (fed to TH code to create an
-- 'InternalClassDefinition')
data ClassDefinition = ClassDef {
  className :: Name, -- ^ Name of the class
  classVersion :: (Int, Int), -- ^ Class version (major, minor)
  classURI :: String, -- ^ URI of the class provider
  classProperties :: [ProtoClassProperty], -- ^ Property descriptors (see 'defPropertyRO' and 'defPropertyRW')
  classMethods :: [ProtoClassMethod], -- ^ Method descriptors (see 'defMethod')
  classConstructor :: Name, -- ^ The object constructor
  classSelfAccessor :: Name -- ^ The function to access the underlying C++ object
  }

-- | This is the real definition of a QObject that is generated via TH
-- code (specifically 'defClass').
--
-- The only thing that is not straightforward is the use of QPointer.
-- Each Haskell QML object has a C++ wrapper object that is used by
-- QML: an HsQMLObject.  This would not need to be user visible at all
-- except for the way signals are emitted from Haskell code.
--
-- To emit a signal, the pointer to the underlying C++ object is
-- required.  This means that the user must store it in their Haskell
-- object for later retrieval (i.e., whenever a signal is emitted).
--
-- The accessor for this is '_classSelfAccessor', which users fill in
-- using the 'classSelfAccessor' field of 'ClassDefinition'.  They get
-- this pointer via '_classConstructor', which is called whenever a
-- new Haskell QML object is created.  This gives them a chance to
-- store the pointer in their Haskell object.  The classConstructor
-- should return the fully-constructed Haskell QML object.
data InternalClassDefinition tt = InternalClassDef {
  _classVersion :: (Int, Int),
  _classURI :: String,
  _classProperties :: [Property],
  _classMethods :: [Method],
  _classSignals :: [Signal],
  _classConstructor :: QPointer -> IO tt,
  _classSelfAccessor :: tt -> QPointer
  }

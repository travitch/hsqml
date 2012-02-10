{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
-- | These are the types used everywhere with few dependencies
module Graphics.QML.Internal.Core (
  -- * Types
  Marshallable(..),
  TypeName(..),
  ProtoClassProperty(..),
  ProtoClassMethod(..),
  ProtoSignal(..),

  -- * Functions
  withMarshal
  ) where

import Foreign.C.Types
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

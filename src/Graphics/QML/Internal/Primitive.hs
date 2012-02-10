module Graphics.QML.Internal.Primitive (
  UniformFunc,
  PlacementFunc
  ) where

import Foreign.Ptr ( Ptr )

type UniformFunc = Ptr () -> Ptr (Ptr ()) -> IO ()
type PlacementFunc = Ptr () -> IO ()

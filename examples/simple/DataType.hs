{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module DataType where


import Data.Typeable
import Data.IORef

import Graphics.QML
import Graphics.QML.Types.Classes
import Graphics.QML.Types.Intrinsics

data GraphNode = GraphNode { nodeName :: IORef Int
                           , nodePtr :: QPointer
                           }
               deriving (Typeable)

writeNodeName gn s = do
  writeIORef (nodeName gn) s
readNodeName = readIORef . nodeName

printNodeName :: GraphNode -> IO ()
printNodeName gn = do
  s <- readIORef (nodeName gn)
  putStrLn ("Name: " ++ (show s))

makeGraphNode p = do
  ref <- newIORef 0
  return $! GraphNode ref p

defClass ClassDef {
  className = ''GraphNode,
  classVersion = (1,0),
  classURI = "net.nochair",
  classMethods = [ defMethod "print" 'printNodeName
                 ],
  classProperties = [ defPropertyRW "nodeName" 'readNodeName 'writeNodeName ],
  classConstructor = 'makeGraphNode,
  classSelfAccessor = 'nodePtr
  }
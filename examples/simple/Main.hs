{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, OverloadedStrings #-}
module Main ( main ) where

import Data.Typeable
import Data.IORef
import Data.Text ( Text )
import qualified Data.Text as T

import Graphics.QML

import Network.URI (URI, nullURI, uriPath, uriToString)

import DataType

data HSType = HSType { hsTypeContent :: IORef Text -- String
                     , hsTypeStatic :: Text -- String
                     , hsTypePtr :: QPointer
                     }
            deriving (Typeable)

makeHSType p = do
  ref <- newIORef ""
  return $! HSType ref "" p

hsTypeReadContent = readIORef . hsTypeContent
hsTypeReadStatic = return . hsTypeStatic

defSignal ''HSType "contentChanged" [''Int]
--defSignal ''HSType "everythingOkay" [''String]

printHello :: HSType -> IO ()
printHello _ = putStrLn "hellos"
printInt :: HSType -> Int -> IO Int
printInt obj i = do
  s <- readIORef (hsTypeContent obj)
  putStrLn (T.unpack s ++ ": " ++ show i)
  contentChanged obj (i+1)
  return i

changeHandler :: HSType -> Int -> IO ()
changeHandler _ i = putStrLn ("Received " ++ show i)


defClass ClassDef {
  className = ''HSType,
  classVersion = (1,0),
  classURI = "net.nochair",
  classMethods = [ defMethod "hsTypeHello" 'printHello,
                   defMethod "hsShowInt" 'printInt,
                   defMethod "hsChanged" 'changeHandler
                 ],
  classProperties = [ defPropertyRO "hsTypeContent" 'hsTypeReadContent,
                      defPropertyRO "hsTypeStatic" 'hsTypeReadStatic
                    ],
  classConstructor = 'makeHSType,
  classSelfAccessor = 'hsTypePtr
  }

main :: IO ()
main = do
  $registerTypes

  ref <- newIORef "117"
  ctx <- allocateContextObject $ HSType ref "foobar"
  let cfg = defaultEngineConfig { contextObject = Just ctx
                                , initialURL = nullURI { uriPath = "content/main.qml" }
                                }
  createEngine cfg
  runEngines

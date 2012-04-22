{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.QML.Internal.Intrinsics where

import Data.Text as T
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import System.IO.Unsafe

#include "hsqml.h"

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral


--
-- String
--

{#pointer *HsQMLStringHandle as ^ newtype #}

foreign import ccall "hsqml.h &hsqml_string_size"
  hsqmlStringSizePtr :: Ptr CInt
hsqmlStringSize :: Int
hsqmlStringSize = fromIntegral $ unsafePerformIO $ peek hsqmlStringSizePtr

{#fun unsafe hsqml_init_string as ^
  {id `HsQMLStringHandle'} ->
  `()' #}

{#fun unsafe hsqml_deinit_string as ^
  {id `HsQMLStringHandle'} ->
  `()' #}

hsqmlMarshalString :: Text -> HsQMLStringHandle -> IO ()
hsqmlMarshalString t hndl =
  withCWStringLen (unpack t) $ \(cStr, cStrLen) ->
  hsqmlMarshalString_ cStr (cIntConv cStrLen) hndl >>= \res ->
  return ()

foreign import ccall unsafe "hsqml.h hsqml_marshal_string"
  hsqmlMarshalString_ :: Ptr CWchar -> CInt -> HsQMLStringHandle -> IO ()

{#fun unsafe hsqml_unmarshal_string_maxlen as ^
  {id `HsQMLStringHandle'} ->
  `Int' #}

hsqmlUnmarshalString :: HsQMLStringHandle -> IO Text
hsqmlUnmarshalString hndl = do
  hsqmlUnmarshalStringMaxlen hndl >>= flip allocaArray (\cStr ->
    hsqmlUnmarshalString_ hndl cStr >>= \cStrLen -> do
      s <- peekCWStringLen (cStr, cIntConv cStrLen)
      return (pack s))

foreign import ccall unsafe "hsqml.h hsqml_unmarshal_string"
  hsqmlUnmarshalString_ :: HsQMLStringHandle -> Ptr CWchar -> IO CInt

--
-- URL
--

{#pointer *HsQMLUrlHandle as ^ newtype #}

foreign import ccall "hsqml.h &hsqml_url_size"
  hsqmlUrlSizePtr :: Ptr CInt
hsqmlUrlSize :: Int
hsqmlUrlSize = fromIntegral $ unsafePerformIO $ peek hsqmlUrlSizePtr

{#fun unsafe hsqml_init_url as ^
  {id `HsQMLUrlHandle'} ->
  `()' #}

{#fun unsafe hsqml_deinit_url as ^
  {id `HsQMLUrlHandle'} ->
  `()' #}

{#fun unsafe hsqml_string_to_url as ^
  {id `HsQMLStringHandle',
   id `HsQMLUrlHandle'} ->
  `()' #}

{#fun unsafe hsqml_url_to_string as ^
  {id `HsQMLUrlHandle',
   id `HsQMLStringHandle'} ->
  `()' #}

hsqmlMarshalUrl :: String -> HsQMLUrlHandle -> IO ()
hsqmlMarshalUrl str url =
  allocaBytes hsqmlStringSize $ \hndlPtr -> do
    let hndl = HsQMLStringHandle hndlPtr
    hsqmlInitString hndl
    hsqmlMarshalString (pack str) hndl
    hsqmlStringToUrl hndl url
    hsqmlDeinitString hndl

hsqmlUnmarshalUrl :: HsQMLUrlHandle -> IO String
hsqmlUnmarshalUrl url =
  allocaBytes hsqmlStringSize $ \hndlPtr -> do
    let hndl = HsQMLStringHandle hndlPtr
    hsqmlInitString hndl
    hsqmlUrlToString url hndl
    str <- hsqmlUnmarshalString hndl
    hsqmlDeinitString hndl
    return (unpack str)

{-# LANGUAGE ForeignFunctionInterface #-}

module AGLGUI.Raw
( WVPtr
, JSValPtr
, JSArrPtr
, Callback
, FunPtr
, aglguiInit
, aglguiQuit
, aglguiUpdate
, aglguiApiReg
, aglguiMakeWebView
, aglguiLoadURL
, aglguiTexture
, mkCallback
, freeCallback
, aglguiInjectMouseMove
, aglguiInjectMouseDown
, aglguiInjectMouseUp
, aglguiJSValueIsBool
, aglguiJSValueIsNumber
, aglguiJSValueIsString
, aglguiJSValueIsArray
, aglguiJSValueIsObject
, aglguiJSValueIsNull
, aglguiJSValueToBool
, aglguiJSValueToDouble
, aglguiJSValueToString
, aglguiJSValueStringLength
, aglguiJSArrayLength
, aglguiJSArrayAt
) where

import Data.Word
import Foreign.C
import Foreign.Ptr

type WVPtr = Ptr ()
type JSValPtr = Ptr ()
type JSArrPtr = Ptr ()

type Callback = JSArrPtr -> IO JSValPtr

-- | Callbacks created by mkCallback should be released using freeCallback when no longer required.
foreign import ccall "wrapper"
    mkCallback :: Callback -> IO (FunPtr Callback)

freeCallback :: FunPtr Callback -> IO ()
freeCallback = freeHaskellFunPtr

foreign import ccall "aglguiInit"
    aglguiInit :: CString -> IO ()

foreign import ccall "aglguiQuit"
    aglguiQuit :: IO ()

foreign import ccall "aglguiUpdate"
    aglguiUpdate :: IO ()

foreign import ccall "aglguiApiReg"
    aglguiApiReg :: CString -> FunPtr Callback -> CChar -> IO ()

foreign import ccall "aglguiMakeWebView"
    aglguiMakeWebView :: CInt -> CInt -> IO WVPtr

foreign import ccall "aglguiLoadURL"
    aglguiLoadURL :: WVPtr -> CString -> IO ()

foreign import ccall "aglguiTexture"
    aglguiTexture :: WVPtr -> IO CUInt

foreign import ccall "aglguiInjectMouseMove"
    aglguiInjectMouseMove :: WVPtr -> CInt -> CInt -> IO ()

foreign import ccall "aglguiInjectMouseDown"
    aglguiInjectMouseDown :: WVPtr -> CInt -> IO ()

foreign import ccall "aglguiInjectMouseUp"
    aglguiInjectMouseUp :: WVPtr -> CInt -> IO ()

foreign import ccall "aglguiJSValueIsBool"
    aglguiJSValueIsBool :: JSValPtr -> IO CChar

foreign import ccall "aglguiJSValueIsNumber"
    aglguiJSValueIsNumber :: JSValPtr -> IO CChar

foreign import ccall "aglguiJSValueIsString"
    aglguiJSValueIsString :: JSValPtr -> IO CChar

foreign import ccall "aglguiJSValueIsArray"
    aglguiJSValueIsArray :: JSValPtr -> IO CChar

foreign import ccall "aglguiJSValueIsObject"
    aglguiJSValueIsObject :: JSValPtr -> IO CChar

foreign import ccall "aglguiJSValueIsNull"
    aglguiJSValueIsNull :: JSValPtr -> IO CChar

foreign import ccall "aglguiJSValueToBool"
    aglguiJSValueToBool :: JSValPtr -> IO CChar

foreign import ccall "aglguiJSValueToDouble"
    aglguiJSValueToDouble :: JSValPtr -> IO CDouble

foreign import ccall "aglguiJSValueToString"
    aglguiJSValueToString :: JSValPtr -> IO (Ptr Word16)

foreign import ccall "aglguiJSValueStringLength"
    aglguiJSValueStringLength :: JSValPtr -> IO CUInt

foreign import ccall "aglguiJSArrayLength"
    aglguiJSArrayLength :: JSArrPtr -> IO CUInt

foreign import ccall "aglguiJSArrayAt"
    aglguiJSArrayAt :: JSArrPtr -> CUInt -> IO JSValPtr

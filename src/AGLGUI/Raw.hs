{-# LANGUAGE ForeignFunctionInterface #-}

module AGLGUI.Raw
( WVPtr
, Callback
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
) where

import Foreign.C
import Foreign.Ptr

type WVPtr = Ptr ()

type Callback = Ptr () -> IO (Ptr ())

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

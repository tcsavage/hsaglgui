module AGLGUI
( WebView
, init
, quit
, update
, mkWebView
, loadURL
, webViewTexture
, MouseButton (..)
, injectMouseMove
, injectMouseDown
, injectMouseUp
) where

import Prelude hiding (init)

import Control.Applicative
import Foreign.C

import Graphics.Rendering.OpenGL.GL.Texturing.Objects

import AGLGUI.Raw

newtype WebView = WebView { unwrapWV :: WVPtr }

init :: String -> IO ()
init name = withCString name aglguiInit

quit :: IO ()
quit = aglguiQuit

update :: IO ()
update = aglguiUpdate

mkWebView :: Int -> Int -> IO WebView
mkWebView w h = WebView <$> aglguiMakeWebView (fromIntegral w) (fromIntegral h)

loadURL :: WebView -> String -> IO ()
loadURL wv url = withCString url (aglguiLoadURL (unwrapWV wv))

webViewTexture :: WebView -> IO TextureObject
webViewTexture wv = TextureObject <$> aglguiTexture (unwrapWV wv)

data MouseButton = LeftButton
                 | MiddleButton
                 | RightButton
                 deriving (Enum, Eq, Show)

injectMouseMove :: WebView -> Int -> Int -> IO ()
injectMouseMove wv x y = aglguiInjectMouseMove (unwrapWV wv) (fromIntegral x) (fromIntegral y)

injectMouseDown :: WebView -> MouseButton -> IO ()
injectMouseDown wv = aglguiInjectMouseDown (unwrapWV wv) . toEnum . fromEnum

injectMouseUp :: WebView -> MouseButton -> IO ()
injectMouseUp wv = aglguiInjectMouseUp (unwrapWV wv) . toEnum . fromEnum

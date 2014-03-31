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
, apiReg
) where

import Prelude hiding (init)

import Control.Applicative
import Control.Monad
import Data.Aeson as Aeson
import Data.IORef
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as Vector
import Foreign.C
import Foreign.Marshal.Utils
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Rendering.OpenGL.GL.Texturing.Objects

import AGLGUI.Raw

newtype WebView = WebView { unwrapWV :: WVPtr } deriving (Eq)

callbackMap :: IORef (Map.HashMap String (FunPtr Callback))
callbackMap = unsafePerformIO $ newIORef Map.empty

init :: String -> IO ()
init name = withCString name aglguiInit

quit :: IO ()
quit = do
	callbacks <- Map.elems <$> readIORef callbackMap
	mapM freeCallback callbacks
	aglguiQuit

update :: IO ()
update = aglguiUpdate

apiReg :: String -> (Aeson.Array -> IO Aeson.Value) -> IO ()
apiReg name callback = do
	-- Create C callback, marshalling arguments and return value to/from Aeson types.
	cbf <- mkCallback $ \argsPtr -> do
		-- Temporary: ignore arguments, always return NULL pointer.
		r <- callback Vector.empty
		pure nullPtr
	withCString name $ \cname -> aglguiApiReg cname cbf (fromBool False)
	modifyIORef callbackMap (Map.insert name cbf)

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

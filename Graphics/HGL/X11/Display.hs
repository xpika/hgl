-- #hide
module Graphics.HGL.X11.Display
	( getDisplayName
	, openDisplay
	, closeDisplay
	, getDisplay
	) where

import Graphics.HGL.Internals.Utilities (modMVar)

import qualified Graphics.X11.Xlib as X

import Control.Concurrent.MVar (MVar, newMVar, readMVar, takeMVar, putMVar)
import Control.Monad (when)
import Data.Maybe (isJust)
import System.Environment (getEnv)
<<<<<<< HEAD
import System.IO.Error (try)
=======
import System.IO.Error (tryIOError,catchIOError)
>>>>>>> danr/master
import System.IO.Unsafe (unsafePerformIO)

getDisplayName :: IO String
getDisplayName = do
<<<<<<< HEAD
  disp <- try (getEnv "DISPLAY")
=======
  disp <- tryIOError (getEnv "DISPLAY")
>>>>>>> danr/master
  return (either (const ":0.0") id disp)

displayRef :: MVar (Maybe X.Display)
displayRef = unsafePerformIO (newMVar Nothing)

openDisplay  :: String -> IO () -> IO X.Display
openDisplay host cleanup = do
  mb_display <- readMVar displayRef
  when (isJust mb_display) cleanup
  openDisplay'
 where
<<<<<<< HEAD
  openDisplay' = do      
    display <- X.openDisplay host `catch` \ err -> 
=======
  openDisplay' = do
    display <- X.openDisplay host `catchIOError` \ err ->
>>>>>>> danr/master
                 ioError (userError ("Unable to open X display " ++ host))
    modMVar displayRef (const $ Just display)
    return display

closeDisplay :: IO ()
closeDisplay = do
  mb_display <- takeMVar displayRef
  case mb_display of
    Nothing      -> do
      putMVar displayRef Nothing
    Just display -> do
      X.closeDisplay display
      putMVar displayRef Nothing

getDisplay   :: IO X.Display
getDisplay = do
  mb_display <- readMVar displayRef
  case mb_display of
    Nothing      -> ioError $ userError "Display not opened yet"
    Just display -> return display

-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.HGL.Internals.Events
-- Copyright   :  (c) Alastair Reid, 1999-2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (requires concurrency)
--
-- A simple graphics library.
--
-----------------------------------------------------------------------------

-- #hide
module Graphics.HGL.Internals.Events(
	Events, newEvents, getEvent, sendEvent, isNoEvent,
        getTick, sendTick
	) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan(TChan, newTChanIO, readTChan, writeTChan, isEmptyTChan)

import Graphics.HGL.Internals.Event
import Graphics.HGL.Internals.Flag

----------------------------------------------------------------
-- Interface
----------------------------------------------------------------

-- Events are more or less just a channel (~list) of events
--
-- The only subtlety is that ticks are not part of the channel:
-- they're a separate "flag" so that ticks don't accumulate in the 
-- queue (if you process them too fast) and so that ticks can 
-- "overtake" other events.
-- (Win32 timers do the same thing.  I was rather surprised to find
-- myself reimplementing this in Haskell (even in the Win32 version
-- of the Graphics library).  Exposure events in X11 behave in a
-- similar way except that they do not overtake other events.)

type Chan = TChan

data Events = Events { events :: Chan Event
                     , tick   :: Flag ()
                     }

newEvents :: IO Events
getEvent  :: Events -> IO Event
isNoEvent :: Events -> IO Bool
sendEvent :: Events -> Event -> IO ()
sendTick  :: Events -> IO ()
getTick   :: Events -> IO ()

----------------------------------------------------------------
-- Implementation
----------------------------------------------------------------

newEvents = do
  events <- newChan 
  tick   <- newFlag
  return (Events { events=events, tick=tick })

getEvent  evs = readChan    (events evs)
isNoEvent evs = isEmptyChan (events evs)
sendEvent evs = writeChan   (events evs)
sendTick  evs = setFlag     (tick evs) ()
getTick   evs = resetFlag   (tick evs)

----------------------------------------------------------------
-- TChan wrappers
--
-- As `isEmptyChan` was removed by
-- http://hackage.haskell.org/trac/ghc/ticket/4154, we can follow
-- suggestions in there and just replace Chan with TChan.
----------------------------------------------------------------
newChan :: IO (Chan a)
newChan = newTChanIO

readChan :: Chan a -> IO a
readChan = atomically . readTChan

writeChan :: Chan a -> a -> IO ()
writeChan c v = atomically $ writeTChan c v

isEmptyChan :: Chan a -> IO Bool
isEmptyChan = atomically . isEmptyTChan

----------------------------------------------------------------
-- End
----------------------------------------------------------------

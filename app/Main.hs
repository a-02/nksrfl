{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Colog.Core.Action
import Colog.Core.IO

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict

import Data.Bifunctor
import Data.Bitraversable
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.These
import Data.Time

import GHC.Conc

import Graphics.Vty as Vty

-- import Graphics.Vty.Image as Vty

import Sound.Osc.Packet
import Sound.Osc.Transport.Fd.Tcp

import System.IO
import System.Directory

import Command
import Pattern
import Types

{-
   Overview:
   NKSRFL is a tool to control 2 Renoise applications simultaneously.
   It translates keyboard inputs into OSC messages sent to each Renoise client.
   This is done over port 6066.
   Status of both Renoise clients (referred to as "decks") is kept track of by
   repeatedly pinging Renoise over port 8088, and using an external Renoise plugin
   to translate this into data out of Renoise.
-}

-- initscript: 
-- ask for ip addresses to connect to
initScript :: IO (String, String)
initScript = do
  putStrLn "deck 1 ip"
  one <- getLine
  putStrLn "deck 2 ip"
  two <- getLine
  return (one, two)

-- main:
-- set up logging handles, spawn threads, then run the display
main :: IO ()
main = do
  time <- getCurrentTime
  let now = formatTime defaultTimeLocale "%s" time
      filename = "log/nksrfl_" ++ now
      logMain = filename ++ "/main"
      logFlowers = filename ++ "/flowers"
      logGifts = filename ++ "/gifts"
  createDirectoryIfMissing True filename
  mainHandle <- openFile logMain WriteMode
  flowersHandle <- openFile logFlowers WriteMode
  giftsHandle <- openFile logGifts WriteMode
  mapM_ (`hSetBuffering` NoBuffering) [mainHandle, flowersHandle, giftsHandle] 
  (one, two) <- initScript
  logStringHandle mainHandle <& "initscript ran"
  cfg <- standardIOConfig
  vty <- mkVty cfg
  deck1 <- openTcp one rnsServerPort -- unduplicate all this?
  deck2 <- openTcp two rnsServerPort
  tool1 <- openTcp one nksServerPort
  tool2 <- openTcp two nksServerPort
  let sendFlowers =
        race_ 
          (flowers flowersHandle tool1) 
          (flowers flowersHandle tool2)
      deckstate1 = dsEmpty{socket = deck1}
      deckstate2 = dsEmpty{socket = deck2}
  dsTVar1 <- newTVarIO deckstate1
  dsTVar2 <- newTVarIO deckstate2
  let receiveGifts = gifts dsTVar1 dsTVar2 tool1 tool2 giftsHandle
      program = execRWST (vtyGO False) (vty, mainHandle) (dsTVar1, dsTVar2)
  _ <- withAsync (concurrently sendFlowers receiveGifts) (const program)
  shutdown vty

-- flowers:
-- send an OSC message of "/flowers" to a renoise client listening at "conn"
flowers :: Handle -> Tcp -> IO ()
flowers handle conn = forever $ do
  logStringHandle handle <& "sending flowers"
  liftIO $ tcp_send_packet conn (Packet_Message (message "/flowers" []))
  threadDelay 50000 -- 1/20 of a second

gifts ::
  TVar DeckState ->
  TVar DeckState ->
  Tcp ->
  Tcp ->
  Handle ->
  IO ()
gifts ds1 ds2 tcp1 tcp2 handle = forever $ do
  logStringHandle handle <& "gettin me packets hopefully?"
  res1 <- tcp_recv_packet tcp1
  res2 <- tcp_recv_packet tcp2
  logStringHandle handle <& ("got: " ++ show res1 ++ show res2)
  let ts1 = packetToTrackStatus res1
      ts2 = packetToTrackStatus res2
  logStringHandle handle <& ("now: " ++ show ts1 ++ show ts2)
  logStringHandle handle <& "updating tvars"
  atomically $ do
    oldState1 <- readTVar ds1
    oldState2 <- readTVar ds2
    writeTVar ds1 (updateDeckState ts1 oldState1)
    writeTVar ds2 (updateDeckState ts2 oldState2)
  logStringHandle handle <& "apparently done updating tvars"

vtyGO :: Bool -> App ()
vtyGO shouldWeExit = do
  handle <- snd <$> ask
  logStringHandle handle <& "calling updateDisplay"
  updateDisplay
  unless shouldWeExit $ handleEvent >>= vtyGO

updateDisplay :: App ()
updateDisplay = do
  (vty, handle) <- ask
  logStringHandle handle <& "finding display region"
  displayRegion <- liftIO $ (standardIOConfig >>= outputForConfig) >>= displayBounds -- wow!
  logStringHandle handle <& ("display region is " ++ show displayRegion)
  tvar <- get
  (ds1, ds2) <- liftIO $ bitraverse readTVarIO readTVarIO tvar -- woah!
  logStringHandle handle <& "generating picture, running update"
  logStringHandle handle <& show ds1
  logStringHandle handle <& show ds2
  let img1 = string (defAttr `withForeColor` magenta) (show ds1)
      img2 = string (defAttr `withForeColor` cyan) (show ds2)
      pic = picForImage $ img1 <-> img2
  liftIO $ update vty pic

updateDeckState :: TrackStatus -> DeckState -> DeckState
updateDeckState ts ds =
  ds
    { bpm = Just $ trackBpm ts
    , playing = readBool $ trackPlaying ts
    , looping = readBool $ trackLooping ts
    , blockLooping = readBool $ trackBlockLooping ts
    , pattern = fst parsed
    , row = snd parsed
    }
 where
  parsed = patternParse $ trackPosition ts

handleEvent :: App Bool
handleEvent = do
  (vty, handle) <- ask
  tvar <- get
  ds <- liftIO $ bitraverse readTVarIO readTVarIO tvar -- woah!
  let (d1, d2) = bimap socket socket ds
  logStringHandle handle <& "waiting for next event"
  ev <- liftIO $ nextEvent vty
  case ev of
    k -> do
      runKeyCommand ds $ case k of
        -- deck 1
        EvKey (KChar 'q') [] -> Start (This d1)
        EvKey (KChar 'w') [] -> Stop (This d1)
        EvKey (KChar 'e') [] -> Loop (This d1)
        EvKey (KChar 'r') [] -> Load (This d1)
        EvKey (KChar 't') [] -> Queue (This d1) 1 -- add this to deckstate!
        EvKey (KChar 'a') [] -> BlockLoop (This d1)
        EvKey (KChar 's') [] -> BlockLoopSize (This d1) (blockSize $ fst ds)
        EvKey (KChar 'd') [] -> BPM (This d1) 128 -- add "queued BPM" to deck size
        EvKey (KChar 'f') [] -> Faster (This d1) 10
        EvKey (KChar 'g') [] -> Slower (This d1) 10
        EvKey (KChar 'z') [] -> Transpose (This d1) 1
        EvKey (KChar 'x') [] -> None
        EvKey (KChar 'c') [] -> None
        EvKey (KChar 'v') [] -> None
        EvKey (KChar 'b') [] -> None
        -- deck 2
        EvKey (KChar 'y') [] -> Start (That d2)
        EvKey (KChar 'u') [] -> Stop (That d2)
        EvKey (KChar 'i') [] -> Loop (That d2)
        EvKey (KChar 'o') [] -> Load (That d2)
        EvKey (KChar 'p') [] -> Queue (That d2) 1 -- add this to deckstate!
        EvKey (KChar 'h') [] -> BlockLoop (That d2)
        EvKey (KChar 'j') [] -> BlockLoopSize (That d2) (blockSize $ fst ds)
        EvKey (KChar 'k') [] -> BPM (That d2) 128 -- add "queued BPM" to deck size
        EvKey (KChar 'l') [] -> Faster (That d2) 10
        EvKey (KChar ';') [] -> Slower (That d2) 10
        EvKey (KChar 'n') [] -> Transpose (That d2) 1
        EvKey (KChar 'm') [] -> None
        EvKey (KChar ',') [] -> None
        EvKey (KChar '.') [] -> None
        EvKey (KChar '/') [] -> None
        -- both decks
        EvKey (KChar 'Q') [] -> Start (These d1 d2)
        EvKey (KChar 'W') [] -> Stop (These d1 d2)
        EvKey (KChar 'E') [] -> Loop (These d1 d2)
        EvKey (KChar 'R') [] -> Load (These d2 d2)
        EvKey (KChar 'T') [] -> Queue (These d1 d2) 1 -- add this to deckstate!
        EvKey (KChar 'A') [] -> BlockLoop (These d1 d2)
        EvKey (KChar 'S') [] -> BlockLoopSize (These d1 d2) (blockSize $ fst ds)
        EvKey (KChar 'D') [] -> BPM (These d1 d2) 128 -- add "queued BPM" to deck size
        EvKey (KChar 'F') [] -> Faster (These d1 d2) 10
        EvKey (KChar 'G') [] -> Slower (These d1 d2) 10
        EvKey (KChar 'Z') [] -> Transpose (These d1 d2) 1
        EvKey (KChar 'X') [] -> None
        EvKey (KChar 'C') [] -> None
        EvKey (KChar 'V') [] -> None
        EvKey (KChar 'B') [] -> None
        _ -> None
      return $ k == EvKey KEsc []

runKeyCommand :: (DeckState, DeckState) -> KeyCommand -> App ()
runKeyCommand (d1, d2) kc = do
  (vty, handle) <- ask
  logStringHandle handle <& show kc
  case kc of
    None -> return ()
    Start ds -> mergeTheseWith start start (*>) ds
    Stop ds -> mergeTheseWith stop stop (*>) ds
    Loop ds -> do
      mergeTheseWith (loop $ not d1.looping) (loop $ not d2.looping) (*>) ds
    Load ds -> do
      result <- liftIO $ execRWST (vtyLoadFile False) vty Nothing -- i think this is a mispattern?
      let fp = fmap toList . fst $ result
      mergeTheseWith (load fp) (load fp) (*>) ds
    Queue ds i -> mergeTheseWith (queue i) (queue i) (*>) ds
    BlockLoop ds -> do
      mergeTheseWith (blockLoop $ not d1.blockLooping) (blockLoop $ not d2.blockLooping) (*>) ds
    BlockLoopSize ds bs -> do
      mergeTheseWith (blockLoopSize bs) (blockLoopSize bs) (*>) ds
    BPM ds i -> mergeTheseWith (setBPM i) (setBPM i) (*>) ds
    Faster ds i -> do
      mergeTheseWith (setBPM $ maybe 0.0 (+ i) d1.bpm) (setBPM $ maybe 0.0 (+ i) d2.bpm) (*>) ds
    Slower ds i -> do
      mergeTheseWith
        (setBPM $ maybe 0 (\x -> x - i) d1.bpm)
        (setBPM $ maybe 0 (\x -> x - i) d2.bpm)
        (*>)
        ds
    Transpose ds i -> do
      mergeTheseWith
        (setTranspose $ d1.transpose + i)
        (setTranspose $ d2.transpose + i)
        (*>)
        ds

vtyLoadFile :: Bool -> RWST Vty () (Maybe (Seq.Seq Char)) IO ()
vtyLoadFile shouldExit = do
  unless shouldExit $ handleBreakoutEvent >>= vtyLoadFile

handleBreakoutEvent :: RWST Vty () (Maybe (Seq.Seq Char)) IO Bool
handleBreakoutEvent = do
  let seqInit = Seq.reverse . Seq.drop 1 . Seq.reverse
  put $ Just Seq.empty
  vty <- ask
  ev <- liftIO $ nextEvent vty
  case ev of
    EvKey (KChar ch) [] -> modify (\x -> (Seq.|> ch) <$> x) >> return False
    EvKey KBS [] -> modify (fmap seqInit) >> return False
    EvKey KEnter [] -> return True
    EvKey KEsc [] -> put Nothing >> return True
    _ -> return False

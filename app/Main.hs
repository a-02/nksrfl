{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Monad.Trans.RWS.Lazy
import Control.Monad
import Control.Monad.IO.Class

import Data.Bifunctor
import Data.Foldable (toList)
import Data.IORef
import Data.These
import qualified Data.Sequence as Seq

import Graphics.Vty

import Sound.Osc.Datum
import Sound.Osc.Packet
import Sound.Osc.Transport.Fd.Udp
import Sound.Osc.Transport.Fd as Fd

type App = RWST Vty () (DeckState,DeckState) IO

data DeckState = DeckState {
  socket :: Udp,
  buffer :: IORef String,
  bpm :: Maybe Int,
  pattern :: Maybe Int,
  row :: Maybe Int,
  playing :: Bool,
  looping :: Bool,

  blockLooping :: Bool,
  blockSize :: Maybe BlockSize,
  transpose :: Int,
  songName :: String
  }

dsEmpty :: DeckState
dsEmpty = DeckState {
    socket = undefined,
    buffer = undefined,
    bpm = Nothing,
    pattern = Nothing,
    row = Nothing,
    playing = False,
    looping = False,
    blockLooping = False,
    blockSize = Nothing,
    transpose = 0,
    songName = ""
  }


data BlockSize = Half | Quarter | Eighth | Sixteenth

unBlockSize :: BlockSize -> Int
unBlockSize = \case 
  Half -> 2
  Quarter -> 4
  Eighth -> 8
  Sixteenth -> 16

{--
oscBuffer :: IORef String -> Udp -> IO ()
oscBuffer a t = forever $ do
  pckt <- Fd.waitAddress t "/nks/all"
  let unpackedMessage = case pckt of
    Packet_Message msg -> messageDatum msg
    Packet_Bundle bundle -> undefined
  atomicWriteIORef a unpackedMessage
  return ()
--}

initScript :: IO (String, String)
initScript = do
  putStrLn "deck 1 ip"
  one <- getLine
  putStrLn "deck 2 ip"
  two <- getLine
  return (one, two)

rnsServerPort :: Int 
rnsServerPort = 60606
nksServerPort :: Int
nksServerPort = 60608
nksClientPort :: Int
nksClientPort = 60609

main :: IO ()
main = do
  (one, two) <- initScript
  cfg <- standardIOConfig
  vty <- mkVty cfg
  deck1 <- openUdp one rnsServerPort -- these will be asked for on startup later
  deck2 <- openUdp two rnsServerPort
  buffer1 <- newIORef ""
  buffer2 <- newIORef ""
  _ <- execRWST (vtyGO False) vty (dsEmpty { socket = deck1, buffer = buffer1 }, dsEmpty { socket = deck2, buffer = buffer2 })
  shutdown vty

vtyGO :: Bool -> App ()
vtyGO shouldWeExit = do
  updateDisplay
  unless shouldWeExit $ handleEvent >>= vtyGO

updateDisplay :: App ()
updateDisplay = undefined

handleEvent :: App Bool
handleEvent = do
  vty <- ask
  ds <- get
  let (d1, d2) = bimap socket socket ds
  ev <- liftIO $ nextEventNonblocking vty 
  case ev of
    Nothing -> do -- poll for osc
      return True
    Just k -> do
      runKeyCommand $ case k of
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
        EvKey (KChar 'R') [] -> Load (These d1 d2)
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

type DeckSockets = These Udp Udp

data KeyCommand = 
  Start DeckSockets |
  Stop DeckSockets |
  Loop DeckSockets |
  Load DeckSockets |
  Queue DeckSockets Int |
  BlockLoop DeckSockets |
  BlockLoopSize DeckSockets (Maybe BlockSize) |
  BPM DeckSockets Int |
  Faster DeckSockets Int |
  Slower DeckSockets Int |
  Transpose DeckSockets Int |
  None

runKeyCommand :: KeyCommand -> App ()
runKeyCommand = \case
  Start ds -> mergeTheseWith start start (*>) ds
  Stop ds -> mergeTheseWith stop stop (*>) ds
  Loop ds -> do
    (d1, d2) <- get
    mergeTheseWith (loop $ not d1.looping) (loop $ not d2.looping) (*>) ds
  Load ds -> do
    vty <- ask
    result <- liftIO $ execRWST (vtyLoadFile False) vty Nothing -- i think this is a mispattern?
    let fp = fmap toList . fst $ result
    mergeTheseWith (load fp) (load fp) (*>) ds
  Queue ds i -> mergeTheseWith (queue i) (queue i) (*>) ds
  BlockLoop ds -> do
    (d1, d2) <- get
    mergeTheseWith (blockLoop $ not d1.blockLooping) (blockLoop $ not d2.blockLooping) (*>) ds
  BlockLoopSize ds bs -> do
    mergeTheseWith (blockLoopSize bs) (blockLoopSize bs) (*>) ds
  BPM ds i -> mergeTheseWith (setBPM i) (setBPM i) (*>) ds
  Faster ds i -> do
    (d1, d2) <- get
    mergeTheseWith (setBPM $ maybe 0 (+ i) d1.bpm) (setBPM $ maybe 0 (+ i) d2.bpm) (*>) ds
  Slower ds i -> do
    (d1, d2) <- get
    mergeTheseWith 
     (setBPM $ maybe 0 (\x -> x - i) d1.bpm) 
     (setBPM $ maybe 0 (\x -> x - i) d2.bpm) 
     (*>) ds
  Transpose ds i -> do
    (d1, d2) <- get
    mergeTheseWith
      (setTranspose $ d1.transpose + i)
      (setTranspose $ d2.transpose + i)
      (*>) ds
  None -> return ()

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


start :: Udp -> App ()
start conn = liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/transport/start" []))

stop :: Udp -> App ()
stop conn = liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/transport/stop" []))

loop :: Bool -> Udp -> App ()
loop b conn = liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/transport/loop/pattern" [oscBool b]))

load :: Maybe FilePath -> Udp -> App ()
load maybeFp conn = case maybeFp of -- easier to read this way
  Nothing -> return ()
  Just fp -> do
    let loadMessage x = "renoise.app():load_song(" ++ x ++ ")"
        saveMessage = "renoise.app():save_song_as(/dev/null)"
    liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString $ ascii saveMessage])) -- this wont work on windows!
    liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString . ascii $ loadMessage fp]))

queue :: Int -> Udp -> App ()
queue i conn = do
  let queueMessage = "renoise.song().transport:add_scheduled_sequence(" ++ show i ++ ")"
  liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString $ ascii queueMessage]))

setBPM :: Int -> Udp -> App ()
setBPM i conn = liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/song/bpm" [Int32 $ fromIntegral i]))

setTranspose :: Int -> Udp -> App ()
setTranspose i conn = do
  let transposeMessage = "renoise.song().instruments[].transpose = " ++ show i
  liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString $ ascii transposeMessage]))

blockLoop :: Bool -> Udp -> App ()
blockLoop b conn = liftIO $ udp_send_packet conn (Packet_Message (message "renoise/transport/loop/block" [oscBool b]))

blockLoopSize :: Maybe BlockSize -> Udp -> App ()
blockLoopSize bs conn = do
  let size = maybe 8 unBlockSize bs -- Default to 1/8 if blocksize hasn't been set by anything.
      bsMessage = "renoise.song().transport.loop_block_range_coeff = " ++ show size
  liftIO $ udp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString $ ascii bsMessage]))

-- blockSize

oscBool :: Bool -> Datum
oscBool True = AsciiString $ ascii "true"
oscBool False = AsciiString $ ascii "false"

{-
main :: IO ()
main = do
  let server = openUdp "10.0.0.197" 2345 -- snake case? in haskell? who wrote this library?
  Fd.withTransport server (\fd -> Fd.sendMessage fd (message "/renoise/evaluate" [AsciiString "1 + 1"]))
  return ()
-}

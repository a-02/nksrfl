module Command where

import Types
import Sound.Osc
import Pattern

start :: Tcp -> App ()
start conn = liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/transport/start" []))

stop :: Tcp -> App ()
stop conn = liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/transport/stop" []))

loop :: Bool -> Tcp -> App ()
loop b conn = liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/transport/loop/pattern" [oscBool b]))

load :: Maybe FilePath -> Tcp -> App ()
load maybeFp conn = case maybeFp of -- easier to read this way
  Nothing -> return ()
  Just fp -> do
    let loadMessage x = "renoise.app():load_song(" ++ x ++ ")"
        saveMessage = "renoise.app():save_song_as(/dev/null)"
    liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString $ ascii saveMessage])) -- this wont work on windows!
    liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString . ascii $ loadMessage fp]))

queue :: Int -> Tcp -> App ()
queue i conn = do
  let queueMessage = "renoise.song().transport:add_scheduled_sequence(" ++ show i ++ ")"
  liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString $ ascii queueMessage]))

setBPM :: Float -> Tcp -> App ()
setBPM i conn = liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/song/bpm" [Float i]))

setTranspose :: Int -> Tcp -> App ()
setTranspose i conn = do
  let transposeMessage = "renoise.song().instruments[].transpose = " ++ show i
  liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString $ ascii transposeMessage]))

blockLoop :: Bool -> Tcp -> App ()
blockLoop b conn = liftIO $ tcp_send_packet conn (Packet_Message (message "renoise/transport/loop/block" [oscBool b]))

blockLoopSize :: Maybe BlockSize -> Tcp -> App ()
blockLoopSize bs conn = do
  let size = maybe 8 unBlockSize bs -- Default to 1/8 if blocksize hasn't been set by anything.
      bsMessage = "renoise.song().transport.loop_block_range_coeff = " ++ show size
  liftIO $ tcp_send_packet conn (Packet_Message (message "/renoise/evaluate" [AsciiString $ ascii bsMessage]))

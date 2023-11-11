module Pattern where

import Data.Bifunctor
import Data.Maybe

import Sound.Osc

import Types

packetToTrackStatus :: Packet -> TrackStatus
packetToTrackStatus packet = messageToTrackStatus $ 
  fromMaybe (error "bundle") (packet_to_message packet)

messageToTrackStatus :: Message -> TrackStatus
messageToTrackStatus msg = 
  let datum = messageDatum msg
      tsbpm = datumToFloat $ head datum
      tsply = datumToString $ datum !! 1
      tspos = datumToString $ datum !! 2
      tslpn = datumToString $ datum !! 3
      tsblk = datumToString $ datum !! 4
      datumToFloat :: Datum -> Float
      datumToFloat (Float f) = f
      datumToFloat _ = 0
      datumToString :: Datum -> String
      datumToString (AsciiString a) = ascii_to_string a
      datumToString _ = ""
   in TrackStatus tsbpm tsply tspos tslpn tsblk

patternParse :: String -> (Int,Int)
patternParse pos =
  let (xs,ys) = span (== ',') pos
      zs = drop 2 ys
   in bimap read read (xs,zs)

readBool :: String -> Bool
readBool "true" = True
readBool _ = False

oscBool :: Bool -> Datum
oscBool True = AsciiString $ ascii "true"
oscBool False = AsciiString $ ascii "false"


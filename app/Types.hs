{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Types where

import Control.Monad.Trans.RWS.Lazy

import Data.These

import Graphics.Vty

import GHC.Conc

import Sound.Osc

type App = RWST Vty () (TVar DeckState, TVar DeckState) IO

data DeckState = DeckState {
  socket :: Udp, -- sending OSC over 6066
  bpm :: Maybe Float,
  pattern :: Int,
  row :: Int,
  playing :: Bool,
  looping :: Bool,
  blockLooping :: Bool,
  blockSize :: Maybe BlockSize,
  transpose :: Int,
  songName :: String
  }

instance Show DeckState where
  show ds = unlines
    [ show ds.bpm
    , show ds.pattern
    , show ds.row
    , show ds.playing
    , show ds.looping
    , show ds.blockLooping
    , show ds.blockSize
    , show ds.transpose
    , show ds.songName
    ]
    

dsEmpty :: DeckState
dsEmpty = DeckState {
    socket = undefined,
    bpm = Nothing,
    pattern = 0,
    row = 0,
    playing = False,
    looping = False,
    blockLooping = False,
    blockSize = Nothing,
    transpose = 0,
    songName = ""
  }

data TrackStatus = TrackStatus {
  trackBpm :: Float,
  trackPlaying :: String,
  trackPosition :: String,
  trackLooping :: String,
  trackBlockLooping :: String
} deriving Show

tsEmpty :: TrackStatus
tsEmpty = TrackStatus 0 "" "" "" ""

data BlockSize = Half | Quarter | Eighth | Sixteenth deriving Show

unBlockSize :: BlockSize -> Int
unBlockSize = \case 
  Half -> 2
  Quarter -> 4
  Eighth -> 8
  Sixteenth -> 16

rnsServerPort :: Int 
rnsServerPort = 6066
nksServerPort :: Int
nksServerPort = 8088
nksClientPort :: Int
nksClientPort = 9099

type DeckSockets = These Udp Udp

data KeyCommand = 
  Start DeckSockets |
  Stop DeckSockets |
  Loop DeckSockets |
  Load DeckSockets |
  Queue DeckSockets Int |
  BlockLoop DeckSockets |
  BlockLoopSize DeckSockets (Maybe BlockSize) |
  BPM DeckSockets Float |
  Faster DeckSockets Float |
  Slower DeckSockets Float |
  Transpose DeckSockets Int |
  None

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Vindinium.Types
        ( Vindinium
        , runVindinium
        , asks
        , Settings (..)
        , Key (..)
        , Bot
        , State (..)
        , GameId (..)
        , Game (..)
        , HeroId (..)
        , Hero (..)
        , Board (..)
        , Tile (..)
        , Pos (..)
        , Dir (..)
        , BoardId
        )
    where

import Data.Text (Text)

import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (MonadIO)

type BoardId = String

newtype Key = Key Text deriving (Show, Eq)

data Settings = Settings {
    settingsKey :: Key
  , settingsUrl :: Text
} deriving (Show, Eq)

newtype Vindinium a = Vindinium { unVindinium :: ReaderT Settings IO a }
    deriving (Monad, MonadReader Settings, MonadIO)

runVindinium :: Settings -> Vindinium a -> IO a
runVindinium s = flip runReaderT s . unVindinium

type Bot = State -> Vindinium Dir

data State = State {
    stateGame    :: Game
  , stateHero    :: Hero
  , stateToken   :: Text
  , stateViewUrl :: Text
  , statePlayUrl :: Text
} deriving (Show, Eq)

newtype GameId = GameId Text
    deriving (Show, Eq)

data Game = Game {
    gameId       :: GameId
  , gameTurn     :: Integer
  , gameMaxTurns :: Integer
  , gameHeroes   :: [Hero]
  , gameBoard    :: Board
  , gameFinished :: Bool
} deriving (Show, Eq)

newtype HeroId = HeroId Int
    deriving (Show, Eq)

data Hero = Hero {
    heroId        :: HeroId
  , heroName      :: Text
  , heroUserId    :: Maybe Text
  , heroElo       :: Maybe Integer
  , heroPos       :: Pos
  , heroLife      :: Integer
  , heroGold      :: Integer
  , heroMineCount :: Integer
  , heroSpawnPos  :: Pos
  , heroCrashed   :: Bool
} deriving (Show, Eq)

data Board = Board {
    boardSize  :: Int
  , boardTiles :: [Tile]
} deriving (Show, Eq)

data Tile = FreeTile
          | WoodTile
          | TavernTile
          | HeroTile HeroId
          | MineTile (Maybe HeroId)
    deriving (Show, Eq)

data Pos = Pos {
    posX :: Int
  , posY :: Int
} deriving (Show, Eq)

data Dir = Stay | North | South | East | West
    deriving (Show, Eq)

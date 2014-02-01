{-# LANGUAGE OverloadedStrings #-}
module Vindinium.Api
        ( startTraining
        , startArena
        , move
        )
    where

import Vindinium.Types

import Network.HTTP.Client
import Network.HTTP.Types

import Data.Text (Text, pack, unpack)
import Data.Aeson
import Data.Monoid ((<>))

import Control.Monad (liftM, mzero)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>))

startTraining :: Maybe Int -> Maybe BoardId -> Vindinium State
startTraining mi mb = do
    url <- startUrl "training"
    let obj = object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                    <> maybe [] (\b -> [("map",  toJSON b)]) mb
                     )

    request url obj

move :: State -> Dir -> Vindinium State
move s d = do
    let url = statePlayUrl s
        obj = object [("dir", toJSON d)]

    request url obj


startArena :: Vindinium State
startArena = do
    url <- startUrl "arena"
    let obj = object []

    request url obj

startUrl :: Text -> Vindinium Text
startUrl v = liftM (\x -> x <> "/api/" <> v) $ asks settingsUrl

request :: Text -> Value -> Vindinium State
request url val = do
    key <- asks settingsKey

    initReq <- liftIO $ parseUrl $ unpack url
    let req = initReq
                { method = "POST"
                , requestHeaders =
                    [ (hContentType, "application/json")
                    , (hAccept,      "application/json")
                    , (hUserAgent,   "vindinium-starter-haskell")
                    ]
                , requestBody = jsonBody (injectKey val key)
                }

    liftIO $ withManager defaultManagerSettings $ \mgr ->
        liftM (decodeBody . responseBody) $ httpLbs req mgr

  where
    jsonBody = RequestBodyLBS . encode
    decodeBody body = case eitherDecode body of
            Left e  -> error $ "request: unable to decode state: " ++ e
            Right s -> s
    injectKey (Object a) k =
        let
            (Object b) = object [("key", toJSON k)]
        in
            Object (a <> b)

parseBoard :: Int -> String -> Board
parseBoard s t =
    Board s $ map parse (chunks t)
  where
    chunks []       = []
    chunks (_:[])   = error "chunks: even chars number"
    chunks (a:b:xs) = (a, b):chunks xs

    parse (' ', ' ') = FreeTile
    parse ('#', '#') = WoodTile
    parse ('@', x)   = HeroTile $ HeroId $ read [x]
    parse ('[', ']') = TavernTile
    parse ('$', '-') = MineTile Nothing
    parse ('$', x)   = MineTile $ Just $ HeroId $ read [x]
    parse (a, b)     = error $ "parse: unknown tile pattern " ++ (show $ a:b:[])

printTiles :: [Tile] -> Text
printTiles =
    foldl (<>) "" . map printTile
  where
    printTile FreeTile = "  "
    printTile WoodTile = "##"
    printTile (HeroTile (HeroId i)) = "@" <> (pack $ show i)
    printTile TavernTile = "[]"
    printTile (MineTile Nothing) = "$-"
    printTile (MineTile (Just (HeroId i))) = "$" <> (pack $ show i)

instance ToJSON Key where
    toJSON (Key k) = String k

instance ToJSON Board where
    toJSON b  = object [ "size"  .= boardSize b
                       , "tiles" .= (printTiles $ boardTiles b)
                       ]

instance FromJSON State where
    parseJSON (Object o) = State <$> o .: "game"
                                 <*> o .: "hero"
                                 <*> o .: "token"
                                 <*> o .: "viewUrl"
                                 <*> o .: "playUrl"
    parseJSON _ = mzero

instance FromJSON Game where
    parseJSON (Object o) = Game <$> o .: "id"
                                <*> o .: "turn"
                                <*> o .: "maxTurns"
                                <*> o .: "heroes"
                                <*> o .: "board"
                                <*> o .: "finished"
    parseJSON _ = mzero

instance FromJSON GameId where
    parseJSON x = GameId <$> parseJSON x

instance FromJSON Hero where
    parseJSON (Object o) = Hero <$> o .: "id"
                                <*> o .: "name"
                                <*> o .:? "userId"
                                <*> o .:? "elo"
                                <*> o .: "pos"
                                <*> o .: "life"
                                <*> o .: "gold"
                                <*> o .: "mineCount"
                                <*> o .: "spawnPos"
                                <*> o .: "crashed"
    parseJSON _ = mzero

instance FromJSON HeroId where
    parseJSON x = HeroId <$> parseJSON x

instance FromJSON Pos where
    parseJSON (Object o) = Pos <$> o .: "x" <*> o .: "y"
    parseJSON _ = mzero

instance FromJSON Board where
    parseJSON (Object o) = parseBoard <$> o .: "size" <*> o .: "tiles"
    parseJSON _ = mzero

instance ToJSON Dir where
    toJSON Stay = String "Stay"
    toJSON North = String "North"
    toJSON South = String "South"
    toJSON East = String "East"
    toJSON West = String "West"

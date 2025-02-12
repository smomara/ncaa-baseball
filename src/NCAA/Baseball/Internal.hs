module NCAA.Baseball.Internal where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple
import Text.HTML.Scalpel

type TeamId = Text
type PlayerId = Text
type Year = Int

data Player = Player
  { playerName :: Text
  , playerId :: PlayerId
  , playerNumber :: Maybe Text
  , playerClass :: Maybe Text
  , playerPosition :: Maybe Text
  , playerHeight :: Maybe Text
  , playerBats :: Maybe Text
  , playerThrows :: Maybe Text
  , playerHometown :: Maybe Text
  , playerHighSchool :: Maybe Text
  } deriving (Show)

instance Eq Player where
  a == b = playerId a == playerId b

baseUrl :: Text
baseUrl = T.pack "https://stats.ncaa.org"

fetchHtml :: Text -> IO (Maybe Text)
fetchHtml url = do
  request <- parseRequest (T.unpack url)
  response <- httpBS request
  pure $ case TE.decodeUtf8' (getResponseBody response) of
    Left _ -> Nothing
    Right body -> Just body

maybeText :: Selector -> Scraper Text (Maybe Text)
maybeText selector = do
  t <- text selector
  pure $ if T.null t then Nothing else Just t

textToMaybeInt :: Text -> Maybe Int
textToMaybeInt t = case reads (T.unpack t) of
  [(n, "")] -> Just n
  _ -> Nothing

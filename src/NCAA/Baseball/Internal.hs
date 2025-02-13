{-# LANGUAGE OverloadedStrings #-}

module NCAA.Baseball.Internal where

import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.Ok (Ok)
import Database.SQLite.Simple.ToField (ToField (..))
import Network.HTTP.Simple
import Text.HTML.Scalpel

type TeamId = Text
type PlayerId = Text
type Year = Int
type TeamName = Text
type TeamStats = Map PlayerId HittingStats

data Team = Team
  { teamName :: TeamName
  , teamId :: TeamId
  , teamDivision :: Division
  , teamYear :: Year
  }
  deriving (Show, Eq)

instance FromRow Team where
  fromRow = Team <$> field <*> field <*> field <*> field

instance ToRow Team where
  toRow (Team name tid division year) = toRow (name, tid, division, year)

data Division = Division1 | Division2 | Division3 deriving (Show, Eq, Enum, Bounded)

instance ToField Division where
  toField division = SQLText . T.pack $ show division

instance FromField Division where
  fromField f = do
    divisionText <- fromField f :: Ok Text
    case divisionText of
      "Division1" -> pure Division1
      "Division2" -> pure Division2
      "Division3" -> pure Division3
      _ -> fail "Unknown Division"

data Player = Player
  { playerName :: Text
  , playerId :: PlayerId
  , playerTeamId :: TeamId
  , playerNumber :: Maybe Text
  , playerClass :: Maybe Text
  , playerPosition :: Maybe Text
  , playerHeight :: Maybe Text
  , playerBats :: Maybe Text
  , playerThrows :: Maybe Text
  , playerHometown :: Maybe Text
  , playerHighSchool :: Maybe Text
  }
  deriving (Show)

instance Eq Player where
  a == b = playerId a == playerId b

instance ToRow Player where
  toRow (Player name pid tid num cls pos hgt bats throws town hs) =
    [ toField name
    , toField pid
    , toField tid
    , toField num
    , toField cls
    , toField pos
    , toField hgt
    , toField bats
    , toField throws
    , toField town
    , toField hs
    ]

instance FromRow Player where
  fromRow =
    Player
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

data HittingStats = HittingStats
  { hittingStatsPlayerId :: Text
  , battingAverage :: Double
  , onBasePercentage :: Double
  , sluggingPercentage :: Double
  , runs :: Int
  , atBats :: Int
  , hits :: Int
  , doubles :: Int
  , triples :: Int
  , totalBases :: Int
  , homeRuns :: Int
  , rbis :: Int
  , walks :: Int
  , hitByPitch :: Int
  , sacrificeFlies :: Int
  , sacrificeHits :: Int
  , strikeouts :: Int
  , opponentDoublePlay :: Int
  , caughtStealing :: Int
  , pickedOff :: Int
  , stolenBases :: Int
  , intentionalWalks :: Int
  , groundedIntoDP :: Int
  , rbisWithTwoOuts :: Int
  }
  deriving (Show)

instance ToRow HittingStats where
  toRow (HittingStats pid ba obp slg r ab h d t tb hr rbi bb hbp sf sh k dp cs po sb ibb gidp rbi2) =
    [ toField pid
    , toField ba
    , toField obp
    , toField slg
    , toField r
    , toField ab
    , toField h
    , toField d
    , toField t
    , toField tb
    , toField hr
    , toField rbi
    , toField bb
    , toField hbp
    , toField sf
    , toField sh
    , toField k
    , toField dp
    , toField cs
    , toField po
    , toField sb
    , toField ibb
    , toField gidp
    , toField rbi2
    ]

instance FromRow HittingStats where
  fromRow =
    HittingStats
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field

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

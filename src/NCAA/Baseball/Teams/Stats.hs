{-# LANGUAGE OverloadedStrings #-}

module NCAA.Baseball.Teams.Stats (
  -- * Types
  HittingStats (..),
  TeamStats,

  -- * Queries
  getTeamStats,
  lookupPlayerStats,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NCAA.Baseball.Internal
import Text.HTML.Scalpel
import Text.Read (readMaybe)

type TeamStats = Map PlayerId HittingStats

data HittingStats = HittingStats
  { battingAverage :: Double
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

getTeamStats :: TeamId -> IO (Maybe TeamStats)
getTeamStats tid = do
  body <- fetchHtml $ buildStatsUrl tid
  pure $ body >>= scrapeStats

buildStatsUrl :: TeamId -> Text
buildStatsUrl tid = baseUrl <> "/teams/" <> tid <> "/season_to_date_stats"

lookupPlayerStats :: PlayerId -> TeamStats -> Maybe HittingStats
lookupPlayerStats = M.lookup

scrapeStats :: Text -> Maybe TeamStats
scrapeStats body = do
  pairs <- scrapeStringLike body $ do
    rows <- chroots statsSelector statRows
    hrefs <- chroots statsSelector playerHrefs
    let playerIds = [extractPlayerId href | href <- hrefs, isPlayerHref href]
    pure $ catMaybes $ zipWith makeStats rows playerIds
  pure $ M.fromList pairs
 where
  statsSelector = "table" @: ["id" @= "stat_grid"] // "tbody" // "tr" @: [hasClass "text"]
  statRows = texts ("td" @: [hasClass "align_right"])
  playerHrefs = attr "href" ("td" // "a")
  isPlayerHref = T.isPrefixOf "/players/"

  extractPlayerId url = case T.splitOn "/" url of
    (_ : "players" : pidWithParams : _) -> case T.splitOn "?" pidWithParams of
      (pid : _) -> Just pid
      _ -> Nothing
    _ -> Nothing

  makeStats [ba, obp, slg, r, ab, h, d, t, tb, hr, rbi, bb, hbp, sf, sh, k, dp, cs, po, sb, ibb, gidp, rbi2] (Just pid) =
    Just
      ( pid
      , HittingStats
          { battingAverage = maybeDouble ba
          , onBasePercentage = maybeDouble obp
          , sluggingPercentage = maybeDouble slg
          , runs = maybeInt r
          , atBats = maybeInt ab
          , hits = maybeInt h
          , doubles = maybeInt d
          , triples = maybeInt t
          , totalBases = maybeInt tb
          , homeRuns = maybeInt hr
          , rbis = maybeInt rbi
          , walks = maybeInt bb
          , hitByPitch = maybeInt hbp
          , sacrificeFlies = maybeInt sf
          , sacrificeHits = maybeInt sh
          , strikeouts = maybeInt k
          , opponentDoublePlay = maybeInt dp
          , caughtStealing = maybeInt cs
          , pickedOff = maybeInt po
          , stolenBases = maybeInt sb
          , intentionalWalks = maybeInt ibb
          , groundedIntoDP = maybeInt gidp
          , rbisWithTwoOuts = maybeInt rbi2
          }
      )
  makeStats _ _ = Nothing

  maybeDouble s = fromMaybe 0 $ readMaybe $ T.unpack s
  maybeInt s = fromMaybe 0 $ readMaybe $ T.unpack s

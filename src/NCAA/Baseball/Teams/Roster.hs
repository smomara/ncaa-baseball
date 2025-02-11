{-# LANGUAGE OverloadedStrings #-}

module NCAA.Baseball.Teams.Roster (
  -- * Types
  Player (..),

  -- * Main functions
  getRoster,
) where

import Data.Text (Text)
import qualified Data.Text as T
import NCAA.Baseball.Internal
import Text.HTML.Scalpel

data Player = Player
  { playerName :: Text
  , playerId :: Text
  , playerNumber :: Maybe Text
  , playerClass :: Maybe Text
  , playerPosition :: Maybe Text
  , playerHeight :: Maybe Text
  , playerBats :: Maybe Text
  , playerThrows :: Maybe Text
  , playerHometown :: Maybe Text
  , playerHighSchool :: Maybe Text
  , gamesPlayed :: Maybe Int
  , gamesStarted :: Maybe Int
  }
  deriving (Show, Eq)

getRoster :: TeamId -> IO (Maybe [Player])
getRoster tid = do
  body <- fetchHtml $ buildRosterUrl tid
  pure $ body >>= scrapeRoster

buildRosterUrl :: TeamId -> Text
buildRosterUrl tid = "https://stats.ncaa.org/teams/" <> tid <> "/roster"

scrapeRoster :: Text -> Maybe [Player]
scrapeRoster body = scrapeStringLike body $ do
  rows <- chroots rosterSelector playerRows
  hrefs <- chroots rosterSelector playerHrefs
  let uniqueRows = take (length rows `div` 2) rows
      playerIds = [extractPlayerId href | href <- hrefs, isPlayerHref href]
  pure $ zipWith makePlayer uniqueRows playerIds
 where
  rosterSelector = "table" // "tbody" // "tr"
  playerRows = texts "td"
  playerHrefs = attr "href" ("td" // "a")

  isPlayerHref = T.isPrefixOf "/players/"
  extractPlayerId = last . T.splitOn "/"

  makePlayer [gp, gs, num, name, cls, pos, hgt, bats, throws, town, hs] pid =
    Player
      { playerName = name
      , playerId = pid
      , playerNumber = validateField num
      , playerClass = validateField cls
      , playerPosition = validateField pos
      , playerHeight = validateField hgt
      , playerBats = validateField $ T.strip bats
      , playerThrows = validateField $ T.strip throws
      , playerHometown = validateField town
      , playerHighSchool = validateField hs
      , gamesPlayed = textToMaybeInt gp
      , gamesStarted = textToMaybeInt gs
      }
  makePlayer _ _ = error "Invalid row length"

validateField :: Text -> Maybe Text
validateField = clean . T.strip
 where
  clean t
    | T.null t || T.all (== '-') t = Nothing
    | otherwise = Just t

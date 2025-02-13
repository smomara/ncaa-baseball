{-# LANGUAGE OverloadedStrings #-}

module NCAA.Baseball.Teams.Roster (
  -- * Types
  Player (..),

  -- * Main functions
  getRoster,
) where

import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import NCAA.Baseball.Internal
import Text.HTML.Scalpel

getRoster :: TeamId -> IO (Maybe [Player])
getRoster tid = do
  body <- fetchHtml $ buildRosterUrl tid
  pure $ body >>= scrapeRoster tid

buildRosterUrl :: TeamId -> Text
buildRosterUrl tid = baseUrl <> "/teams/" <> tid <> "/roster"

scrapeRoster :: TeamId -> Text -> Maybe [Player]
scrapeRoster tid body = scrapeStringLike body $ do
  rows <- chroots rosterSelector playerRows
  hrefs <- chroots rosterSelector playerHrefs
  let playerIds = [extractPlayerId href | href <- hrefs, isPlayerHref href]
      players = catMaybes $ zipWith makePlayer rows playerIds
  pure $ nub players
 where
  rosterSelector = "table" // "tbody" // "tr"
  playerRows = texts "td"
  playerHrefs = attr "href" ("td" // "a")

  isPlayerHref = T.isPrefixOf "/players/"
  extractPlayerId = last . T.splitOn "/"

  -- the two blank fields are games played and games started, respectively
  -- we don't handle them here because they are not core to the player's identity
  makePlayer [_, _, num, name, cls, pos, hgt, bats, throws, town, hs] pid =
    Just $
      Player
        { playerName = name
        , playerId = pid
        , playerTeamId = tid
        , playerNumber = validateField num
        , playerClass = validateField cls
        , playerPosition = validateField pos
        , playerHeight = validateField hgt
        , playerBats = validateField $ T.strip bats
        , playerThrows = validateField $ T.strip throws
        , playerHometown = validateField town
        , playerHighSchool = validateField hs
        }
  makePlayer _ _ = Nothing

validateField :: Text -> Maybe Text
validateField = clean . T.strip
 where
  clean t
    | T.null t || T.all (== '-') t = Nothing
    | otherwise = Just t

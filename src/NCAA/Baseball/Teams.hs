{-# LANGUAGE OverloadedStrings #-}

module NCAA.Baseball.Teams (
  -- * Main functions
  getTeams,
  getTeamsByDivision,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import NCAA.Baseball.Internal
import Text.HTML.Scalpel

getTeams :: Year -> IO [Team]
getTeams year = do
  let divisions = [minBound .. maxBound] :: [Division]
  teams <- mapM (`getTeamsByDivision` year) divisions
  pure $ concat teams

getTeamsByDivision :: Division -> Year -> IO [Team]
getTeamsByDivision division year = do
  maybeBody <- fetchHtml $ buildTeamUrl division year
  pure $ fromMaybe [] (maybeBody >>= scrapeTeams division year)

divisionToText :: Division -> Text
divisionToText Division1 = "1"
divisionToText Division2 = "2"
divisionToText Division3 = "3"

buildTeamUrl :: Division -> Int -> Text
buildTeamUrl division year =
  baseUrl
    <> "/team/inst_team_list?sport_code=MBA&division="
    <> divisionToText division
    <> "&academic_year="
    <> T.pack (show year)

scrapeTeams :: Division -> Year -> Text -> Maybe [Team]
scrapeTeams division year body = scrapeStringLike body teams
 where
  teams = chroots ("div" @: [hasClass "css-panes"] // "td" // "a") team
  team = do
    name <- text anySelector
    href <- attr "href" anySelector
    case T.splitOn "/" href of
      (_ : "teams" : tid : _) -> pure $ Team name tid division year
      _ -> fail $ "Invalid team URL format: " <> T.unpack href

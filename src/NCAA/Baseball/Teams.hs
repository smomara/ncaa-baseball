{-# LANGUAGE OverloadedStrings #-}

module NCAA.Baseball.Teams (
  -- * Types
  Team (..),
  Division (..),

  -- * Main functions
  getTeams,
  getTeamsByDivision,

  -- * Helper types and functions
  TeamName,
) where

import Data.Text (Text)
import qualified Data.Text as T
import NCAA.Baseball.Internal
import Text.HTML.Scalpel

type TeamName = Text

data Team = Team
  { teamName :: TeamName
  , teamId :: TeamId
  }
  deriving (Show, Eq)

data Division = Division1 | Division2 | Division3 deriving (Show, Eq, Enum, Bounded)

getTeams :: Year -> IO (Maybe [Team])
getTeams year = do
  let divisions = [minBound .. maxBound] :: [Division]
  teams <- mapM (`getTeamsByDivision` year) divisions
  pure $ concat <$> sequence teams

getTeamsByDivision :: Division -> Year -> IO (Maybe [Team])
getTeamsByDivision division year = do
  body <- fetchHtml $ buildTeamUrl division year
  pure $ body >>= scrapeTeams

divisionToText :: Division -> Text
divisionToText = T.pack . show . (+ 1) . fromEnum

buildTeamUrl :: Division -> Int -> Text
buildTeamUrl division year =
  "https://stats.ncaa.org/team/inst_team_list?sport_code=MBA"
    <> "&division="
    <> divisionToText division
    <> "&academic_year="
    <> T.pack (show year)

scrapeTeams :: Text -> Maybe [Team]
scrapeTeams body = scrapeStringLike body teams
 where
  teams = chroots ("div" @: [hasClass "css-panes"] // "td" // "a") team
  team = do
    name <- text anySelector
    href <- attr "href" anySelector
    case T.splitOn "/" href of
      (_ : "teams" : tid : _) -> pure $ Team name tid
      _ -> fail $ "Invalid team URL format: " <> T.unpack href

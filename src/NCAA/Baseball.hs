module NCAA.Baseball (
  -- * Teams
  Team(..),
  TeamName,
  TeamId,
  Division(..),
  getTeams,
  getTeamsByDivision,

  -- * Players
  Player(..),
  PlayerId,
  getRoster,

  -- * Statistics
  HittingStats(..),
  TeamStats,
  getTeamStats,
  lookupPlayerStats,

  -- * Complete Player Info
  CompletePlayer(..),
  getCompleteRoster,

  -- * Common
  Year
) where

import NCAA.Baseball.Internal (PlayerId, TeamId, Year)
import NCAA.Baseball.Teams
import NCAA.Baseball.Teams.Roster
import NCAA.Baseball.Teams.Stats

data CompletePlayer = CompletePlayer
  { cpInfo :: Player
  , cpStats :: HittingStats
  } deriving (Show)

getCompleteRoster :: TeamId -> IO (Maybe [CompletePlayer])
getCompleteRoster tid = do
  roster <- getRoster tid
  stats <- getTeamStats tid
  pure $ combineRosterAndStats <$> roster <*> stats

combineRosterAndStats :: [Player] -> TeamStats -> [CompletePlayer]
combineRosterAndStats roster statsMap =
  [ CompletePlayer player stats
  | player <- roster
  , Just stats <- [lookupPlayerStats (playerId player) statsMap]
  ]

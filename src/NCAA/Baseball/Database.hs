{-# LANGUAGE OverloadedStrings #-}

module NCAA.Baseball.Database where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, void)
import Database.SQLite.Simple
import NCAA.Baseball
import Data.Foldable (for_)

initializeDB :: IO ()
initializeDB = do
  conn <- open "ncaa.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS teams (name TEXT, teamId TEXT, division TEXT, year INT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS players (name TEXT, playerId TEXT, number TEXT, class TEXT, position TEXT, height TEXT, bats TEXT, throws TEXT, hometown TEXT, highSchool TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS hittingStats (playerId TEXT, battingAverage REAL, onBasePercentage REAL, sluggingPercentage REAL, runs INT, atBats INT, hits INT, doubles INT, triples INT, totalBases INT, homeRuns INT, rbis INT, walks INT, hitByPitch INT, sacrificeFlies INT, sacrificeHits INT, strikeouts INT, opponentDoublePlay INT, caughtStealing INT, pickedOff INT, stolenBases INT, intentionalWalks INT, groundedIntoDP INT, rbisWithTwoOuts INT)"
  close conn

insertTeam :: Connection -> Team -> IO ()
insertTeam conn = execute conn "INSERT INTO teams VALUES (?, ?, ?, ?)"

insertPlayer :: Connection -> Player -> IO ()
insertPlayer conn = execute conn "INSERT INTO players VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertHittingStats :: Connection -> HittingStats -> IO ()
insertHittingStats conn = execute conn "INSERT INTO hittingStats VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

populateDBForYear :: Year -> IO ()
populateDBForYear year = do
  conn <- open "ncaa.db"
  teams <- getTeams year
  void $ mapConcurrently (insertTeamAndData conn) teams
  close conn
 where
  insertTeamAndData conn team = do
    insertTeam conn team
    -- First get stats for the whole team
    maybeStats <- getTeamStats (teamId team)
    -- Then get roster and match with stats
    maybeRoster <- getRoster (teamId team)
    case (maybeRoster, maybeStats) of
      (Just roster, Just stats) -> do
        forM_ roster $ \player -> do
          insertPlayer conn player
          -- For each player in roster, look up their stats
          for_ (lookupPlayerStats (playerId player) stats) (insertHittingStats conn)
      _ -> pure ()

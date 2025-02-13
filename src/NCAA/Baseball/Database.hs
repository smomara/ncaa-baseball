{-# LANGUAGE OverloadedStrings #-}

module NCAA.Baseball.Database where

import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Data.Foldable (for_)
import Database.SQLite.Simple
import NCAA.Baseball

initializeDB :: IO ()
initializeDB = do
  conn <- open "ncaa.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS teams (name TEXT, teamId TEXT, division TEXT, year INT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS players (name TEXT, playerId TEXT, teamId TEXT, number TEXT, class TEXT, position TEXT, height TEXT, bats TEXT, throws TEXT, hometown TEXT, highSchool TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS hittingStats (playerId TEXT, battingAverage REAL, onBasePercentage REAL, sluggingPercentage REAL, runs INT, atBats INT, hits INT, doubles INT, triples INT, totalBases INT, homeRuns INT, rbis INT, walks INT, hitByPitch INT, sacrificeFlies INT, sacrificeHits INT, strikeouts INT, opponentDoublePlay INT, caughtStealing INT, pickedOff INT, stolenBases INT, intentionalWalks INT, groundedIntoDP INT, rbisWithTwoOuts INT)"
  close conn

insertTeam :: Connection -> Team -> IO ()
insertTeam conn = execute conn "INSERT INTO teams VALUES (?, ?, ?, ?)"

insertPlayer :: Connection -> Player -> IO ()
insertPlayer conn = execute conn "INSERT INTO players VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

insertHittingStats :: Connection -> HittingStats -> IO ()
insertHittingStats conn = execute conn "INSERT INTO hittingStats VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

populateDBForYear :: Year -> IO ()
populateDBForYear year = do
  teams <- getTeams year
  void $ mapConcurrently processTeam teams
 where
  processTeam team = do
    (maybeStats, maybeRoster) <-
      concurrently
        (getTeamStats $ teamId team)
        (getRoster $ teamId team)

    case (maybeRoster, maybeStats) of
      (Just roster, Just stats) ->
        bracket (open "ncaa.db") close $ \conn -> do
          insertTeam conn team
          forM_ roster $ \player -> do
            insertPlayer conn player
            for_ (lookupPlayerStats (playerId player) stats) (insertHittingStats conn)
      _ -> pure ()

# NCAA Baseball

Haskell library for accessing NCAA baseball statistics.
Fetches data by scraping the NCAA's [public statistics site](https://stats.ncaa.org).

## Features
- Get teams across all NCAA divisions
- Access team rosters and player information
- Fetch hitting statistics
- Combine roster and stats information
- Store data in SQLite database for cached offline access

## Usage

### Working with Teams
```haskell
import NCAA.Baseball

-- Get all teams across all divisions for 2024
teams <- getTeams 2024

-- Get teams for just Division 1
d1teams <- getTeamsByDivision Division1 2024

-- Access team information
let someTeam = head teams
"Team: " <> teamName someTeam
"ID: " <> teamId someTeam
```

### Accessing Rosters
```haskell
import NCAA.Baseball

-- Get roster for a specific team
roster <- getRoster someTeamId
case roster of
    Just players -> do
        -- Print all pitchers
        let pitchers = filter (\p -> playerPosition p == Just "P") players
        mapM_ (print . playerName) pitchers
    Nothing -> putStrLn "Failed to get roster"
```

### Working with Stats
```haskell
import NCAA.Baseball

-- Get team stats
stats <- getTeamStats someTeamId
case stats of
    Just teamStats -> do
        -- Look up a specific player's stats
        case lookupPlayerStats playerId teamStats of
            Just stats -> print $ "BA: " <> show (battingAverage stats)
            Nothing -> putStrLn "Player not found"
    Nothing -> putStrLn "Failed to get stats"
```

### Combining Roster and Stats
```haskell
import NCAA.Baseball

-- Get complete player information including stats
completeRoster <- getCompleteRoster someTeamId
case completeRoster of
    Just players -> do
        -- Find players batting over .300
        let goodHitters = filter (\p -> battingAverage (cpStats p) > 0.300) players
        forM_ goodHitters $ \player -> do
            putStrLn $ playerName (cpInfo player) <> 
                      ": " <> show (battingAverage $ cpStats player)
    Nothing -> putStrLn "Failed to get complete roster"
```

### Database Storage
```haskell
import NCAA.Baseball
import NCAA.Baseball.Database

-- Initialize database tables
initializeDB

-- Populate database with all teams, rosters, and stats for a given year
-- This fetches data concurrently for better performance
populateDBForYear 2024
```

## Development

```bash
# Enter development shell
nix develop
```

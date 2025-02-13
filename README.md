# NCAA Baseball

Haskell library for accessing NCAA baseball statistics.
Fetches data by scraping the NCAA's [public statistics site](https://stats.ncaa.org).

## Features
- Get teams across all NCAA divisions
- Access team rosters and player information
- Fetch hitting statistics
- Combine roster and stats information
- Store data in SQLite database for offline access

## Installation & Usage

### Using Nix

The recommended way to use this package is with Nix:

```bash
# Development environment
nix develop

# Run the database population tool
nix run

# Install the package
nix profile install github:smomara/ncaa-baseball
```

### Using Cabal

If you prefer not to use Nix:

```bash
# Clone the repository
git clone https://github.com/smomara/ncaa-baseball.git
cd ncaa-baseball

# Build and run
cabal build
cabal run ncaa-db
```

## Library Usage

### Working with Teams
```haskell
import NCAA.Baseball

-- Get all teams across all divisions for 2024
teams <- getTeams 2024

-- Get teams for just Division 1
d1teams <- getTeamsByDivision Division1 2024

-- Access team information
let someTeam = head teams
putStrLn $ "Team: " <> teamName someTeam
putStrLn $ "ID: " <> teamId someTeam
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

## Database Population Tool

The package includes an executable `ncaa-db` that will populate a SQLite database with all NCAA baseball data for the current year. To use it:

```bash
# Using Nix
nix run

# Or using Cabal
cabal run ncaa-db

# This will:
# 1. Create ncaa.db if it doesn't exist
# 2. Initialize required tables
# 3. Fetch and store all teams, rosters, and stats
```

The data is stored in three tables:
- `teams`: Basic team information (name, ID, division, year)
- `players`: Player roster information (name, ID, position, etc.)
- `hittingStats`: Player statistics (batting stats)

## Project Structure
```
ncaa-baseball/
├── app/                    # Executable source
│   └── Main.hs            # Database population tool
├── src/                   # Library source
│   └── NCAA/
│       └── Baseball/      # Core modules
├── ncaa-baseball.cabal    # Project configuration
├── flake.nix             # Nix configuration
└── README.md             # This file
```

## Development

### Prerequisites
If not using Nix:
- GHC 9.6 or later
- Cabal 3.0 or later
- SQLite3

Using Nix automatically provides all required dependencies.

### Development Shell
The Nix development shell includes:
- GHC with all dependencies
- Cabal
- Haskell Language Server
- SQLite

## Acknowledgments

- Data provided by [NCAA Statistics](https://stats.ncaa.org)

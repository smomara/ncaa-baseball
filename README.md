# NCAA Baseball

Haskell library for accessing NCAA baseball statistics.
Fetches data by scraping the NCAA's [public statistics site](stats.ncaa.org).

## Usage

```haskell
import NCAA.Baseball.Teams

-- Get all teams across all division for 2025
teams <- getTeams 2024

-- Get teams for just Division 1
d1teams <- getTeamsByDivision Division1 2024
```

## Development

```bash
# Enter development shell
nix develop
```

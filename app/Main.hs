module Main where

import NCAA.Baseball.Database

main :: IO ()
main = do
  putStrLn "Initializing database..."
  initializeDB

  putStrLn "Populating database for 2024..."
  populateDBForYear 2024

  putStrLn "Database population complete!"

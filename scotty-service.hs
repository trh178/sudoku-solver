{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat)
import Sudoku

main = scotty 3000 $ do

  -- in case someone requests root
  get "/" $ text "Try requesting /generate or /solve/<puzzle>"

  -- generate a puzzle
  get "/generate" $ text "<generated puzzle here>"

  get "/solve" $ text "Try passing a puzzle: /solve/<puzzle>"

  get "/solve/:puzzle" $ do
    p <- param "puzzle"
    html $ mconcat ["<h1>", p, "</h1>"]
    

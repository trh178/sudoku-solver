{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Monoid (mconcat)
import Sudoku
import Control.Monad.Trans
import Control.Applicative ((<$>))
import System.Environment (getEnv)

main = do
  port <- read <$> getEnv "PORT"
  scotty port $ do

    -- in case someone requests root
    get "/" $ text "Try requesting /generate or /solve/<puzzle>"

    -- generate a puzzle
    get "/generate" $ do
      p <- liftIO generate
      json p

    get "/solve" $ text "Try passing a puzzle: /solve/<puzzle>"

    get "/solve/:puzzle" $ do
      p <- param "puzzle"
      json $ solve p
    

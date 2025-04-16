module Main where

import Control.Monad.State (evalStateT)

import Game (
    initialGameState,
    initialize,
    runLoop,
    shutdown,
 )

main :: IO ()
main = flip evalStateT initialGameState $ do
    gameData <- initialize
    runLoop gameData
    shutdown
module Main where

import Control.Monad.State (evalStateT)

import Game (
    exitClean,
    gameLoop,
    initialGameState,
    initialize,
 )

main :: IO ()
main = flip evalStateT initialGameState $ do
    gameData <- initialize
    gameLoop gameData
    exitClean
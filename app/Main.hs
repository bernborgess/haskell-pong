module Main where

import Control.Monad.State (evalStateT)

import Game (
    exitClean,
    gameLoop,
    initSDL,
    initialGameState,
 )

main :: IO ()
main = flip evalStateT initialGameState $ do
    gameData <- initSDL
    gameLoop gameData
    exitClean
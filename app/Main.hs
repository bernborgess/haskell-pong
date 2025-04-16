module Main where

import Control.Monad.State (evalStateT)
import Foreign.C.Types (CInt)

import Game (
    initialGameState,
    initialize,
    runLoop,
    shutdown,
 )

screenWidth, screenHeight :: CInt
screenWidth = 1024
screenHeight = 500

main :: IO ()
main = flip evalStateT initialGameState $ do
    gameData <- initialize screenWidth screenHeight
    runLoop gameData
    shutdown
module Game (
    -- * Core Game Functions
    gameLoop,
    initialGameState,
    exitClean,

    -- * Initialization
    initialize,
)
where

import Control.Monad.State (StateT, gets)
import Data.Foldable (sequenceA_, traverse_)

import qualified SDL

import Game.Initialize (initialGameState, initialize)
import Game.State (
    GameData (..),
    GameState (gameDraws, gameUpdates),
    exitClean,
 )

gameLoop :: GameData -> StateT GameState IO ()
gameLoop gameData = do
    processInput gameData
    updateGame
    generateOutput gameData
    gameLoop gameData

{- | Process all queued SDL events
 Handles:
 - ESC: Clean exit
 - Window close: Clean exit
-}
processInput :: GameData -> StateT GameState IO ()
processInput gameData = SDL.pollEvents >>= traverse_ handleSingleEvent
  where
    handleSingleEvent :: SDL.Event -> StateT GameState IO ()
    handleSingleEvent event = case SDL.eventPayload event of
        SDL.KeyboardEvent ke -> handleKeyboardEvent ke
        SDL.QuitEvent -> handleQuitEvent
        _ -> pure ()

    handleKeyboardEvent :: SDL.KeyboardEventData -> StateT GameState IO ()
    handleKeyboardEvent ke
        | isKeyPressed = case keyCode of
            SDL.KeycodeEscape -> handleEscape
            _ -> pure ()
        | otherwise = pure ()
      where
        isKeyPressed = SDL.keyboardEventKeyMotion ke == SDL.Pressed
        keyCode = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)

    handleQuitEvent :: StateT GameState IO ()
    handleQuitEvent = exitClean

    handleEscape :: StateT GameState IO ()
    handleEscape = exitClean

updateGame :: StateT GameState IO ()
updateGame = gets gameUpdates >>= sequenceA_

generateOutput :: GameData -> StateT GameState IO ()
generateOutput gameData = do
    let renderer = gameRenderer gameData
    let colorEerieBlack = SDL.V4 27 27 27 255
    SDL.rendererDrawColor renderer SDL.$= colorEerieBlack
    SDL.clear renderer

    gets gameDraws >>= traverse_ ($ gameData)

    SDL.present renderer
    SDL.delay 16

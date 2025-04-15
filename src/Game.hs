module Game (
    -- * Core Game Functions
    gameLoop,
    initialGameState,
    exitClean,

    -- * Initialization
    initSDL,
)
where

import Control.Monad.State (StateT)

import qualified SDL

import Data.Foldable (traverse_)
import Game.InitSDL (initSDL)
import Game.StateManagement (
    GameData (..),
    GameState,
    exitClean,
    initialGameState,
 )

gameLoop :: GameData -> StateT GameState IO ()
gameLoop gameData = do
    SDL.pollEvents >>= handleEvents gameData

    let renderer = gameRenderer gameData
    SDL.clear renderer

    SDL.present renderer
    SDL.delay 16
    gameLoop gameData

{- | Process all queued SDL events
 Handles:
 - ESC: Clean exit
 - Window close: Clean exit
-}
handleEvents :: GameData -> [SDL.Event] -> StateT GameState IO ()
handleEvents gameData = traverse_ handleSingleEvent
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

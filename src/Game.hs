module Game (
    -- * Core Game Functions
    gameLoop,
    initialGameState,
    exitClean,

    -- * Initialization
    initSDL,
)
where

import Control.Monad.State (StateT, forM_, gets)

import qualified SDL

import Data.Foldable (traverse_)
import Foreign.C.Types (CInt)
import Game.InitSDL (initSDL)
import Game.StateManagement (
    GameData (..),
    GameState (),
    exitClean,
    initialGameState,
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

-- TODO
updateGame :: StateT GameState IO ()
updateGame = do
    return ()

generateOutput :: GameData -> StateT GameState IO ()
generateOutput gameData = do
    let renderer = gameRenderer gameData
    let colorEerieBlack = SDL.V4 27 27 27 255
    SDL.rendererDrawColor renderer SDL.$= colorEerieBlack
    SDL.clear renderer

    -- drawables <- gets gameDrawables
    -- forM_ drawables $ \d -> case d of
    --     (DrawableBall b) -> do
    --         return ()

    -- ? Test: Just draw a square

    let colorHoneyDew = SDL.V4 250 255 250 255
    SDL.rendererDrawColor renderer SDL.$= colorHoneyDew
    let square = SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 100 100) :: SDL.Rectangle CInt
    SDL.fillRect renderer (Just square)

    SDL.present renderer
    SDL.delay 16

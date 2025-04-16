module Game (
    -- * Core Game Functions
    gameLoop,
    initialGameState,
    exitClean,

    -- * Initialization
    initialize,
)
where

import Control.Monad.State (get, gets, put, unless)
import Data.Foldable (traverse_)

import qualified SDL

import Game.Initialize (initialGameState, initialize)
import Game.State (
    GameData (..),
    GameProcedure,
    GameState (..),
    exitClean,
 )

gameLoop :: GameData -> GameProcedure
gameLoop gameData = do
    processInput gameData
    updateGame gameData
    generateOutput gameData
    gameLoop gameData

{- | Process all queued SDL events
 Handles:
 - ESC: Clean exit
 - Window close: Clean exit
-}
processInput :: GameData -> GameProcedure
processInput _ = do
    -- Game Events
    SDL.pollEvents >>= traverse_ handleSingleEvent
    -- Pass keyboard state to process input of actors
    ks <- SDL.getKeyboardState
    gets gameProcessInputs >>= traverse_ ($ ks)
  where
    handleSingleEvent :: SDL.Event -> GameProcedure
    handleSingleEvent event = case SDL.eventPayload event of
        SDL.KeyboardEvent ke -> handleKeyboardEvent ke
        SDL.QuitEvent -> handleQuitEvent
        _ -> pure ()

    handleKeyboardEvent :: SDL.KeyboardEventData -> GameProcedure
    handleKeyboardEvent ke
        | isKeyPressed = case keyCode of
            SDL.KeycodeEscape -> handleEscape
            _ -> pure ()
        | otherwise = pure ()
      where
        isKeyPressed = SDL.keyboardEventKeyMotion ke == SDL.Pressed
        keyCode = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)

    handleQuitEvent :: GameProcedure
    handleQuitEvent = exitClean

    handleEscape :: GameProcedure
    handleEscape = exitClean

updateGame :: GameData -> GameProcedure
updateGame gameData = do
    -- Wait for enough ticks
    waitForTicks

    -- Calculate new tick
    gs <- get
    currentTicks <- SDL.ticks
    let gTicks = gameTicks gs
        deltaTime = min 0.05 $ fromIntegral (currentTicks - gTicks) / 1000.0 :: Float

    put gs{gameTicks = currentTicks}

    -- Runs updates of all Actors with deltaTime
    updateActors gameData deltaTime
  where
    waitForTicks :: GameProcedure
    waitForTicks = do
        currentTicks <- SDL.ticks
        gTicks <- gets gameTicks
        let ticksPassed = currentTicks >= gTicks + 16
        unless ticksPassed waitForTicks

updateActors :: GameData -> Float -> GameProcedure
updateActors gameData deltaTime = do
    -- TODO: Implement addition and remotion of actors
    -- TODO: Set gameUpdatingActors to True

    -- Call update on each actor
    gets gameUpdates >>= traverse_ (\up -> up gameData deltaTime)

    -- TODO: Set gameUpdatingActors to False
    -- TODO: filter dead actors from actors list
    return ()

generateOutput :: GameData -> GameProcedure
generateOutput gameData = do
    let renderer = gameRenderer gameData
    let colorEerieBlack = SDL.V4 27 27 27 255
    SDL.rendererDrawColor renderer SDL.$= colorEerieBlack
    SDL.clear renderer

    gets gameDraws >>= traverse_ ($ renderer)

    SDL.present renderer

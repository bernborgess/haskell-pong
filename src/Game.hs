module Game (
    -- * Core Game Functions
    runLoop,
    initialGameState,
    shutdown,

    -- * Initialization
    initialize,
)
where

import Control.Monad.State (StateT, get, gets, put, unless)
import Data.Foldable (traverse_)

import qualified SDL

-- import Game.Initialize (initialGameState, initialize)

import Actors.Ball (ballSetPosition, ballSetVelocity, initializeBall, newBall)
import Actors.Paddle (initializePaddle, newPaddle, paddleSetPosition)
import Game.Config (windowConfig, windowTitle)
import Game.State (
    GameData (..),
    GameProcedure,
    GameState (..),
    addClean,
    safeRun,
    shutdown,
 )
import Linear (V2 (V2))

initialGameState :: GameState
initialGameState =
    GameState
        { gameActions = []
        , gameTicks = 0
        , -- \* Actors
          gameBall = newBall
        , gamePaddle = newPaddle
        , -- \* Game Loop Methods
          gameProcessInputs = []
        , gameUpdates = []
        , gameDraws = []
        }

initialize :: StateT GameState IO GameData
initialize = do
    addClean $ putStrLn "All Clean!"

    safeRun
        SDL.initializeAll
        "Error initializing SDL2"
    addClean SDL.quit

    window <-
        safeRun
            (SDL.createWindow windowTitle windowConfig)
            "Error creating the Window"
    addClean $ SDL.destroyWindow window

    renderer <-
        safeRun
            (SDL.createRenderer window (-1) SDL.defaultRenderer)
            "Error creating the Renderer"
    addClean $ SDL.destroyRenderer renderer

    addClean $ SDL.destroyRenderer renderer

    addClean $ putStrLn "Start Cleaning"

    -- Set the initial clock
    gs <- get
    ticks <- SDL.ticks
    put gs{gameTicks = ticks}

    initializeActors

    return GameData{gameRenderer = renderer, gameWindow = window}

runLoop :: GameData -> GameProcedure
runLoop gameData = do
    processInput gameData
    updateGame gameData
    generateOutput gameData
    runLoop gameData

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
    handleQuitEvent = shutdown

    handleEscape :: GameProcedure
    handleEscape = shutdown

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

initializeActors :: GameProcedure
initializeActors = do
    --  Instantiate the mPaddle paddle and initialize its position with the paddleSetPosition method
    initializePaddle
    paddleSetPosition $ V2 100.0 100.0

    -- Instantiate the mBall ball and initialize its position and velocity with the
    -- ballSetPosition and ballSetVelocity methods, respectively
    initializeBall
    ballSetPosition $ V2 400.0 400.0
    ballSetVelocity $ V2 400.0 400.0
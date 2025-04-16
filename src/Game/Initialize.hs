module Game.Initialize (initialize, initialGameState) where

import Control.Monad.State (MonadState (get), StateT, put)

import qualified SDL

import Actors.Ball (ballDraw, ballUpdate, newBall)
import Actors.Paddle (newPaddle, paddleDraw, paddleProcessInput, paddleUpdate)
import Game.Config (windowConfig, windowTitle)
import Game.State (GameData (..), GameState (..), addClean, safeRun)

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

    return GameData{gameRenderer = renderer, gameWindow = window}

initialGameState :: GameState
initialGameState =
    GameState
        { gameActions = []
        , gameTicks = 0
        , -- \* Actors
          gameBall = newBall
        , gamePaddle = newPaddle
        , -- \* Game Loop Methods
          gameProcessInputs = [paddleProcessInput]
        , gameUpdates = [ballUpdate, paddleUpdate]
        , gameDraws = [ballDraw, paddleDraw]
        }

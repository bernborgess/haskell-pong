module Game.Initialize (initialize, initialGameState) where

import Control.Monad.State (MonadState (get), StateT, put)

import qualified SDL

import Actors.Ball (ballDraw)
import Actors.Paddle (paddleDraw, paddleUpdate)
import Actors.Types (Ball (..), Paddle (..))
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
        , gameBall =
            Ball
                { ballPosition = SDL.V2 100 0
                , ballSize = SDL.V2 15 15
                , ballColor = SDL.V4 250 255 250 255 -- HoneyDew #F0FFF0
                }
        , gamePaddle =
            Paddle
                { paddlePosition = SDL.V2 100 100
                , paddleSize = SDL.V2 15 100
                , paddleColor = SDL.V4 0 255 0 0 -- Lime #00FF00
                }
        , gameDraws = [ballDraw, paddleDraw]
        , gameUpdates = [paddleUpdate]
        }

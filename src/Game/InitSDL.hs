module Game.InitSDL (initSDL) where

import Control.Monad.State (StateT)

import qualified SDL

import Game.Config (windowConfig, windowTitle)
import Game.StateManagement (GameData (..), GameState, addClean, safeRun)

initSDL :: StateT GameState IO GameData
initSDL = do
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

    return GameData{gameRenderer = renderer, gameWindow = window}
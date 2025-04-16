module Actors.Ball (ballDraw) where

import Control.Monad.State (StateT, gets)

import qualified SDL

import Actors.Types (Ball (..))
import Game.State (GameData (..), GameState (..))

ballDraw :: GameData -> StateT GameState IO ()
ballDraw gd = do
    ball <- gets gameBall
    let renderer = gameRenderer gd
        position = ballPosition ball
        size = ballSize ball
    SDL.rendererDrawColor renderer SDL.$= ballColor ball
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P position) size)

module Actors.Ball (newBall, ballDraw) where

import Control.Monad.State (StateT, gets)

import qualified SDL

import Actors.Types (Ball (..))
import Game.State (GameData (..), GameState (..))

newBall :: Ball
newBall =
    Ball
        { ballPosition = SDL.V2 100 0
        , ballSize = SDL.V2 15 15
        , ballColor = SDL.V4 250 255 250 255 -- HoneyDew #F0FFF0
        }

ballDraw :: GameData -> StateT GameState IO ()
ballDraw gd = do
    ball <- gets gameBall
    let renderer = gameRenderer gd
        position = ballPosition ball
        size = ballSize ball
    SDL.rendererDrawColor renderer SDL.$= ballColor ball
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P position) size)

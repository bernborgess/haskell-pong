module Actors.Ball (newBall, ballDraw) where

import Control.Monad.State (StateT, gets)
import Linear (V2 (V2))

import qualified SDL

import Actors.Types (Ball (..))
import Components.DrawComponent (drawRectangle)
import Game.State (GameState (..))

newBall :: Ball
newBall =
    Ball
        { ballPosition = V2 100 0
        , ballSize = 15
        , ballColor = SDL.V4 250 255 250 255 -- HoneyDew #F0FFF0
        }

ballDraw :: SDL.Renderer -> StateT GameState IO ()
ballDraw renderer = do
    ball <- gets gameBall
    drawRectangle (ballPosition ball) (ballSize ball, ballSize ball) (Just $ ballColor ball) renderer
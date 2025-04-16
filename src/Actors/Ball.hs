{-# LANGUAGE NamedFieldPuns #-}

module Actors.Ball (newBall, ballDraw) where

import Control.Monad.State (gets)
import Linear (V2 (V2))

import qualified SDL

import Actors.Types (Ball (..))
import Components.DrawComponent (drawRectangle)
import Game.State (GameProcedure, GameState (..))

newBall :: Ball
newBall =
    Ball
        { ballPosition = V2 100 0
        , ballSize = 15
        , ballColor = SDL.V4 250 255 250 255 -- HoneyDew #F0FFF0
        }

ballDraw :: SDL.Renderer -> GameProcedure
ballDraw renderer = do
    Ball{ballPosition, ballSize, ballColor} <- gets gameBall
    drawRectangle ballPosition (ballSize, ballSize) (Just ballColor) renderer
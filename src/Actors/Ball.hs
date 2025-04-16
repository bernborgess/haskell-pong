{-# LANGUAGE NamedFieldPuns #-}

module Actors.Ball (
    newBall,
    initializeBall,
    ballSetPosition,
    ballSetVelocity,
) where

import Control.Monad.State (get, gets, modify, when)
import Linear (V2 (V2), zero)

import qualified SDL

import Actors.Types (Ball (..), Paddle (..))
import Components.DrawComponent (drawRectangle)
import Data.Ord (clamp)
import Game.State (DrawProcedure, GameData (gameWindow), GameProcedure, GameState (..), UpdateProcedure, addActor, addDrawable, shutdown)

newBall :: Ball
newBall =
    Ball
        { ballPosition = zero
        , ballSize = 15
        , ballVelocity = zero
        , ballColor = SDL.V4 250 255 250 255 -- HoneyDew #F0FFF0
        }

initializeBall :: GameProcedure
initializeBall = do
    addDrawable ballDraw
    addActor Nothing ballUpdate

ballSetPosition :: V2 Float -> GameProcedure
ballSetPosition pos = do
    ball <- gets gameBall
    modify $ \gs -> gs{gameBall = ball{ballPosition = pos}}

ballSetVelocity :: V2 Float -> GameProcedure
ballSetVelocity pos = do
    ball <- gets gameBall
    modify $ \gs -> gs{gameBall = ball{ballVelocity = pos}}

ballUpdate :: UpdateProcedure
ballUpdate gameData deltaTime = do
    GameState
        { gameBall = Ball{ballPosition = V2 posX posY, ballVelocity = V2 velX velY, ballSize}
        , gamePaddle = Paddle{paddlePosition = V2 paddlePosX paddlePosY, paddleWidth, paddleHeight}
        } <-
        get

    -- Get screen height
    windowConfig <- SDL.getWindowConfig (gameWindow gameData)
    let SDL.V2 windowWidth windowHeight = SDL.windowInitialSize windowConfig
    -- screenY = fromIntegral windowHeight :: Float

    let newPosX = posX + velX * deltaTime
        newPosY = posY + velY * deltaTime
        dx = newPosX - paddlePosX
        dy = abs $ newPosY - paddlePosY

    -- \* Ball passed through the left edge -> GAME OVER
    when (newPosX < 0.0) shutdown

    let newVelX
            -- Hit the paddle
            | velX < 0.0
                && dy <= toEnum (paddleHeight + ballSize) / 2.0
                && dx == clamp (0.0, toEnum paddleWidth) dx =
                -velX
            -- Hit the right wall
            | velX > 0.0
                && newPosX >= toEnum (fromEnum windowWidth - (ballSize `div` 2)) =
                -velX
            | otherwise = velX

    let newVelY
            -- Hit the top wall
            | velY < 0.0
                && newPosY <= toEnum (ballSize `div` 2) =
                -velY
            -- Hit the bottom wall
            | velY > 0.0
                && newPosY >= toEnum (fromEnum windowHeight - (ballSize `div` 2)) =
                -velY
            | otherwise = velY

    -- Update ball kinetic values
    modify $ \gs ->
        gs
            { gameBall =
                (gameBall gs)
                    { ballPosition = V2 newPosX newPosY
                    , ballVelocity = V2 newVelX newVelY
                    }
            }

ballDraw :: DrawProcedure
ballDraw renderer = do
    Ball{ballPosition, ballSize, ballColor} <- gets gameBall
    drawRectangle ballPosition (ballSize, ballSize) (Just ballColor) renderer
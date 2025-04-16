{-# LANGUAGE NamedFieldPuns #-}

module Actors.Paddle (
    newPaddle,
    paddleProcessInput,
    paddleUpdate,
    paddleDraw,
) where

import Control.Monad.State (gets, modify, when)
import Data.Ord (clamp)

import qualified SDL

import Actors.Types (Paddle (..))
import Components.DrawComponent (drawRectangle)
import Game.State (GameData (..), GameProcedure, GameState (..))

paddleVerticalSpeed :: Float
paddleVerticalSpeed = 1000.0

newPaddle :: Paddle
newPaddle =
    Paddle
        { paddlePosition = SDL.V2 100.0 100.0
        , paddleWidth = 15
        , paddleHeight = 100
        , paddleColor = SDL.V4 0 255 0 0 -- Lime #00FF00
        , paddleDirection = 0.0
        }

paddleProcessInput :: (SDL.Scancode -> Bool) -> GameProcedure
paddleProcessInput ks = do
    modify $ \gs -> gs{gamePaddle = (gamePaddle gs){paddleDirection = newDirection}}
  where
    -- Change direction
    up = ks SDL.ScancodeUp || ks SDL.ScancodeW
    down = ks SDL.ScancodeDown || ks SDL.ScancodeS
    newDirection | up = -1 | down = 1 | otherwise = 0

paddleUpdate :: GameData -> Float -> GameProcedure
paddleUpdate gameData deltaTime = do
    paddle <- gets gamePaddle
    let direction = paddleDirection paddle
    when (direction /= 0) $ do
        let deltaY = paddleVerticalSpeed * direction * deltaTime

        -- Get screen height
        windowConfig <- SDL.getWindowConfig (gameWindow gameData)
        let SDL.V2 _ windowHeight = SDL.windowInitialSize windowConfig
            screenY = fromIntegral windowHeight :: Float

        -- Get paddle height
        let mHeight = fromIntegral (paddleHeight paddle) :: Float

            -- Get current position
            SDL.V2 paddleX paddleY = paddlePosition paddle

            -- Calculate new position
            newPaddleY = clamp (mHeight / 2.0, screenY - mHeight / 2.0) (paddleY + deltaY)

        modify $ \gs -> gs{gamePaddle = paddle{paddlePosition = SDL.V2 paddleX newPaddleY}}

paddleDraw :: SDL.Renderer -> GameProcedure
paddleDraw renderer = do
    Paddle{paddlePosition, paddleWidth, paddleHeight, paddleColor} <- gets gamePaddle
    drawRectangle paddlePosition (paddleWidth, paddleHeight) (Just paddleColor) renderer

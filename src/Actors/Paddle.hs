module Actors.Paddle (
    newPaddle,
    paddleProcessInput,
    paddleUpdate,
    paddleDraw,
) where

import Control.Monad.State (StateT, gets, modify, when)
import Data.Ord (clamp)
import GHC.Float (floorFloat)

import qualified SDL

import Actors.Types (Paddle (..))
import Foreign.C.Types (CInt)
import Game.State (GameData (..), GameState (..))
import Linear (V2 (V2))

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

paddleProcessInput :: (SDL.Scancode -> Bool) -> StateT GameState IO ()
paddleProcessInput ks = do
    modify $ \gs -> gs{gamePaddle = (gamePaddle gs){paddleDirection = newDirection}}
  where
    -- Change direction
    up = ks SDL.ScancodeUp || ks SDL.ScancodeW
    down = ks SDL.ScancodeDown || ks SDL.ScancodeS
    newDirection | up = -1 | down = 1 | otherwise = 0

paddleUpdate :: GameData -> Float -> StateT GameState IO ()
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

paddleDraw :: GameData -> StateT GameState IO ()
paddleDraw gd = do
    let renderer = gameRenderer gd
    paddle <- gets gamePaddle
    SDL.rendererDrawColor renderer SDL.$= paddleColor paddle

    -- \* Create a SDL_Rect rectangle to visually represent the object:
    -- The position of the rectangle should be the center of the object
    -- (not the upper left corner, as originally defined by SDL).
    -- This will make collision calculations easier.

    -- Get the original position of the object (upper left corner) , the width and height
    let V2 posX posY = paddlePosition paddle
        width = toEnum $ paddleWidth paddle
        height = toEnum $ paddleHeight paddle

    -- To move the object's position to its center, subtract the original position from the x coordinate
    -- by half the object's width
    let px = floorFloat posX - width `div` 2 :: CInt
    -- and the y coordinate by half the height
    let py = floorFloat posY - height `div` 2 :: CInt
    -- Assign the result of these operations as the final position of the created rectangle.
    -- The height and width of the object do not need to be transformed.
    let rectangle = SDL.Rectangle (SDL.P (SDL.V2 px py)) (SDL.V2 width height)

    SDL.fillRect renderer (Just rectangle)

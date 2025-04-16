module Actors.Ball (newBall, ballDraw) where

import Control.Monad.State (StateT, gets)
import Linear (V2 (V2))

import qualified SDL

import Actors.Types (Ball (..))
import Foreign.C.Types (CInt)
import GHC.Float (floorFloat)
import Game.State (GameData (..), GameState (..))

newBall :: Ball
newBall =
    Ball
        { ballPosition = V2 100 0
        , ballSize = 15
        , ballColor = SDL.V4 250 255 250 255 -- HoneyDew #F0FFF0
        }

ballDraw :: GameData -> StateT GameState IO ()
ballDraw gd = do
    let renderer = gameRenderer gd
    ball <- gets gameBall
    SDL.rendererDrawColor renderer SDL.$= ballColor ball

    -- \* Create a SDL_Rect rectangle to visually represent the object:
    -- The position of the rectangle should be the center of the object
    -- (not the upper left corner, as originally defined by SDL).
    -- This will make collision calculations easier.

    -- Get the original position of the object (upper left corner) , the width and height
    let V2 posX posY = ballPosition ball
        width = toEnum $ ballSize ball
        height = toEnum $ ballSize ball

    -- To move the object's position to its center, subtract the original position from the x coordinate
    -- by half the object's width
    let px = floorFloat posX - width `div` 2 :: CInt
    -- and the y coordinate by half the height
    let py = floorFloat posY - height `div` 2 :: CInt
    -- Assign the result of these operations as the final position of the created rectangle.
    -- The height and width of the object do not need to be transformed.
    let rectangle = SDL.Rectangle (SDL.P (SDL.V2 px py)) (SDL.V2 width height)

    SDL.fillRect renderer (Just rectangle)
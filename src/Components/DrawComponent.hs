module Components.DrawComponent (drawRectangle) where

import Control.Monad.State (StateT)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import GHC.Float (floorFloat)
import Linear (V2 (V2))

import qualified SDL

import Game.State (GameState)

drawRectangle :: V2 Float -> (Int, Int) -> Maybe (SDL.V4 Word8) -> SDL.Renderer -> StateT GameState IO ()
drawRectangle (V2 posX posY) (width, height) mc renderer = do
    SDL.rendererDrawColor renderer SDL.$= color
    -- \* Create a SDL_Rect rectangle to visually represent the object:
    -- The position of the rectangle should be the center of the object
    -- (not the upper left corner, as originally defined by SDL).
    -- This will make collision calculations easier.
    -- Get the original position of the object (upper left corner) , the width and height
    -- To move the object's position to its center, subtract the original position from the x coordinate
    -- by half the object's width
    let px = floorFloat posX - cWidth `div` 2 :: CInt
    -- and the y coordinate by half the height
    let py = floorFloat posY - cHeight `div` 2 :: CInt
    -- Assign the result of these operations as the final position of the created rectangle.
    -- The height and width of the object do not need to be transformed.
    let rectangle = SDL.Rectangle (SDL.P (SDL.V2 px py)) (SDL.V2 cWidth cHeight)

    SDL.fillRect renderer (Just rectangle)
  where
    color = fromMaybe (SDL.V4 255 255 255 255) mc -- default color if Nothing is passed: white #FFFFFF
    cWidth = toEnum width
    cHeight = toEnum height
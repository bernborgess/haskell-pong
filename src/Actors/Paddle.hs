module Actors.Paddle (Paddle (..)) where

import Data.Word (Word8)
import qualified SDL
import qualified SDL.Raw.Types

data Paddle = Paddle
    { paddlePosition :: SDL.V2 Int
    , paddleSize :: SDL.V2 Int
    , paddleColor :: SDL.V4 Word8
    }
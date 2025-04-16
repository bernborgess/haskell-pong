module Actors.Types (Ball (..), Paddle (..)) where

import Data.Word (Word8)
import Linear (V2)

import qualified SDL

data Ball = Ball
    { ballPosition :: V2 Float
    , ballSize :: Int
    , ballVelocity :: V2 Float
    , ballColor :: SDL.V4 Word8
    }

data Paddle = Paddle
    { paddlePosition :: V2 Float
    , paddleWidth :: Int
    , paddleHeight :: Int
    , paddleColor :: SDL.V4 Word8
    , paddleDirection :: Float
    }

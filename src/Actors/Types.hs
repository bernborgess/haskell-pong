module Actors.Types (Ball (..), Paddle (..)) where

import Data.Word (Word8)
import Foreign.C.Types (CInt)
import qualified SDL

data Ball = Ball
    { ballPosition :: SDL.V2 CInt
    , ballSize :: SDL.V2 CInt
    , ballColor :: SDL.V4 Word8
    }

data Paddle = Paddle
    { paddlePosition :: SDL.V2 CInt
    , paddleSize :: SDL.V2 CInt
    , paddleColor :: SDL.V4 Word8
    }

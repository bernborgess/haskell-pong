module Actors.Ball (Ball (..)) where

import Data.Word (Word8)
import Foreign.C.Types (CInt)
import qualified SDL

-- import qualified SDL.Raw

data Ball = Ball
    { ballPosition :: SDL.V2 CInt
    , ballSize :: SDL.V2 CInt
    , ballColor :: SDL.V4 Word8
    }

-- ballDraw :: GameData -> Procedure
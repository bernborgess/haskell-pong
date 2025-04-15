module Game.Config (
    windowTitle,
    screenWidth,
    screenHeight,
    windowConfig,
) where

import Data.Text (Text, pack)
import Foreign.C.Types (CInt)
import qualified SDL

-- | Window title displayed in title bar
windowTitle :: Text
windowTitle = pack "Window"

-- | Initial screen dimensions in pixels
screenWidth, screenHeight :: CInt
screenWidth = 1024
screenHeight = 500

{- | Window configuration specifying:
- Centered initial position
- Default window properties with custom size
-}
windowConfig :: SDL.WindowConfig
windowConfig =
    SDL.defaultWindow
        { SDL.windowPosition = SDL.Centered
        , SDL.windowInitialSize = SDL.V2 screenWidth screenHeight
        }

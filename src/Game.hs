module Game (runGame) where

import Actors.Actor (Actor (..))
import qualified SDL

runGame :: [Actor] -> IO ()
runGame actors = do
  SDL.initialize [SDL.InitVideo]
  -- ... game loop using actors ...
  SDL.quit

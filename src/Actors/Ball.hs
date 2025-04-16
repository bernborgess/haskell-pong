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
    ball <- gets gameBall
    let renderer = gameRenderer gd
        V2 ballX ballY = ballPosition ball
        px = floorFloat ballX :: CInt
        py = floorFloat ballY :: CInt
        size = toEnum $ ballSize ball :: CInt
    SDL.rendererDrawColor renderer SDL.$= ballColor ball
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P (SDL.V2 px py)) (SDL.V2 size size))

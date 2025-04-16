module Actors.Paddle (paddleDraw) where

import Control.Monad.State (StateT, gets)

import qualified SDL

import Actors.Types (Paddle (..))
import Game.State (GameData (..), GameState (..))

paddleDraw :: GameData -> StateT GameState IO ()
paddleDraw gd = do
    paddle <- gets gamePaddle
    let renderer = gameRenderer gd
        position = paddlePosition paddle
        size = paddleSize paddle
    SDL.rendererDrawColor renderer SDL.$= paddleColor paddle
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P position) size)

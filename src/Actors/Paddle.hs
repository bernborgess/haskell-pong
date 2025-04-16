module Actors.Paddle (paddleDraw, paddleUpdate) where

import Control.Monad.State (MonadState (put), StateT, get, gets)

import qualified SDL

import Actors.Types (Paddle (..))
import Foreign.C.Types (CInt)
import Game.State (GameData (..), GameState (..))

paddleVel :: CInt
paddleVel = 10

paddleUpdate :: StateT GameState IO ()
paddleUpdate = do
    ks <- SDL.getKeyboardState
    gs <- get
    let up = ks SDL.ScancodeUp || ks SDL.ScancodeW
        down = ks SDL.ScancodeDown || ks SDL.ScancodeS
        paddle = gamePaddle gs
        SDL.V2 oldX oldY = paddlePosition paddle
        newY
            | up && not down = oldY - paddleVel
            | down && not up = oldY + paddleVel
            | otherwise = oldY
    put gs{gamePaddle = paddle{paddlePosition = SDL.V2 oldX newY}}

paddleDraw :: GameData -> StateT GameState IO ()
paddleDraw gd = do
    paddle <- gets gamePaddle
    let renderer = gameRenderer gd
        position = paddlePosition paddle
        size = paddleSize paddle
    SDL.rendererDrawColor renderer SDL.$= paddleColor paddle
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P position) size)

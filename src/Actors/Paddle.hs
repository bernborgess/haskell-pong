module Actors.Paddle (
    newPaddle,
    paddleProcessInput,
    paddleUpdate,
    paddleDraw,
) where

import Control.Monad.State (MonadIO (liftIO), StateT, gets, liftIO, modify, when)
import Data.Ord (clamp)
import GHC.Float (floorFloat)

import qualified SDL

import Actors.Types (Paddle (..))
import Foreign.C.Types (CInt)
import Game.State (GameData (..), GameState (..))
import Text.Printf (printf)

paddleVel :: CInt
paddleVel = 100

newPaddle :: Paddle
newPaddle =
    Paddle
        { paddlePosition = SDL.V2 100 100
        , paddleSize = SDL.V2 15 100
        , paddleColor = SDL.V4 0 255 0 0 -- Lime #00FF00
        , paddleDirection = 0
        }

paddleProcessInput :: (SDL.Scancode -> Bool) -> StateT GameState IO ()
paddleProcessInput ks = do
    modify $ \gs -> gs{gamePaddle = (gamePaddle gs){paddleDirection = newDirection}}
    liftIO $ printf "New paddle direction: %d\n" (fromEnum newDirection)
  where
    -- Change direction
    up = ks SDL.ScancodeUp || ks SDL.ScancodeW
    down = ks SDL.ScancodeDown || ks SDL.ScancodeS
    newDirection
        | up = -1
        | down = 1
        | otherwise = 0

paddleUpdate :: GameData -> Float -> StateT GameState IO ()
paddleUpdate gameData deltaTime = do
    paddle <- gets gamePaddle
    let direction = paddleDirection paddle
    when (direction /= 0) $ do
        -- TODO: Keep positions with Float instead of CInt
        let deltaY = fromIntegral (paddleVel * direction) * deltaTime
            window = gameWindow gameData
        windowConfig <- SDL.getWindowConfig window
        let SDL.V2 _windowWidth windowHeight = SDL.windowInitialSize windowConfig
            SDL.V2 _ paddleHeight = paddleSize paddle
            mHeight = fromIntegral paddleHeight :: Float
            screenY = fromIntegral windowHeight :: Float
            SDL.V2 paddleX paddleY = paddlePosition paddle
            mPosY = fromIntegral paddleY :: Float
            newY = clamp (mHeight / 2.0, screenY - mHeight / 2.0) (mPosY + deltaY)
            finalPaddleY = floorFloat newY :: CInt

        modify $ \gs -> gs{gamePaddle = paddle{paddlePosition = SDL.V2 paddleX finalPaddleY}}
        liftIO $ putStrLn "Entrei no when!"

paddleDraw :: GameData -> StateT GameState IO ()
paddleDraw gd = do
    paddle <- gets gamePaddle
    let renderer = gameRenderer gd
        position = paddlePosition paddle
        size = paddleSize paddle
    SDL.rendererDrawColor renderer SDL.$= paddleColor paddle
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P position) size)

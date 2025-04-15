{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Game.StateManagement (
    GameData (..),
    GameState (..),
    addClean,
    exitClean,
    initialGameState,
    safeRun,
)
where

import Control.Exception (SomeException, catch)
import Control.Monad.State (StateT, forM_, gets, liftIO, modify)
import Data.Word (Word32)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, hPutStrLn, stderr)

import qualified SDL

import Actor (Actor (..))
import Actor.Ball (Ball, createBall)
import Drawable (Drawable (..))

-- import Component.DrawComponent (Drawable(..))

-- | Constant values to setup the game
data GameData = GameData
    { gameWindow :: SDL.Window
    , gameRenderer :: SDL.Renderer
    }

-- TODO: Move this OUT!
-- class DrawableC d where
--     draw :: d -> SDL.Renderer -> StateT GameState IO ()

-- data Drawable = forall d. (DrawableC d) => Drawable d

-- instance DrawableC Drawable where
--     draw :: Drawable -> SDL.Renderer -> StateT GameState IO ()
--     draw (Drawable d) = draw d

-- ! OVER TODO: Move this OUT!

-- | Mutable values that run the game
data GameState = GameState
    { gameActions :: [IO ()]
    , gameActors :: [Actor]
    , gameDrawables :: [Drawable]
    , gameTicksCount :: Word32
    --   , gamePaddle :: Paddle
    --   gameBall :: Ball
    }

initialGameState :: GameState
initialGameState =
    GameState
        { gameActions = []
        , gameActors = [ActorBall $ createBall (0, 0) (1, 1)]
        , gameTicksCount = 0
        , gameDrawables = [DrawableBall $ createBall (0, 0) (1, 1)]
        -- , gameBall = createBall (0, 0) (1, 1)
        }

exitClean :: StateT GameState IO ()
exitClean = do
    actions <- gets gameActions
    forM_ actions liftIO
    liftIO exitSuccess

errorClean :: [IO ()] -> String -> SomeException -> IO a
errorClean actions errorMsg e = do
    liftIO $ hPutStrLn stderr $ errorMsg ++ ":"
    liftIO $ hPrint stderr e
    liftIO $ sequence_ actions
    liftIO exitFailure

{- | Safely executes IO actions with error handling and cleanup.
 Automatically performs registered cleanup actions on error.
-}
safeRun :: IO a -> String -> StateT GameState IO a
safeRun action errorMsg = do
    actions <- gets gameActions
    liftIO $ catch action $ errorClean actions errorMsg

addClean :: IO () -> StateT GameState IO ()
addClean action =
    modify $ \gameState ->
        let oldActions = gameActions gameState
         in gameState{gameActions = action : oldActions}

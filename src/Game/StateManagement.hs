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
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, hPutStrLn, stderr)

import Actors.Ball (Ball (..))
import Actors.Paddle (Paddle (..))
import qualified SDL

-- | Constant values to setup the game
data GameData = GameData
    { gameWindow :: SDL.Window
    , gameRenderer :: SDL.Renderer
    }

-- type Procedure = StateT GameState IO ()

-- How to make this move out?
ballDraw :: GameData -> StateT GameState IO ()
ballDraw gd = do
    ball <- gets gameBall
    let renderer = gameRenderer gd
        position = ballPosition ball
        size = ballSize ball
    SDL.rendererDrawColor renderer SDL.$= ballColor ball
    SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P position) size)

-- | Mutable values that run the game
data GameState = GameState
    { gameActions :: [IO ()]
    , gameBall :: Ball
    , gamePaddle :: Paddle
    , gameDraws :: [GameData -> StateT GameState IO ()]
    , gameUpdates :: [StateT GameState IO ()]
    }

initialGameState :: GameState
initialGameState =
    GameState
        { gameActions = []
        , gameBall =
            Ball
                { ballPosition = SDL.V2 100 0
                , ballSize = SDL.V2 40 40
                , ballColor = SDL.V4 255 255 255 255
                }
        , gamePaddle =
            Paddle
                { paddlePosition = SDL.V2 0 0
                , paddleSize = SDL.V2 0 0
                , paddleColor = SDL.V4 0 0 0 0
                }
        , gameDraws = [ballDraw]
        , gameUpdates = []
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

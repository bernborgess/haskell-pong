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

import qualified SDL

-- | Constant values to setup the game
data GameData = GameData
    { gameWindow :: SDL.Window
    , gameRenderer :: SDL.Renderer
    }

-- | Mutable values that run the game
data GameState = GameState
    {gameActions :: [IO ()]}

initialGameState :: GameState
initialGameState = GameState{gameActions = []}

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

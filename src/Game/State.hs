module Game.State (
    GameData (..),
    GameState (..),
    GameProcedure,
    addClean,
    exitClean,
    safeRun,
)
where

import Control.Exception (SomeException, catch)
import Control.Monad.State (StateT, forM_, gets, liftIO, modify)
import Data.Word (Word32)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPrint, hPutStrLn, stderr)

import qualified SDL

import Actors.Types (Ball (..), Paddle (..))

-- | Constant values to setup the game
data GameData = GameData
    { gameWindow :: SDL.Window
    , gameRenderer :: SDL.Renderer
    }

-- | Mutable values that run the game
data GameState = GameState
    { gameActions :: [IO ()]
    , gameTicks :: Word32
    , -- \* Actors
      gameBall :: Ball
    , gamePaddle :: Paddle
    , -- \* Game Loop Methods
      gameProcessInputs :: [(SDL.Scancode -> Bool) -> GameProcedure]
    , gameUpdates :: [GameData -> Float -> GameProcedure]
    , gameDraws :: [SDL.Renderer -> GameProcedure]
    }

-- | Type Alias used when mutating game state
type GameProcedure = StateT GameState IO ()

-- | Helper that runs all clean actions
exitClean :: GameProcedure
exitClean = do
    actions <- gets gameActions
    forM_ actions liftIO
    liftIO exitSuccess

-- | Helper that runs all actions and logs the error message
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

-- | Method to add an action to the cleanup list
addClean :: IO () -> GameProcedure
addClean action =
    modify $ \gameState ->
        let oldActions = gameActions gameState
         in gameState{gameActions = action : oldActions}

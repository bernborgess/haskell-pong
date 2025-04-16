module Game.State (
    GameData (..),
    GameState (..),
    GameProcedure,
    ProcessInputProcedure,
    UpdateProcedure,
    DrawProcedure,
    addClean,
    addDrawable,
    addActor,
    shutdown,
    safeRun,
)
where

import Control.Exception (SomeException, catch)
import Control.Monad.State (StateT, forM_, get, gets, liftIO, modify)
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
      gameProcessInputs :: [ProcessInputProcedure]
    , gameUpdates :: [UpdateProcedure]
    , gameDraws :: [DrawProcedure]
    }

-- | Type Alias used when mutating game state
type GameProcedure = StateT GameState IO ()

type ProcessInputProcedure = (SDL.Scancode -> Bool) -> GameProcedure
type UpdateProcedure = GameData -> Float -> GameProcedure
type DrawProcedure = SDL.Renderer -> GameProcedure

-- | Helper that runs all clean actions
shutdown :: GameProcedure
shutdown = do
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

addDrawable :: DrawProcedure -> GameProcedure
addDrawable draw = do
    GameState{gameDraws = draws} <- get
    modify $ \gs -> gs{gameDraws = draw : draws}

addActor :: Maybe ProcessInputProcedure -> UpdateProcedure -> GameProcedure
addActor mpri up = do
    GameState{gameProcessInputs = pris, gameUpdates = ups} <- get
    let npris = case mpri of
            Just pri -> pri : pris
            Nothing -> pris

    modify $ \gs ->
        gs
            { gameProcessInputs = npris
            , gameUpdates = up : ups
            }

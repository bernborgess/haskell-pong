-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE InstanceSigs #-}

module Component () where

-- import Actor (Actor)
-- import Control.Monad.State (StateT)
-- import Game.StateManagement (GameState)

-- import qualified SDL

-- class ComponentC c where
--     owner :: c -> Actor
--     update :: c -> c
--     processInput :: (SDL.Scancode -> Bool) -> StateT GameState IO ()

-- data Component = forall c. (ComponentC c) => Component c

-- instance ComponentC Component where
--     owner :: Component -> Actor
--     owner (Component c) = owner c

--     update :: Component -> Component
--     update (Component c) = Component $ update c

--     processInput :: (SDL.Scancode -> Bool) -> StateT GameState IO ()
--     processInput key = do
--         -- IDK
--         undefined

-- import qualified SDL

-- class Component a where
-- getOwner :: a -> Actor

-- class Drawable
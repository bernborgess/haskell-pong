{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}

module Actor (Actor (..)) where

import Actor.Ball (Ball)
import Actor.Paddle (Paddle)

-- -- Common interface for all actors
-- class ActorC a where
--     getPosition :: a -> (Double, Double)
--     update :: a -> a -- Example method: update actor's state
--     -- components :: [Component]

-- data Actor = forall a. (ActorC a) => Actor a

-- instance ActorC Actor where
--     getPosition :: Actor -> (Double, Double)
--     getPosition (Actor a) = getPosition a

--     update :: Actor -> Actor
--     update (Actor a) = Actor (update a)

data Actor = ActorBall Ball | ActorPaddle Paddle
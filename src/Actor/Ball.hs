{-# LANGUAGE InstanceSigs #-}

module Actor.Ball (Ball, createBall) where

-- import Actor (ActorC (..))

data Ball = Ball
    { position :: (Double, Double)
    , velocity :: (Double, Double)
    }

createBall :: (Double, Double) -> (Double, Double) -> Ball
createBall pos vel = Ball pos vel

-- instance ActorC Ball where
--     getPosition :: Ball -> (Double, Double)
--     getPosition = position

--     update :: Ball -> Ball
--     update b = b{position = (x + dx, y + dy)}
--       where
--         (x, y) = position b
--         (dx, dy) = velocity b

-- drawBall :: SDL.Renderer ->
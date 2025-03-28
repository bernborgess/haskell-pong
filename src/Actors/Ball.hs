module Actors.Ball where

import Actors.Actor (Actor (..))
import Components.Component
import Components.DrawComponent

initialBall :: Actor
initialBall =
  Actor
    { transform = (0, 0),
      components = [ballPhysics],
      drawComponent = ballDraw
    }

ballPhysics :: Component
ballPhysics = Component $ \deltaTime ->
  -- Update logic
  pure ballPhysics -- Return updated component

ballDraw :: DrawComponent
ballDraw = DrawComponent $ \renderer ->
  -- Draw logic
  pure ()
module Actors.Actor where

import Components.Component
import Components.DrawComponent

data Actor = Actor
  { transform :: (Float, Float), -- Position
    components :: [Component],
    drawComponent :: DrawComponent
  }
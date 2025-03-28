module Components.DrawComponent where

import SDL (Renderer)

data DrawComponent = DrawComponent
  { draw :: Renderer -> IO () -- Rendering logic
  }
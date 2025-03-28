module Components.Component where

-- Base component type
data Component = Component
  { update :: Double -> IO Component -- Update logic
  }
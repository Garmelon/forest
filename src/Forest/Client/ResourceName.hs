module Forest.Client.ResourceName
  ( ResourceName(..)
  ) where

data ResourceName = RnViewport | RnEditor
  deriving (Show, Eq, Ord)

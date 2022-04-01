module Graphics.Glapple.Data.Component where

import Graphics.Glapple.Data.Hooks (Hooks)

newtype Component input sprite a = Component (input -> Hooks sprite a)

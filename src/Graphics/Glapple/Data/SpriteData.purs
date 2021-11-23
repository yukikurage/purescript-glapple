module Graphics.Glapple.Data.SpriteData where

import Graphics.Glapple.Data.Picture (Picture)

data SpriteData s = FromImage s String | FromPicture s (Picture s)
module MapGen.Space where

type Width  = Int
type Height = Int

data Vec2 = Vec2
     { x :: {-# UNPACK #-} !Int
     , y :: {-# UNPACK #-} !Int
     } deriving (Eq, Ord, Show)

instance Num Vec2 where
         (Vec2 x1 y1) + (Vec2 x2 y2) = Vec2 (x1+x2) (y1+y2)
         (Vec2 x1 y1) - (Vec2 x2 y2) = Vec2 (x1-x2) (y1-y2)
         (Vec2 x1 y1) * (Vec2 x2 y2) = Vec2 (x1*x2) (y1*y2)
         abs (Vec2 x y) = Vec2 (abs x) (abs y)
         signum (Vec2 x y) = Vec2 (signum x) (signum y)
         fromInteger i = Vec2 i' i' where i' = fromInteger i

data Line = Line
     { o :: !Vec2 -- Origin
     , d :: !Vec2 -- Destination
     } deriving (Eq, Ord, Show)

data Quad = Quad
     { p1 :: !Vec2
     , p2 :: !Vec2
     } deriving (Eq, Ord, Show)
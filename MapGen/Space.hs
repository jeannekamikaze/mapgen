module MapGen.Space where

data Vec2 = Vec2
     { x :: {-# UNPACK #-} !Int
     , y :: {-# UNPACK #-} !Int
     } deriving (Eq, Show)

data Line = Line
     { o :: !Vec2 -- Origin
     , d :: !Vec2 -- Destination
     } deriving (Eq, Show)

data Quad = Quad
     { p1 :: !Vec2
     , p2 :: !Vec2
     } deriving (Eq, Show)
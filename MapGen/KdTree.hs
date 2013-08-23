module MapGen.KdTree
(
    KdTree(..)
    -- * Construction
,   kdtree
,   kdtree'
,   defaultSplitStdev
,   Seed
,   Depth
,   Stdev
    -- * Access
,   splitters
,   quads
    -- * Traversal
,   fold
,   mapM_
)
where

import MapGen.Space

import Control.Monad (liftM2)
import Control.Monad.Random
import Control.Monad.State hiding (mapM_)
import qualified Data.Random.Normal as N
import System.Random hiding (split)
import Prelude hiding (mapM_)

type Tangent = Line

type Seed   = Int
type Depth  = Int
type Stdev  = Double

type Normals = [Double]

data Direction = Vertical | Horizontal deriving (Eq, Show)

data Params = Params
     { width      :: Width  -- ^ Map width
     , height     :: Height -- ^ Map height
     , depth      :: Depth  -- ^ Maximum kd-tree depth
     , splitStdev :: Stdev  -- ^ Normal distribution standard deviation.
     } deriving Show

data KdTree
     = Node
     { splitter :: !Line
     , left     :: !KdTree
     , right    :: !KdTree
     }
     | Leaf
     { quad :: !Quad
     }
     deriving Show

-- Random monad with implicit normal distribution.

mean = 0.5
defaultSplitStdev = 0.2

type RandM a = StateT Normals Rand a

normal :: RandM Double
normal = get >>= \(x:xs) -> put xs >> return x

def (Just x) _ = x
def Nothing  x = x

-- | Construct a random Kd-Tree.
kdtree :: Width -> Height -> Depth -> Maybe Stdev -> Seed -> KdTree
kdtree w h d stdev' s = flip evalRand (mkStdGen s) . kdtree'' $ Params w h d stdev
       where stdev = def stdev' defaultSplitStdev

-- | Construct a random Kd-Tree.
kdtree' :: RandomGen g => g -> Width -> Height -> Depth -> Maybe Stdev -> KdTree
kdtree' g w h d stdev' = flip evalRand g . kdtree'' $ Params w h d stdev
        where stdev = def stdev' defaultSplitStdev

kdtree'' :: Params -> Rand KdTree
kdtree'' ps = do
         let w = width ps
             h = height ps
             q = Quad (Vec2 0 0) (Vec2 (w-1) (h-1))
             stdev = splitStdev ps
         normals <- rand >>= return . N.mkNormals' (mean,stdev) :: Rand Normals
         tang <- randTangent w h
         evalStateT (subtree (depth ps) q tang) normals

subtree :: Depth -> Quad -> Tangent -> RandM KdTree
subtree 0 q _ = return $ Leaf q
subtree d q t = do
        let dir = perpDir t
        (t', q1, q2) <- split q dir
        if degenerate t' then return $ Leaf q
           else liftM2 (Node t') (subtree (d-1) q1 t') (subtree (d-1) q2 t')

degenerate :: Line -> Bool
degenerate (Line p1 p2) = p1 == p2

-- Split the oriented quad in the given direction.
-- The quad's orientation determines the splitter's orientation.
split :: Quad -> Direction -> RandM (Line, Quad, Quad)

split q@(Quad p1 p2) Vertical = do
      n <- normal' -- A "normal" in [0,1]
      let x' = lerp n (x p1) (x p2) -- Map from [0,1] to [x1,x2]
      return . split' q $ Line (Vec2 x' (y p1)) (Vec2 x' (y p2))

split q@(Quad p1 p2) Horizontal = do
      n <- normal' -- A "normal" in [0,1]
      let y' = lerp n (y p1) (y p2) -- Map from [0,1] to [y1,y2]
      return . split' q $ Line (Vec2 (x p1) y') (Vec2 (x p2) y')

split' :: Quad -> Line -> (Line, Quad, Quad)
split' (Quad p1 p2) l@(Line p3 p4) = (l, Quad p1 p4', Quad p3' p2)
       where p4' = disp p1 p4
             p3' = disp p2 p3
             disp pline pquad = pquad +
                  if (x p3 == x p4)
                  -- Vertical line
                  then if (x pquad > x pline) then Vec2 (-1) 0 else Vec2 1 0
                  -- Horizontal line
                  else if (y pquad > y pline) then Vec2 0 (-1) else Vec2 0 1

lerp :: Double -> Int -> Int -> Int
lerp n a1' a2' = floor $ a1 + n*(a2-a1)
     where a1 = fromIntegral a1'
           a2 = fromIntegral a2'

clamp a b x = if x < a then a else if x > b then b else x

normal' = fmap (clamp 0 1) normal

randTangent :: Int -> Int -> Rand Tangent
randTangent x y = do
            xp1 <- oneOf [0,x]
            yp1 <- oneOf [0,y]
            xp2 <- oneOf [0,x]
            let yp2 = if xp1 /= xp2 then yp1 else y-yp1
            return $ Line (Vec2 xp1 yp1) (Vec2 xp2 yp2)

perpDir :: Line -> Direction
perpDir l = if vertical l then Horizontal else Vertical

vertical :: Line -> Bool
vertical (Line p1 p2) = x p1 == x p2

-- Access

splitters :: KdTree -> [Line]
splitters = fold splitter []
          where splitter ss l@Leaf{} = ss
                splitter ss l@(Node s _ _) = s:ss

quads :: KdTree -> [Quad]
quads = fold f []
      where f qs l@(Leaf q) = q:qs
            f qs _ = qs

-- Traversal

fold :: (a -> KdTree -> a) -> a -> KdTree -> a
fold f a l@Leaf{} = f a l
fold f a n@(Node _ c1 c2) = fold f (fold f (f a n) c1) c2

mapM_ :: Monad m => (KdTree -> m a) -> KdTree -> m ()
mapM_ r l@Leaf{} = r l >> return ()
mapM_ r n@(Node _ c1 c2) = r n >> mapM_ r c1 >> mapM_ r c2 >> return ()
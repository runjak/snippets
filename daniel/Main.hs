module Main where

import Control.Monad
import Control.Monad.State as St
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Word (Word8)
import Prelude hiding (Left, Right, last)

data Segment = Edge | Plain deriving (Show, Eq)

cube :: [Segment]
cube = [p,p,e,e,e,p,e,e,p,e,e,e,p,e,p,e,e,e,e,p,e,p,e,p,e,p]
  where
    e = Edge
    p = Plain

data Direction = Left | Right | Up | Down | Back | Front
  deriving (Show, Eq, Ord, Enum)

type Position = (Word8, Word8, Word8)

walk :: Direction -> Segment -> [Direction]
walk d      Plain = return d
walk Left   Edge  = [Up,   Down,   Back, Front]
walk Right  Edge  = [Up,   Down,   Back, Front]
walk Up     Edge  = [Left, Right,  Back, Front]
walk Down   Edge  = [Left, Right,  Back, Front]
walk Back   Edge  = [Left, Right,  Up,   Down]
walk Front  Edge  = [Left, Right,  Up,   Down]

applyD :: Position -> Direction -> Position
applyD (x,y,z) Left   = (x - 1,y,z)
applyD (x,y,z) Right  = (x + 1,y,z)
applyD (x,y,z) Up     = (x,y + 1,z)
applyD (x,y,z) Down   = (x,y - 1,z)
applyD (x,y,z) Back   = (x,y,z + 1)
applyD (x,y,z) Front  = (x,y,z - 1)

valid :: Position -> Bool
valid (x,y,z)
  | x < 0 || y < 0 || z < 0 = False
  | x > 2 || y > 2 || z > 2 = False
  | otherwise = True

data Cube = Cube {
    last :: (Direction, Position)
  , field :: Set Position
  , history :: [(Direction,Position)]
  } deriving (Show)

type CState = State Cube

cstep :: Segment -> CState [Cube]
cstep s = do
  h@(d, p) <- gets last
  f <- gets field
  hs <- gets history
  let
    ds = walk d s
    ps = map (applyD p) ds
    ns' = filter (valid . snd) $ zip ds ps
    ns = filter (flip S.notMember f . snd) ns'
  return $ map (\dp -> Cube dp (S.insert (snd dp) f) $ h:hs) ns

csolve :: [Segment] -> CState [Cube]
csolve [] = liftM return get
csolve (s:ss) = liftM (concatMap $ evalState $ csolve ss) $ cstep s

starts :: [Cube]
starts = [
      mkCube Right  (0,0,0)
    , mkCube Left   (1,0,0)
    , mkCube Up     (1,0,0)
    , mkCube Back   (1,0,0)
    , mkCube Left   (1,1,0)
    , mkCube Up     (1,1,0)
    , mkCube Back   (1,1,0)
    , mkCube Left   (1,1,1)
  ]
  where
    mkCube :: Direction -> Position -> Cube
    mkCube d p = Cube (d, p) (S.fromList [p]) []

main :: IO ()
main = mapM_ (print . history) $ concatMap (evalState $ csolve cube) starts

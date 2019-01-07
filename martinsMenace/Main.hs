module Main where

import qualified Data.Set as Set
import Data.Set (Set, (\\))

type N = Int
type Position = (N, N)
type Piece = [Position]
type Field = Set Position
type Scenario = (Field, [[Field]])

pieces :: [Piece]
pieces = [
  {-
      #
    # # # #
  -}
    [ (0, 0)
    , (1, 0)
    , (1, 1)
    , (2, 0)
    , (3, 0)
    ]
  {-
      #
      #
    # # #
  -}
  , [ (0, 0)
    , (1, 0)
    , (2, 0)
    , (1, 1)
    , (1, 2)
    ]
  {-
      # #
      #
    # #
  -}
  , [ (0, 0)
    , (1, 0)
    , (1, 1)
    , (1, 2)
    , (2, 2)
    ]
  {-
      #
      # #
    # #
  -}
  , [ (0, 0)
    , (1, 0)
    , (1, 1)
    , (2, 1)
    , (1, 2)
    ]
  ]

mirror :: Piece -> Piece
mirror = fmap . uncurry $ flip (,)

rotate :: Piece -> Piece
rotate = fmap (\(x, y) -> (y, -x))

move :: Position -> Piece -> Piece
move (dx, dy) = fmap (\(x, y) -> (x + dx, y + dy))

foldlPiece :: (N -> N -> N) -> Position -> Piece -> Position
foldlPiece λ defaults = foldl1 (\(a, b) (x, y) -> (λ a x, λ b y)) . (defaults :)

dimension :: Piece -> Position
dimension = foldlPiece max (0, 0)

moveIntoPositive :: Piece -> Piece
moveIntoPositive piece =
  let (wx, wy) = foldlPiece min (0, 0) piece
  in move (-wx, -wy) piece

variants :: Piece -> [Piece]
variants p =
  let p' = mirror p
      f = take 4 . iterate rotate
  in fmap moveIntoPositive $ f p <> f p'

fieldForDimension :: Position -> Field
fieldForDimension (a, b) = Set.fromList [(x, y)| x <- [0..a], y <- [0..b]]

fields :: [Field]
fields = [
    -- 90°
    fieldForDimension (4, 3)
    -- 45°
  , fieldForDimension (5, 5) \\ Set.fromList [
      (0,0), (1,0), (4,0), (5, 0)
    , (0,1), (5,1)
    , (5,3)
    , (0,4), (4,4), (5,4)
    , (0,5), (1,5), (3,5), (4,5), (5,5)
    ]
    -- 22.5°
  , fieldForDimension (5, 5) \\ Set.fromList [
      (0,0), (1,0), (3,0), (4,0), (5,0)
    , (0,1), (5,1)
    , (0,2)
    , (0,4), (1,4), (5,4)
    , (0,5), (1,5), (2,5), (3,5), (5,5)
    ]
    -- 22.5°+?
  , fieldForDimension (6, 5) \\ Set.fromList [
      (0,0), (1,0), (3,0), (4,0), (5,0), (6,0)
    , (0,1), (5,1), (6,1)
    , (0,2)
    , (6,3)
    , (0,4), (1,4), (5,4), (6,4)
    , (0,5), (1,5), (2,5), (3,5), (5,5), (6,5)
    ]
  ]

piecePlacementCandidates :: Field -> Piece -> [Field]
piecePlacementCandidates field piece =
  let ps = variants piece
      ms = [move (x, y) p | p <- ps, x <- [0..10], y <- [0..10]]
  in filter (Set.isSubsetOf `flip` field) $ fmap Set.fromList ms

scenarios :: [Scenario]
scenarios = [(f, map (piecePlacementCandidates f) pieces) | f <- fields]

play :: Scenario -> [[Piece]]
play (currentField, placementCandidates) = play' (currentField, []) placementCandidates
  where
    play' :: (Field, [Piece]) -> [[Field]] -> [[Piece]]
    play' (_, history) [] = [history]
    play' (currentField, history) (f:fs) =
      let possibleMoves = filter (Set.isSubsetOf `flip` currentField) f
          nexts = fmap (\m -> (currentField \\ m, Set.toList m : history)) possibleMoves
      in if null possibleMoves
        then []
        else concatMap (play' `flip` fs) nexts

testScenario :: Scenario
testScenario =
  let field = fieldForDimension (4, 4) \\ Set.fromList [(0,0), (0,1), (4,0), (4,1), (0,3), (0,4), (1,3), (3,3), (4,3), (4,4)]
  in (field, piecePlacementCandidates field <$> drop 1 pieces)

main :: IO ()
main = do
  putStrLn "Martin's Menace"
  print $ fmap play scenarios

module Maze2 where

import Control.Monad
import Prelude hiding (Left, Right)

data Direction = Up|Down|Left|Right deriving (Show,Read,Eq)
directions = [Up, Down, Left, Right]

data Position a = Position {
		pCarry :: a
		,pLeft :: [a]
		,pRight :: [a]
		,pUp :: [Position a]
		,pDown :: [Position a]
	}

possibleDirections :: Position a -> [Direction]
possibleDirections p = do
	let u = if null (pUp p)
		then []
		else [Up]
	let d = if null (pDown p)
		then []
		else [Down]
	let l = if null (pLeft p)
		then []
		else [Left]
	let r = if null (pRight p)
		then []
		else [Right]
	concat [u,d,l,r]

move :: Position a -> Direction -> Position a
move p Up		= (head $ pUp p){pDown = p:pDown p}
move p Down		= (head $ pDown p){pUp = p:pUp p}
move p Left		= p
move p Right	= p

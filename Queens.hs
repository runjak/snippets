module Queens where

import Control.Monad

type Row = Int

class Filter f where
	collides :: f -> Row -> Bool
	incDistance :: f -> f

data Queen = Queen {
		 row		:: Row
		,distance	:: Int
	}deriving(Show)

mkQueen :: Row -> Queen
mkQueen = flip Queen 0

instance Filter Queen where
	collides q r
		| r == row q = True
		| otherwise = -- Something is wrong here o.O
			let p = row q * distance q
			in or [r == p, r == negate p]

	incDistance q = q{distance= distance q + 1}

newtype QueenList = QueenList [Queen]

addQueen :: QueenList -> Queen -> QueenList
addQueen ql = addQueen' (incDistance ql)
	where addQueen' (QueenList ql) q = QueenList (q:ql)

emptyQL = QueenList []

instance Filter QueenList where
	collides (QueenList q) r = or $ map (flip collides r) q
	
	incDistance (QueenList q) = QueenList $ map incDistance q

instance Show QueenList where
	show (QueenList ql) = unlines $ map (\q -> replicate (row q - 1) ' ' ++ ['x']) ql

type FieldSize = Int

rowQueens :: FieldSize -> [Queen]
rowQueens fs = take fs $ map mkQueen [1..]

go :: [Queen] -> QueenList -> FieldSize -> [QueenList]
go _ ql 0 = [ql]
go qs ql fs = do
	next <- filter (not . collides ql . row) qs
	let ql' = addQueen ql next
	go qs ql' (fs - 1)

solve :: FieldSize -> [QueenList]
solve fs = go (rowQueens fs) emptyQL fs

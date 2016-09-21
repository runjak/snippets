module Maze where

import Control.Monad (liftM)
import Data.Array
import Data.List (groupBy, permutations)
import System.Random (randoms, newStdGen)

width = 60 :: Int
height = 20 :: Int
type Position = (Int,Int)
origin = (1, 1) :: Position
size = (height, width) :: Position

type Field = Array Position Char
startField :: Field
startField = array (origin,size) . zip [(y,x)|y<-[1..height], x<-[1..width]] $ repeat '░'

showField :: Field -> String
showField field = unlines . map (map (\x -> field!x)) . groupBy (\x y -> fst x == fst y) $ indices field

up (y, x)	= (y, x-1)
down (y, x)	= (y, x+1)
left (y, x)	= (y-1, x)
right (y, x)	= (y+1, x)

next :: Position -> [Position]
next p = filter (inRange (down . right $ origin, up . left $ size)) $ map (\f -> f p) [up,down,left,right]

shuffle x l = let slist = permutations l in slist!!(x`mod` length slist)

maze :: Position -> Field -> [Int] -> Field
maze position field (r:rs) = if (>1) . length . filter (==' ') . map (field!) $ next position
		then field
		else do
			let field' = field // [(position,' ')]
			let nxt = next position
			if null nxt
				then field'
				else foldl (\f p -> maze p f rs) field' $ shuffle r nxt

main :: IO ()
main = putStrLn =<< liftM (showField . maze (2,2) startField . randoms) newStdGen

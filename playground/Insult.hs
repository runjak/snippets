module Insult where

{-
  Inspired by http://lost-in-space.soup.io/post/356758305/Butterface-crotch-waffle 
-}
import Control.Concurrent (threadDelay)
import Control.Monad
import System.Random as Random

w1,w2,w3 :: [String]
w1 = ["Lazy","Stupid","Insecure","Idiotic","Slimy","Slutty","Smelly","Pompous","Communist","Dicknose","Pie-eating","Racist","Elitist","White trash","Drug-loving","Butterface","Tone deaf","Ugly","Creepy"]
w2 = ["douche","ass","turd","rectum","butt","cock","shit","crotch","bitch","turd","prick","slut","taint","fuck","dick","boner","shart","nut","sphincter"]
w3 = ["pilot","canoe","captain","pirate","hammer","knob","box","jockey","nazi","waffle","goblin","blossom","biscuit","clown","socket","monster","hound","dragon","balloon"]
ws = [w1,w2,w3]

lengths :: [Int]
lengths = cycle $ map length ws

walk :: [String] -> [String]
walk xs = let y  = unwords $ take 3 xs
              ys = walk    $ drop 3 xs
          in y:ys

main :: IO ()
main = do
  rs <- liftM randoms newStdGen
  let ixs = zipWith mod rs lengths
      ws' = zipWith (!!) (cycle ws) ixs
      slp = const $ threadDelay 1000000
  mapM_ (slp <=< putStrLn) $ walk ws'

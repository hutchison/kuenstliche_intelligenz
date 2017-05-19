module Util where

import System.IO hiding (openFile)
import System.Process
import System.Exit
import Data.List
import Data.Maybe

type Prob = Float
type RandomNumber = Prob

trace' :: String -> a -> a
trace' _ a = a

argMax :: Ord b => [a] -> (a -> b) -> a
argMax [] vf = error "Can't compute the argmax from an empty list of candidates"
argMax xs vf = fst $ head $ sortBy (\(x,v) -> \(x',v') -> compare v' v) $ map (\x -> (x, vf x)) xs

argMaxR :: (Ord b, Eq b) => RandomNumber -> [a] -> (a -> b) -> a
argMaxR r [] vf = error "Can't compute the argmax from an empty list of candidates"
argMaxR r xs vf = x
  where
    -- mv is the maximum value vf(x)
    mv = maximum $ map vf xs
    -- xc contains all maximal elements (x-candidates)
    xc = filter (\x -> (vf x) == mv) xs
    -- x is a random element from xc
    x  = xc !! (floor (r * (fromIntegral $ length xc)))

average :: Fractional a => [a] -> a
average vs = sum vs / (fromIntegral $ length vs)

-- |Return @True@ with probability 'p' for a given random number 'r'.
trueP :: RandomNumber -> Prob -> Bool
trueP r p = r <= p

randomChoice :: RandomNumber -> [a] -> a
randomChoice r xs = xs !! (floor (r * (fromIntegral $ length xs)))


sampleFromDiscreteDistribution :: RandomNumber -> [(a, Prob)] -> a
sampleFromDiscreteDistribution r o2p = rr -- trace ("sdd: "++(show o2p)++" => "++(show rr)++", ap="++(show ap)++", r'="++(show r')) rr
  where
    rr = fst $ head $ dropWhile (\(o,ap) -> ap<r') o2ap
    r' = ap * r
    (ap,o2ap) = discreteDistribution2accumulatedProbabilities o2p
    discreteDistribution2accumulatedProbabilities o2p = dd2ap 0 o2p
      where
        dd2ap ap [] = (ap,[])
        dd2ap ap ((o,p):o2p) = let (ap',o2p') = dd2ap p' o2p in (ap',(o,p'):o2p') where p'=ap+p


dot2pdf :: FilePath -> IO ExitCode
dot2pdf fp = do
  putStrLn $ "% convert '"++fp++"' via dotty to pdf ..."
  --system ("open \""++fp++"\"")
  system ("dot -Tpdf -O \""++fp++"\"")

show' o = "\""++(escape (show o))++"\""
show'' o = (escape (show o))

indent l = take l $ repeat ' '

escape :: String -> String
escape [] = []
escape ('\\':ss) = "\\"++(escape ss)
escape ('"':ss) = "\\\""++(escape ss)
escape (s:ss) = s:(escape ss)

p1o3 :: (a,b,c) -> a
p1o3 (a,_,_) = a
p2o3 :: (a,b,c) -> b
p2o3 (_,b,_) = b
p3o3 :: (a,b,c) -> c
p3o3 (_,_,c) = c

intercalate' :: (Show a) => String -> [a] -> String
intercalate' d vs = intercalate d (map show vs)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:_) = Just x

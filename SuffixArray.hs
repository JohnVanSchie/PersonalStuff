module SuffixArray where

import Data.Array
import Data.List (isPrefixOf, sort, tails, transpose)
import Debug.Trace

type SuffixArrayEntry = (String,Int)

query :: String -> String -> [SuffixArrayEntry]
query prefix input = takeWhile (isPrefixOf prefix.fst) $ map (suffixInput !) [find suffixInput prefix..]
  where suffixInput = suffixArray input

suffixArray :: String -> Array Int SuffixArrayEntry
suffixArray input = 
  listArray (0, length input - 1) 
    $ (head suffixArray', 0) : zipWith (\x y -> (x, findLcpLength x y)) (tail suffixArray') suffixArray'
  where
    suffixArray' = sort $ init $ tails input

find :: Array Int SuffixArrayEntry -> String -> Int
find arr val  | val <= fst (arr ! minBound)  = minBound
              | val >  fst (arr ! maxBound)  = maxBound + 1
              | otherwise = find' arr val minBound maxBound
  where
    find' arr val minB maxB | (maxB - minB) <= 1  = maxB
                            | otherwise           = let midB = (minB + maxB) `quot` 2 
                                                    in -- trace ("minB/midBif/maxB " ++ show minB ++ "/" ++ show midB ++ "/" ++ show maxB) $
                                                       if val <= fst (arr ! midB)
                                                       then find' arr val minB midB
                                                       else find' arr val midB maxB
    minBound = fst $ bounds arr
    maxBound = snd $ bounds arr


findLcpLength :: String -> String -> Int
findLcpLength x y = length $ findLcp x y

findLcp :: String -> String -> String
findLcp x y = reverse $ findLcp' x y ""
  where 
  findLcp' [] []     acc = acc
  findLcp' (x:xs) [] acc = acc
  findLcp' [] (y:ys) acc = acc
  findLcp' (x:xs) (y:ys) acc 
    | x == y = findLcp' xs ys (x:acc)
    | otherwise = acc

-- Copyright 2013 Dat Le <dat.le@zalora.com>

module Main where 

import NLP.Scores
import Data.List.Split
import Data.List
import Data.Maybe
import Text.Printf
import System.IO
import System.Environment

main = do
  	args <- getArgs
		let country = args !! 0 
		let sku_count = 1 + (read (args !! 1) :: Int)
		let resultFile  = args !! 2 
		let raptorFiles = drop 3 args -- all the remaining are raptors to join, put as much as you want
		raptors <- sequence $ map readFile raptorFiles
		let raptorsCountry = map (raptorForCountry country) raptors
		let raptorCombinedCountry = joinList raptorsCountry
		outh <- openFile (resultFile) WriteMode
		hPutStrLn outh $ intercalate "\n" $ map (\x -> country++'\t': intercalate "\t" (take sku_count x)) $ toRaptor raptorCombinedCountry
		hClose outh

join :: Eq a => [[a]] -> [[a]] -> [[a]]
join (ks1:f1) (ks2:f2) = union ks1 ks2 :
    [k1 : v1++v2 | (k1:v1) <- f1, (k2:v2) <- f2, k1 == k2]
join _        _        = []

joinList :: Eq a => [[[a]]] -> [[a]]
joinList []  = []
joinList [x] = x 
joinList (x:xs)  = joinList $ (join x (head xs)) : (tail xs)  

scoreSKU_to_tuple :: String -> (Float,String)
scoreSKU_to_tuple s | isInfixOf "-" s =	let splits = splitOn "-" s in 
												(read (splits !! 0) :: Float, splits !! 1 )
					| otherwise = (0,"")

tuple_to_scoreSKU :: (Float,String) -> String 
tuple_to_scoreSKU (f,s) = printf "%.2f" (f :: Float) ++ "-" ++ s
					
raptorForCountry :: String -> String -> [[String]]
raptorForCountry country raptor = map tail $ filter (\x -> x!!0 == country ) (map (splitOn "\t") (lines raptor))
--raptorForCountry country raptor = map tail $ filter (\x -> x!!0 == country ) (map (splitOn "\t") (tail $ lines raptor)) -- for the case when Raptors CSV files start with header

toRaptor = map toRaptorByLines 

toRaptorByLines :: [String] -> [String]
toRaptorByLines raptorCombined = filter (/=printf "%.2f" (0 :: Float) ++ "-") $
								 head raptorCombined : -- First SKU to compare
								 (map tuple_to_scoreSKU $ sortBy sortByScore $ combine $ sortBy sortBySKU $ map (\s -> scoreSKU_to_tuple s) $ tail raptorCombined)

combine :: [(Float,String)] -> [(Float,String)]
combine (tuple1:[]) = [tuple1]
combine (tuple1:tuple2:sorted_tuples) | snd tuple1 == snd tuple2 = combine ((fst tuple1 + fst tuple2, snd tuple2) : sorted_tuples)
									  | otherwise = tuple1 : (combine (tuple2 : sorted_tuples))
									  
sortByScore (a1, b1) (a2, b2)
  | a1 < a2 = GT
  | a1 > a2 = LT
  | a1 == a2 = compare b1 b2

sortBySKU (a1, b1) (a2, b2)
  | b1 < b2 = GT
  | b1 > b2 = LT
  | b1 == b2 = compare a1 a2  

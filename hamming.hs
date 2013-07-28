-- Copyright 2013 Dat Le <dat.le@zalora.com>

-- Received metadata and stock info.
-- Output a smart mapping of SKUs to their similar SKUs (which has more stock)

module Main where 

import Data.List.Split
import Data.List
import System.IO
import System.Environment
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST

type DataFrame = [[String]]
type Row = [String]

for_ xs f =  mapM_ f xs

levenshtein :: String -> String -> Int
levenshtein s t = d ! (ls , lt)
    where s' = array (0,ls) [ (i,x) | (i,x) <- zip [0..] s ]::UArray Int Char
          t' = array (0,lt) [ (i,x) | (i,x) <- zip [0..] t ]::UArray Int Char
          ls = length s
          lt = length t
          (l,h) = ((0,0),(length s,length t))
          d = runSTUArray $ do
                m <- newArray (l,h) 0 :: ST s (STUArray s (Int,Int) Int)
                for_ [0..ls] $ \i -> writeArray m (i,0) i
                for_ [0..lt] $ \j -> writeArray m (0,j) j
                for_ [1..lt] $ \j -> do
                              for_ [1..ls] $ \i -> do
                                  let c = if s'!(i-1)==t'! (j-1) 
                                          then 0 else 1
                                  x <- readArray m (i-1,j)
                                  y <- readArray m (i,j-1)
                                  z <- readArray m (i-1,j-1)
                                  writeArray m (i,j) $ minimum [x+1, y+1, z+c ]
                return m

filterBySubcatCatBrand :: Row -> DataFrame -> DataFrame
filterBySubcatCatBrand row df = row : (                             
                              filter (\x -> (row!!2)==(x!!2) && (row!!5)==(x!!5) && (row!!6)==(x!!6))
                              df)


filterBySubcatCatBrandGroup :: DataFrame -> [DataFrame]
filterBySubcatCatBrandGroup [] = []
filterBySubcatCatBrandGroup [row] = [[row]]
filterBySubcatCatBrandGroup (row:rows) = let firstgroup = filterBySubcatCatBrand row rows 
                              in firstgroup : (filterBySubcatCatBrandGroup (minus rows firstgroup))


levenshteinRow :: Row -> DataFrame -> DataFrame
levenshteinRow row df = row : (
                              filter (\x -> levenshtein (row!!0) (x!!0) < 3 ) (
                              filter (\x -> levenshtein (row!!1) (x!!1) < 3 ) (                                
                              filter (\x -> levenshtein (row!!3) (x!!3) < 3 ) (                                
                              filter (\x -> levenshtein (row!!4) (x!!4) < 3 )                              
                              df))))

levenshteinGroup :: DataFrame -> [DataFrame]
levenshteinGroup [] = []
levenshteinGroup [row] = [[row]]
levenshteinGroup (row:rows) = let firstgroup = levenshteinRow row rows 
                              in firstgroup : (levenshteinGroup (minus rows firstgroup))



minus :: Ord a => [a] -> [a] -> [a]
minus [] _ = []
minus xs [] = xs
minus l1@(x:xs) l2@(y:ys)
    | x > y = minus l1 ys
    | x < y = x : minus xs l2
    | otherwise = minus xs l2

main = do
      args <- getArgs
      let country = args !! 0 
      metadata <- readFile $ "Data/sku_metadata_" + country + ".csv"
      let grouped = concat $ map levenshteinGroup $ filterBySubcatCatBrandGroup $ sort $ tail $ toDataFrame metadata
      outh  <- openFile ("Data/grouped.csv") WriteMode
      hPutStrLn outh $ intercalate "\n" $ map (\df -> intercalate " " (map (\row -> row !! 0) df)) grouped
      hClose outh

toDataFrame :: String -> DataFrame
toDataFrame input = map (splitOn "\t") (lines input)


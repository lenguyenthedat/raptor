-- Copyright 2013 Dat Le <dat.le@zalora.com>

module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as DS
import qualified Data.HashMap.Strict as Map

import Data.List.Split
import Data.List
import Data.Char
import System.IO
import System.Environment
import Text.Printf
import Data.Ord

type DataFrame    = [[String]]
type Row          = [String]
type SKU          = String
type Algorithm    = String
type SKU_Distance = String
type USER         = String
type ItemSize     = Int
type SKU_USER_Map = Map.HashMap SKU (DS.Set USER)
type Score = Float


buildSetsFromFile :: [FilePath] -> IO [DS.Set SKU]
buildSetsFromFile files = mapM (\f -> return $ DS.fromList $ indexAsList f) files


buildMapsFromFile :: [FilePath] -> IO ([SKU_USER_Map])
buildMapsFromFile files = mapM (\f -> return $ toMap f) files


toMap :: String -> SKU_USER_Map
toMap input = Map.fromList $ map (\row -> (row !! 0, DS.fromList $ splitOn " " (row !!1))) $ toDataFrame input
-- > toMap "A\t1 2 3 4\nB\t1 2 5 6"
-- fromList [("A",["1","2","3","4"]),("B",["1","2","5","6"])]


indexAsList :: String -> [String]
indexAsList input = map (\row -> row !! 0) $ toDataFrame input
-- > indexAsList "1\t2\t3\n4\t5\t6\n7\t8\t9"
-- ["1","4","7"]


toDataFrame :: String -> DataFrame
toDataFrame input = map (splitOn "\t") (lines input)
-- > toDataFrame "a\tb\n1\t2"
-- [["a","b"],["1","2"]]


-- purchase, view, cart, valid, instock, male, female
getDataDirectories :: String -> String -> [FilePath]
getDataDirectories home country =
  let file_extension = ".csv"
      data_directory_name = "/Data"
      data_types = ["/VTD_purchased_", "/VTD_view_", "/VTD_cart_", "/valid_skus_", "/instock_skus_", "/sku_male_", "/sku_female_"]
  in map (\t -> concat [home, data_directory_name, t, country, file_extension]) data_types


getOutputFilePath :: String -> String -> String -> FilePath
getOutputFilePath home algorithm country = concat [home, "/Result/", algorithm, "/Raptor_", country, ".csv"]


processArgs :: [String] -> (String,Int,String, String) -- country, size, algorithm, home folder
processArgs xs | length xs == 4 = (xs !! 0, read (xs !! 1), xs !! 2, xs !! 3)
               | otherwise      = ("sg"   , 3             , "vtd"  , "./Test") -- Testing / Default value

toStr :: String -> [[((SKU, SKU), Float)]] -> String
toStr country results = unlines [intercalate "\t" $ [(fst . fst . head) result] ++ (map (\((a, b), score) -> (printf "%.2f" score :: String) ++ "-" ++ b) result) | result <- results]

main :: IO ()
main = do
        args  <- getArgs
        let (country, size, algorithm, home) = processArgs args
            data_directories = getDataDirectories home country
            output_filepath = getOutputFilePath home algorithm country
        (purchase_map:view_map:cart_map:_) <- buildMapsFromFile (take 4 data_directories)
        (sku_dst:sku_src:sku_male:sku_female:_) <- buildSetsFromFile (take 4 $ drop 3 data_directories)
        let sku_dst_male = DS.intersection sku_male sku_dst
            sku_dst_female = DS.intersection sku_female sku_dst
            sku_src_male = DS.intersection sku_male sku_src
            sku_src_female = DS.intersection sku_female sku_src
        outh <- openFile output_filepath WriteMode
        let male_result = applyJaccard algorithm size sku_src_male sku_dst_male purchase_map cart_map view_map
            female_result = applyJaccard algorithm size sku_src_female sku_dst_female purchase_map cart_map view_map
        hPutStrLn outh (toStr country male_result)
        hPutStrLn outh (toStr country female_result)
        hClose outh


applyJaccard :: Algorithm -> ItemSize -> DS.Set SKU -> DS.Set SKU -> SKU_USER_Map -> SKU_USER_Map -> SKU_USER_Map -> [[((SKU, SKU), Float)]]
applyJaccard algorithm size sku_src sku_dst purchase_map cart_map view_map = 
  let valid_sku_pairs_list = groupBy (\x y -> fst x == fst y) $ filter (isPairValid algorithm purchase_map cart_map view_map) [(sku1, sku2) | sku1 <- (DS.toList sku_src), sku2 <- (DS.toList sku_dst)]
      jaccard_scores_list = [map (calcJaccardScore algorithm purchase_map cart_map view_map) valid_sku_pairs | valid_sku_pairs <- valid_sku_pairs_list]
      joined_list = [sortBy (flip (comparing snd)) $ zip valid_sku_pairs jaccard_scores | valid_sku_pairs <- valid_sku_pairs_list, jaccard_scores <- jaccard_scores_list]
  in take size joined_list


isPairValid :: Algorithm -> SKU_USER_Map -> SKU_USER_Map -> SKU_USER_Map -> (SKU, SKU) -> Bool
isPairValid algorithm purchase_map cart_map view_map sku_pair =
  let sku1 = fst sku_pair
      sku2 = snd sku_pair
      set_logic = case algorithm of
                        "original" -> not . DS.null $ DS.intersection (safeGetValue cart_map sku2) (safeGetValue cart_map sku1)
                        "bayes" -> not . DS.null $ DS.intersection (safeGetValue purchase_map sku2) (safeGetValue purchase_map sku1)
                        "vtd" -> not . DS.null $ DS.intersection (safeGetValue purchase_map sku2) (safeGetValue view_map sku1)
  in sku1 /= sku2 && set_logic


mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)


safeGetValue :: SKU_USER_Map -> SKU -> DS.Set USER
safeGetValue m k = Map.lookupDefault DS.empty k m


calcJaccardScore :: Algorithm -> SKU_USER_Map -> SKU_USER_Map -> SKU_USER_Map -> (SKU, SKU) -> Float
calcJaccardScore algorithm purchase_map cart_map view_map sku_pair =
  let view_sku_pair = mapTuple2 (safeGetValue view_map) sku_pair
      purchase_sku_pair = mapTuple2 (safeGetValue purchase_map) sku_pair
      cart_sku_pair = mapTuple2 (safeGetValue cart_map) sku_pair

      ints_cart = floatSize $ DS.intersection (fst cart_sku_pair) (snd cart_sku_pair)
      ints_view = floatSize $ DS.intersection (fst view_sku_pair) (snd view_sku_pair)
      ints_purchase = floatSize $ DS.intersection (fst purchase_sku_pair) (snd purchase_sku_pair)
      ints_purchase1_view2 = floatSize $ DS.intersection (fst purchase_sku_pair) (snd view_sku_pair)
      ints_purchase2_view1 = floatSize $ DS.intersection (snd purchase_sku_pair) (fst view_sku_pair)
  in case algorithm of
        "original" -> (wilson95 ints_purchase ((floatSize (fst purchase_sku_pair)) + (floatSize (snd purchase_sku_pair)) - ints_purchase)) 
                      + 0.2 * (wilson95 ints_cart ((floatSize (fst cart_sku_pair)) + (floatSize (snd cart_sku_pair)) - ints_cart))
        "bayes" -> wilson95 ints_purchase (ints_purchase1_view2 + ints_purchase2_view1)
        "vtd" -> wilson95 ints_purchase2_view1 ints_view
  where floatSize = fromIntegral . DS.size


wilson95 :: Float -> Float -> Float
wilson95 0 _ = 0
wilson95 positive negative = 100 * ((positive + 1.9208) / (positive + negative) - 1.96 * sqrt((positive * negative) / (positive + negative) + 0.9604) / (positive + negative)) /  (1 + 3.8416 / (positive + negative))
-- > wilson95 1 2
-- 6.149031527616047
-- > wilson95 1 0
-- 20.654329147389294
-- > wilson95 2 0
-- 34.23719528896193
-- > wilson95 2 4
-- 9.676933255921687
-- > wilson95 2 5
-- 8.221716570901549
-- > wilson95 100 200
-- 28.239255979025565


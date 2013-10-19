-- Copyright 2013 Dat Le <dat.le@zalora.com>
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Set as DS
import qualified Data.HashMap.Strict as Map

import Data.List
import System.IO
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Ord (comparing)

data Algorithm = ORIG | BAYES | VTD

type DataFrame    = [[B.ByteString]]
type SKU          = B.ByteString
type SKU_Distance = B.ByteString
type USER         = B.ByteString
type ItemSize     = Int
type SKU_USER_Map = Map.HashMap SKU (DS.Set USER)
type Score = Float


convertToSourceMap :: DS.Set SKU -> [[((SKU, SKU), Score)]] -> [(SKU, [(SKU, Score)])]
convertToSourceMap sku_src pair_score_lists = 
  let results = [ ( (fst . fst . head $ pair_score_list),  (map (\((_,b), score) -> (b, score)) pair_score_list) ) | pair_score_list <- pair_score_lists]
      empty_elements_set = DS.difference sku_src (DS.fromList (map fst results))
      serialized = serializeEmptyElementSet empty_elements_set
  in sort (results ++ serialized)
  where 
    serializeEmptyElementSet m = map (\x -> (x, [])) (DS.toList m)


applyJaccard :: Algorithm -> ItemSize -> DS.Set SKU -> DS.Set SKU -> SKU_USER_Map -> SKU_USER_Map -> SKU_USER_Map -> [[((SKU, SKU), Score)]]
applyJaccard algorithm size sku_src sku_dst purchase_map cart_map view_map = 
  let valid_sku_pairs_list = groupBy (\x y -> fst x == fst y) $ filter (isPairValid algorithm purchase_map cart_map view_map) [(sku1, sku2) | sku1 <- (DS.toList sku_src), sku2 <- (DS.toList sku_dst)]
      jaccard_scores_list = [map (calcJaccardScore algorithm purchase_map cart_map view_map) valid_sku_pairs | valid_sku_pairs <- valid_sku_pairs_list]
      joined_list = zipWith (\x y -> sortBy (flip (comparing snd)) $ zip x y) valid_sku_pairs_list jaccard_scores_list
  in map (take size) joined_list


isPairValid :: Algorithm -> SKU_USER_Map -> SKU_USER_Map -> SKU_USER_Map -> (SKU, SKU) -> Bool
isPairValid algorithm purchase_map cart_map view_map sku_pair =
  let sku1 = fst sku_pair
      sku2 = snd sku_pair
      set_logic = case algorithm of
                        ORIG -> not . DS.null $ DS.intersection (safeGetValue cart_map sku2) (safeGetValue cart_map sku1)
                        BAYES -> not . DS.null $ DS.intersection (safeGetValue purchase_map sku2) (safeGetValue purchase_map sku1)
                        VTD -> not . DS.null $ DS.intersection (safeGetValue purchase_map sku2) (safeGetValue view_map sku1)
  in sku1 /= sku2 && set_logic


mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (a1, a2) = (f a1, f a2)


safeGetValue :: SKU_USER_Map -> SKU -> DS.Set USER
safeGetValue m k = Map.lookupDefault DS.empty k m


calcJaccardScore :: Algorithm -> SKU_USER_Map -> SKU_USER_Map -> SKU_USER_Map -> (SKU, SKU) -> Score
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
        ORIG -> (wilson95 ints_purchase ((floatSize (fst purchase_sku_pair)) + (floatSize (snd purchase_sku_pair)) - ints_purchase)) 
                      + 0.2 * (wilson95 ints_cart ((floatSize (fst cart_sku_pair)) + (floatSize (snd cart_sku_pair)) - ints_cart))
        BAYES -> wilson95 ints_purchase (ints_purchase1_view2 + ints_purchase2_view1)
        VTD -> wilson95 ints_purchase2_view1 ints_view
  where floatSize = fromIntegral . DS.size


wilson95 :: Float -> Float -> Score
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


buildSetsFromFile :: [FilePath] -> IO [DS.Set SKU]
buildSetsFromFile files = mapM (\f -> do
                                        c <- B.readFile f
                                        return $ DS.fromList $ indexAsList c) files


buildMapsFromFile :: [FilePath] -> IO ([SKU_USER_Map])
buildMapsFromFile files = mapM (\f -> do 
                                        c <- B.readFile f 
                                        return $ toMap c) files


toMap :: B.ByteString -> SKU_USER_Map
toMap input = Map.fromList $ map (\row -> (row !! 0, DS.fromList $ B.split ' ' (row !! 1))) $ toDataFrame input
-- > toMap "A\t1 2 3 4\nB\t1 2 5 6"
-- fromList [("A",["1","2","3","4"]),("B",["1","2","5","6"])]


indexAsList :: B.ByteString -> [B.ByteString]
indexAsList input = map (\row -> row !! 0) $ toDataFrame input
-- > indexAsList "1\t2\t3\n4\t5\t6\n7\t8\t9"
-- ["1","4","7"]


toDataFrame :: B.ByteString -> DataFrame
toDataFrame input = map (B.split '\t') (B.lines input)
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


processArgs :: [String] -> (String, Int, String, String) -- country, size, algorithm, home folder
processArgs xs | length xs == 4 = (xs !! 0, read (xs !! 1), xs !! 2, xs !! 3)
               | otherwise      = ("sg"   , 3             , "vtd"  , "./Test") -- Testing / Default value


toStr :: String -> [(SKU, [(SKU, Score)])] -> String
toStr country results = unlines [intercalate "\t" $ [country, (B.unpack . fst) result] ++ (map (\(b, score) -> (printf "%.2f" score :: String) ++ "-" ++ (B.unpack b)) (snd result)) | result <- results]


main :: IO ()
main = do
        args <- getArgs
        let s@(country, size, algorithm, home) = processArgs args
            data_directories = getDataDirectories home country
            output_filepath = getOutputFilePath home algorithm country
        putStrLn $ "running with options: " ++ show s
        (purchase_map:view_map:cart_map:_) <- buildMapsFromFile (take 4 data_directories)
        (sku_dst:sku_src:sku_male:sku_female:_) <- buildSetsFromFile (take 4 $ drop 3 data_directories)
        let sku_dst_male = DS.intersection sku_male sku_dst
            sku_dst_female = DS.intersection sku_female sku_dst
            sku_src_male = DS.intersection sku_male sku_src
            sku_src_female = DS.intersection sku_female sku_src
            algorithm_type = case algorithm of 
                               "original" -> ORIG
                               "bayes" -> BAYES
                               "vtd" -> VTD
                               _ -> error "Unknown algorithm type"

        outh <- openFile output_filepath WriteMode
        let male_result = convertToSourceMap sku_src_male $ applyJaccard algorithm_type size sku_src_male sku_dst_male purchase_map cart_map view_map
            female_result = convertToSourceMap sku_src_female $ applyJaccard algorithm_type size sku_src_female sku_dst_female purchase_map cart_map view_map
        hPutStr outh (toStr country male_result)
        hPutStr outh (toStr country female_result)
        hClose outh

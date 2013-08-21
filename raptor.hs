-- Copyright 2013 Dat Le <dat.le@zalora.com>

module Main where 
import Data.List.Split
import Data.List
import Data.Char
import System.IO
import System.Environment
import Text.Printf
import Data.Ord
import qualified Data.Map as Map

type DataFrame = [[String]]
type Row = [String]
type SKU = String
type SKU_Distance = String
type USER = String
main = do
	args <- getArgs
	let country = args !! 0 
	let size = read $ args !! 1
	let algorithm = args !! 2
	purchase <- readFile $ "Data/VTD_purchased_" ++ country ++ ".csv" -- SKU - USERS has purchased
	view 	 <- readFile $ "Data/VTD_view_" ++ country ++ ".csv" 	  -- SKU - USERS has viewed
	cart 	 <- readFile $ "Data/VTD_cart_" ++ country ++ ".csv" 	  -- SKU - USERS has put-in-cart
	valid    <- readFile $ "Data/valid_skus_" ++ country ++ ".csv" 	  -- SKUs that has enough stock to be in the list of recommendation
	instock  <- readFile $ "Data/instock_skus_" ++ country ++ ".csv"  -- SKUs that has stock to be calculated
	male     <- readFile $ "Data/sku_male_" ++ country ++ ".csv" 	  -- SKUs that is for male
	female   <- readFile $ "Data/sku_female_" ++ country ++ ".csv" 	  -- SKUs that is for female
	let sku_src = indexAsList instock
	let sku_dst = indexAsList valid
	let sku_male = indexAsList male
	let sku_female = indexAsList female
	let sku_dst_male = intersect sku_male sku_dst
	let sku_dst_female = intersect sku_female sku_dst
	let sku_src_male = intersect sku_male sku_src
	let sku_src_female = intersect sku_female sku_src
	let purchase_map = toMap purchase
	let view_map = toMap view
	let cart_map = toMap cart
	outh <- openFile ("Result/" ++ algorithm ++ "/Raptor_" ++ country ++ ".csv") WriteMode
	case algorithm of
		"original" -> hPutStrLn outh (toStr country (apply_jaccard size sku_src_male sku_dst_male purchase_map cart_map)) >>
		              hPutStrLn outh (toStr country (apply_jaccard size sku_src_female sku_dst_female purchase_map cart_map)) >>
		              hClose outh
		"bayes" -> hPutStrLn outh (toStr country (apply_bayes_jaccard size sku_src_male sku_dst_male purchase_map view_map)) >>
		           hPutStrLn outh (toStr country (apply_bayes_jaccard size sku_src_female sku_dst_female purchase_map view_map)) >>
		           hClose outh
		"vtd" -> hPutStrLn outh (toStr country (apply_vtd_jaccard size sku_src_male sku_dst_male purchase_map view_map)) >>
		         hPutStrLn outh (toStr country (apply_vtd_jaccard size sku_src_female sku_dst_female purchase_map view_map)) >>
				 hClose outh

-- original
apply_jaccard :: Int -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> [(SKU,[(SKU,Float)])]
apply_jaccard size sku_src sku_dst purchase_map cart_map = 
	[ (sku1, apply_jaccard_for_sku size sku1 sku_src sku_dst purchase_map cart_map) | sku1 <- sku_src ]

apply_jaccard_for_sku :: Int -> SKU -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> [(SKU,Float)]
apply_jaccard_for_sku size sku1 sku_src sku_dst purchase_map cart_map = 
	take size $ sortBy (flip (comparing snd)) $ 
	[ (sku2,(jaccard sku1 sku2 purchase_map cart_map)) | sku2 <- filter_related_sku sku1 sku_dst cart_map]

jaccard :: SKU -> SKU -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> Float
jaccard sku1 sku2 purchase_map cart_map = 
	let purchase_sku1 = safeGetValue purchase_map sku1 in
	let purchase_sku2 = safeGetValue purchase_map sku2 in
	let ints_purchase = floatLen (intersect purchase_sku1 purchase_sku2) in
	let cart_sku1 = safeGetValue cart_map sku1 in
	let cart_sku2 = safeGetValue cart_map sku2 in
	let ints_cart = floatLen (intersect cart_sku1 cart_sku2) in
	(wilson95 ints_purchase (floatLen purchase_sku1 + floatLen purchase_sku2 - ints_purchase)) 
	+ 0.2 * (wilson95 ints_cart (floatLen cart_sku1 + floatLen cart_sku2 - ints_cart)) 

-- bayes
apply_bayes_jaccard :: Int -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> [(SKU,[(SKU,Float)])]
apply_bayes_jaccard size sku_src sku_dst purchase_map view_map  = 
	[ (sku1, apply_bayes_jaccard_for_sku size sku1 sku_src sku_dst purchase_map view_map ) | sku1 <- sku_src]

apply_bayes_jaccard_for_sku :: Int -> SKU -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> [(SKU,Float)]
apply_bayes_jaccard_for_sku size sku1 sku_src sku_dst purchase_map view_map = 
	take size $ sortBy (flip (comparing snd)) $ 
	[ (sku2,(bayes_jaccard sku1 sku2 purchase_map view_map )) | sku2 <- filter_related_sku sku1 sku_dst purchase_map ]

bayes_jaccard :: SKU -> SKU -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> Float
bayes_jaccard sku1 sku2 purchase_map view_map 
	| floatLen (intersect (safeGetValue purchase_map sku1) (safeGetValue purchase_map sku2)) == 0 = 0 
	| otherwise = let purchase_sku1 = safeGetValue purchase_map sku1 in
				  let purchase_sku2 = safeGetValue purchase_map sku2 in
				  let view_sku1     = safeGetValue view_map sku1 in
				  let view_sku2     = safeGetValue view_map sku2 in
				  let ints_view     = floatLen (intersect view_sku1 view_sku2) in
				  let ints_purchase = floatLen (intersect purchase_sku1 purchase_sku2) in
				  let ints_purchase1_view2 = floatLen (intersect purchase_sku1 view_sku2) in
				  let ints_purchase2_view1 = floatLen (intersect purchase_sku2 view_sku1) in
				  wilson95 ints_purchase (ints_purchase1_view2 + ints_purchase2_view1)

-- vtd
apply_vtd_jaccard :: Int -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> [(SKU,[(SKU,Float)])]
apply_vtd_jaccard size sku_src sku_dst purchase_map view_map  = 
	[ (sku1, apply_vtd_jaccard_for_sku size sku1 sku_src sku_dst purchase_map view_map ) | sku1 <- sku_src]

apply_vtd_jaccard_for_sku :: Int -> SKU -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> [(SKU,Float)]
apply_vtd_jaccard_for_sku size sku1 sku_src sku_dst purchase_map view_map = 
	take size $ sortBy (flip (comparing snd)) $ 
	[ (sku2,(vtd_jaccard sku1 sku2 purchase_map view_map )) | sku2 <- filter_related_sku_vtd sku1 sku_dst purchase_map view_map ]

vtd_jaccard :: SKU -> SKU -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> Float
vtd_jaccard sku1 sku2 purchase_map view_map = let view_sku1 	  = safeGetValue view_map sku1 in
												let view_sku2 	  = safeGetValue view_map sku2 in
												let purchase_sku2 = safeGetValue purchase_map sku2 in
				 							    let ints_purchase2_view1 = floatLen (intersect purchase_sku2 view_sku1) in
				 							    let ints_view            = floatLen (intersect view_sku1 view_sku2) in
				 							    wilson95 ints_purchase2_view1 ints_view

-- auxilary functions
-- given an SKU, a list of SKUs to be fitered, a purchase_map
-- output list of SKUs that has at least one or more purchases together with the given SKU
filter_related_sku :: SKU -> [SKU] -> (Map.Map SKU [USER]) ->[SKU]
filter_related_sku sku sku_dst purchase_map = 
	filter (\other_sku -> other_sku /= sku && 
					       length (intersect (safeGetValue purchase_map other_sku) (safeGetValue purchase_map sku)) > 0) sku_dst

filter_related_sku_vtd :: SKU -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) ->[SKU]
filter_related_sku_vtd sku sku_dst purchase_map view_map = 
	filter (\other_sku -> other_sku /= sku && 
					       length (intersect (safeGetValue purchase_map other_sku) (safeGetValue view_map sku)) > 0) sku_dst

floatLen :: [String] -> Float
floatLen s =  fromIntegral $ length $ s

safeGetValue :: Ord a => (Map.Map a [a]) -> a -> [a]
safeGetValue purchase_map sku | Map.member sku purchase_map = purchase_map Map.! sku
						   	  | otherwise = []

toMap :: String -> Map.Map SKU [USER]
toMap input = Map.fromList $ map (\row -> (row !! 0, splitOn " " (row !!1))) $ toDataFrame input
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

toStr :: String -> [(SKU,[(SKU,Float)])] -> String
toStr country result = intercalate "\n" $ map (\(sku,skus) -> country ++ "\t" ++ sku ++ "\t" ++ intercalate "\t" (map (\(sku,score) -> (printf "%.2f" score :: String) ++ "-" ++ sku) skus)) result 
-- > toStr "SG" [("sku1",[("sku2",2.1),("sku3",1.2),("sku4",1.0)]),("sku2",[("sku1",2.2)]),("sku3",[("sku4",1)])]
-- "SG\tsku1\t2.10-sku2\t1.20-sku3\t1.00-sku4\nSG\tsku2\t2.20-sku1\nSG\tsku3\t1.00-sku4"

wilson95 :: Float -> Float -> Float
wilson95 0 0 = 0
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

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

  	let si		args <- getArgs
		let country = args !! 0 ze = read $ args !! 1
		purchase <- readFile $ "Data/VTD_purchased_" ++ country ++ ".csv"
		view 	 <- readFile $ "Data/VTD_view_" ++ country ++ ".csv"
		valid    <- readFile $ "Data/valid_skus_" ++ country ++ ".csv"
		instock  <- readFile $ "Data/instock_skus_" ++ country ++ ".csv"
		male     <- readFile $ "Data/sku_male_" ++ country ++ ".csv"
		female   <- readFile $ "Data/sku_female_" ++ country ++ ".csv"
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
		outh <- openFile ("Result/Raptor_" ++ country ++ ".csv") WriteMode
		--hPutStrLn outh $ toStr country $ apply_jaccard size sku_src_male sku_dst_male purchase_map 
		--hPutStrLn outh $ toStr country $ apply_jaccard size sku_src_female sku_dst_female purchase_map 
		--hPutStrLn outh $ toStr country $ apply_bayes_jaccard size sku_src_male sku_dst_male purchase_map view_map 
		--hPutStrLn outh $ toStr country $ apply_bayes_jaccard size sku_src_female sku_dst_female purchase_map view_map 
		hPutStrLn outh $ toStr country $ apply_views_jaccard size sku_src_male sku_dst_male purchase_map view_map 
		hPutStrLn outh $ toStr country $ apply_views_jaccard size sku_src_female sku_dst_female purchase_map view_map 
		hClose outh

-- Normal Jaccard
apply_jaccard :: Int -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> [(SKU,[(SKU,Float)])]
apply_jaccard size sku_src sku_dst purchase_map  = 
	[ (sku1, apply_jaccard_for_sku size sku1 sku_src sku_dst purchase_map ) | sku1 <- sku_src ]

apply_jaccard_for_sku :: Int -> SKU -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> [(SKU,Float)]
apply_jaccard_for_sku size sku1 sku_src sku_dst purchase_map  = 
	take size $ sortBy (flip (comparing snd)) $ 
	[ (sku2,(jaccard sku1 sku2 purchase_map )) | sku2 <- filter_related_sku sku1 sku_dst purchase_map]

jaccard :: SKU -> SKU -> (Map.Map SKU [USER]) -> Float
jaccard sku1 sku2 purchase_map  = 
	let purchase_sku1 = safeGetValue purchase_map sku1 in
	let purchase_sku2 = safeGetValue purchase_map sku2 in
	let ints_purchase = floatLen (intersect purchase_sku1 purchase_sku2) in
	ints_purchase / (floatLen purchase_sku1 + floatLen purchase_sku2 - ints_purchase)

-- Bayes Jaccard
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
				  ints_purchase / (ints_purchase1_view2 + ints_purchase2_view1 + ints_purchase)

-- View Jaccard
apply_views_jaccard :: Int -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> [(SKU,[(SKU,Float)])]
apply_views_jaccard size sku_src sku_dst purchase_map view_map  = 
	[ (sku1, apply_views_jaccard_for_sku size sku1 sku_src sku_dst purchase_map view_map ) | sku1 <- sku_src]

apply_views_jaccard_for_sku :: Int -> SKU -> [SKU] -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> [(SKU,Float)]
apply_views_jaccard_for_sku size sku1 sku_src sku_dst purchase_map view_map = 
	take size $ sortBy (flip (comparing snd)) $ 
	[ (sku2,(views_jaccard sku1 sku2 purchase_map view_map )) | sku2 <- filter_related_sku_view sku1 sku_dst purchase_map view_map ]

views_jaccard :: SKU -> SKU -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) -> Float
views_jaccard sku1 sku2 purchase_map view_map = let view_sku1 	  = safeGetValue view_map sku1 in
												let view_sku2 	  = safeGetValue view_map sku2 in
												let purchase_sku2 = safeGetValue purchase_map sku2 in
				 							    let ints_purchase2_view1 = floatLen (intersect purchase_sku2 view_sku1) in
				 							    let ints_view            = floatLen (intersect view_sku1 view_sku2) in
				 							    ints_purchase2_view1 / (ints_view + ints_purchase2_view1)

-- auxilary functions
-- given an SKU, a list of SKUs to be fitered, a purchase_map
-- output list of SKUs that has at least one or more purchases together with the given SKU
filter_related_sku :: SKU -> [SKU] -> (Map.Map SKU [USER]) ->[SKU]
filter_related_sku sku sku_dst purchase_map = 
	filter (\other_sku -> other_sku /= sku && 
					       length (intersect (safeGetValue purchase_map other_sku) (safeGetValue purchase_map sku)) > 0) sku_dst

filter_related_sku_view :: SKU -> [SKU] -> (Map.Map SKU [USER]) -> (Map.Map SKU [USER]) ->[SKU]
filter_related_sku_view sku sku_dst purchase_map view_map = 
	filter (\other_sku -> other_sku /= sku && 
					       length (intersect (safeGetValue purchase_map other_sku) (safeGetValue view_map sku)) > 0) sku_dst

floatLen :: [String] -> Float
floatLen s =  fromIntegral $ length $ s

safeGetValue :: Ord a => (Map.Map a [a]) -> a -> [a]
safeGetValue purchase_map sku | Map.member sku purchase_map = purchase_map Map.! sku
						   	  | otherwise = []

toMap :: String -> Map.Map SKU [USER]
toMap input = Map.fromList $ map (\row -> (row !! 0, splitOn " " (row !!1))) $ toDataFrame input

indexAsList :: String -> [String]
indexAsList input = tail $ map (\row -> row !! 0) $ toDataFrame input

toDataFrame :: String -> DataFrame
toDataFrame input = map (splitOn "\t") (lines input)

toStr :: String ->  [(SKU,[(SKU,Float)])] -> String
toStr country result = intercalate "\n" $ map (\(sku,skus) -> country ++ "\t" ++ sku ++ "\t" ++ intercalate "\t" (map (\(sku,score) -> (printf "%.2f" score :: String) ++ "-" ++ sku) skus)) result 

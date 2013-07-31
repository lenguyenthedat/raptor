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
type ITEM = String
type ITEM_Distance = String
type USER = String

sep = "\t"

main = do
		args <- getArgs
		let country = args !! 0 
		let size = read $ args !! 1
		let algorithm = args !! 2
		purchase <- readFile $ "Data/VTD_purchased_" ++ country ++ ".csv"  -- ITEM - USERS has purchased
		view 	 <- readFile $ "Data/VTD_view_" ++ country ++ ".csv"       -- ITEM - USERS has viewed
		cart 	 <- readFile $ "Data/VTD_cart_" ++ country ++ ".csv"       -- ITEM - USERS has put-in-cart
		valid    <- readFile $ "Data/valid_items_" ++ country ++ ".csv"    -- ITEMS that has enough stock to be in the list of recommendation
		instock  <- readFile $ "Data/instock_items_" ++ country ++ ".csv"  -- ITEMS that has stock to be calculated
		male     <- readFile $ "Data/item_male_" ++ country ++ ".csv"      -- ITEMS that is for male
		female   <- readFile $ "Data/item_female_" ++ country ++ ".csv"    -- ITEMS that is for female
		let item_src = indexAsList instock
		let item_dst = indexAsList valid
		let item_male = indexAsList male
		let item_female = indexAsList female
		let item_dst_male = intersect item_male item_dst
		let item_dst_female = intersect item_female item_dst
		let item_src_male = intersect item_male item_src
		let item_src_female = intersect item_female item_src
		let purchase_map = toMap purchase
		let view_map = toMap view
		let cart_map = toMap cart
		outh <- openFile ("Result/" ++ algorithm ++ "/Raptor_" ++ country ++ ".csv") WriteMode
		case algorithm of
			"original" -> hPutStrLn outh (toStr country (apply_jaccard size item_src_male item_dst_male purchase_map cart_map)) >>
			              hPutStrLn outh (toStr country (apply_jaccard size item_src_female item_dst_female purchase_map cart_map)) >>
			              hClose outh
			"bayes" -> hPutStrLn outh (toStr country (apply_bayes_jaccard size item_src_male item_dst_male purchase_map view_map)) >>
			           hPutStrLn outh (toStr country (apply_bayes_jaccard size item_src_female item_dst_female purchase_map view_map)) >>
			           hClose outh
			"vtd" -> hPutStrLn outh (toStr country (apply_vtd_jaccard size item_src_male item_dst_male purchase_map view_map)) >>
			         hPutStrLn outh (toStr country (apply_vtd_jaccard size item_src_female item_dst_female purchase_map view_map)) >>
					 hClose outh

-- original
apply_jaccard :: Int -> [ITEM] -> [ITEM] -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) -> [(ITEM,[(ITEM,Float)])]
apply_jaccard size item_src item_dst purchase_map cart_map = 
	[ (item1, apply_jaccard_for_item size item1 item_src item_dst purchase_map cart_map) | item1 <- item_src ]

apply_jaccard_for_item :: Int -> ITEM -> [ITEM] -> [ITEM] -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) -> [(ITEM,Float)]
apply_jaccard_for_item size item1 item_src item_dst purchase_map cart_map = 
	take size $ sortBy (flip (comparing snd)) $ 
	[ (item2,(jaccard item1 item2 purchase_map cart_map)) | item2 <- filter_related_item item1 item_dst cart_map]

jaccard :: ITEM -> ITEM -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) -> Float
jaccard item1 item2 purchase_map cart_map = 
	let purchase_item1 = safeVal purchase_map item1 in
	let purchase_item2 = safeVal purchase_map item2 in
	let ints_purchase = floatLen (intersect purchase_item1 purchase_item2) in
	let cart_item1 = safeVal cart_map item1 in
	let cart_item2 = safeVal cart_map item2 in
	let ints_cart = floatLen (intersect cart_item1 cart_item2) in
	(wilson95 ints_purchase (floatLen purchase_item1 + floatLen purchase_item2 - ints_purchase)) 
	+ 0.2 * (wilson95 ints_cart (floatLen cart_item1 + floatLen cart_item2 - ints_cart)) 

-- bayes
apply_bayes_jaccard :: Int -> [ITEM] -> [ITEM] -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) -> [(ITEM,[(ITEM,Float)])]
apply_bayes_jaccard size item_src item_dst purchase_map view_map  = 
	[ (item1, apply_bayes_jaccard_for_item size item1 item_src item_dst purchase_map view_map ) | item1 <- item_src]

apply_bayes_jaccard_for_item :: Int -> ITEM -> [ITEM] -> [ITEM] -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) -> [(ITEM,Float)]
apply_bayes_jaccard_for_item size item1 item_src item_dst purchase_map view_map = 
	take size $ sortBy (flip (comparing snd)) $ 
	[ (item2,(bayes_jaccard item1 item2 purchase_map view_map )) | item2 <- filter_related_item item1 item_dst purchase_map ]

bayes_jaccard :: ITEM -> ITEM -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) -> Float
bayes_jaccard item1 item2 purchase_map view_map 
	| floatLen (intersect (safeVal purchase_map item1) (safeVal purchase_map item2)) == 0 = 0 
	| otherwise = let purchase_item1 = safeVal purchase_map item1 in
				  let purchase_item2 = safeVal purchase_map item2 in
				  let view_item1     = safeVal view_map item1 in
				  let view_item2     = safeVal view_map item2 in
				  let ints_view     = floatLen (intersect view_item1 view_item2) in
				  let ints_purchase = floatLen (intersect purchase_item1 purchase_item2) in
				  let ints_purchase1_view2 = floatLen (intersect purchase_item1 view_item2) in
				  let ints_purchase2_view1 = floatLen (intersect purchase_item2 view_item1) in
				  wilson95 ints_purchase (ints_purchase1_view2 + ints_purchase2_view1)

-- vtd
apply_vtd_jaccard :: Int -> [ITEM] -> [ITEM] -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) -> [(ITEM,[(ITEM,Float)])]
apply_vtd_jaccard size item_src item_dst purchase_map view_map  = 
	[ (item1, apply_vtd_jaccard_for_item size item1 item_src item_dst purchase_map view_map ) | item1 <- item_src]

apply_vtd_jaccard_for_item :: Int -> ITEM -> [ITEM] -> [ITEM] -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) -> [(ITEM,Float)]
apply_vtd_jaccard_for_item size item1 item_src item_dst purchase_map view_map = 
	take size $ sortBy (flip (comparing snd)) $ 
	[ (item2,(vtd_jaccard item1 item2 purchase_map view_map )) | item2 <- filter_related_item_vtd item1 item_dst purchase_map view_map ]

vtd_jaccard :: ITEM -> ITEM -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) -> Float
vtd_jaccard item1 item2 purchase_map view_map = let view_item1 	  = safeVal view_map item1 in
												let view_item2 	  = safeVal view_map item2 in
												let purchase_item2 = safeVal purchase_map item2 in
				 							    let ints_purchase2_view1 = floatLen (intersect purchase_item2 view_item1) in
				 							    let ints_view            = floatLen (intersect view_item1 view_item2) in
				 							    wilson95 ints_purchase2_view1 ints_view

-- auxilary functions
-- given an ITEM, a list of ITEMs to be fitered, a purchase_map
-- output list of ITEMs that has at least one or more purchases together with the given ITEM
filter_related_item :: ITEM -> [ITEM] -> (Map.Map ITEM [USER]) ->[ITEM]
filter_related_item item item_dst purchase_map = 
	filter (\other_item -> other_item /= item && 
					       length (intersect (safeVal purchase_map other_item) (safeVal purchase_map item)) > 0) item_dst

filter_related_item_vtd :: ITEM -> [ITEM] -> (Map.Map ITEM [USER]) -> (Map.Map ITEM [USER]) ->[ITEM]
filter_related_item_vtd item item_dst purchase_map view_map = 
	filter (\other_item -> other_item /= item && 
					       length (intersect (safeVal purchase_map other_item) (safeVal view_map item)) > 0) item_dst

floatLen :: [String] -> Float
floatLen s =  fromIntegral $ length $ s

safeVal :: Ord a => (Map.Map a [a]) -> a -> [a]
safeVal map key | Map.member key map = map Map.! key
                | otherwise = []

toMap :: String -> Map.Map ITEM [USER]
toMap input = Map.fromList $ map (\row -> (row !! 0, splitOn " " (row !!1))) $ toDataFrame sep input

indexAsList :: String -> [String]
indexAsList input = tail $ map (\row -> row !! 0) $ toDataFrame sep input

toDataFrame :: String -> String -> DataFrame
toDataFrame input sep = map (splitOn sep) (lines input)

toStr :: String ->  [(ITEM,[(ITEM,Float)])] -> String
toStr country result = intercalate "\n" $ map (\(item,items) -> country ++ sep ++ item ++ sep ++ intercalate sep (map (\(item,score) -> (printf "%.2f" score :: String) ++ "-" ++ item) items)) result 

wilson95 :: Float -> Float -> Float
wilson95 0 0 = 0
wilson95 positive negative = 100 * ((positive + 1.9208) / (positive + negative) - 1.96 * sqrt((positive * negative) / (positive + negative) + 0.9604) / (positive + negative)) /  (1 + 3.8416 / (positive + negative))

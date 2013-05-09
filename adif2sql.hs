import ADIF

import Data.Map hiding (null, map)
import Data.List (intersperse)
import Data.Maybe
import System.Environment

project :: (Ord k1, Ord k2) => Map k1 k2 -> Map k1 a -> [(k2,a)]
project m = toList . mapKeys (m !) . filterWithKey (const . flip member m)   

escape s = "'" ++ concatMap f s ++ "'" where
	f '\\' = "\\\\"
	f '\'' = "\'\'"
	f o    = [o]

geninsert :: String -> Map String String -> String
geninsert tbn pairs = "insert into " ++ tbn ++ "(" 
                    ++ concat (intersperse "," keys)
                    ++ ") values (" ++ concat (intersperse "," vals)
                    ++ ");\n" where
	(keys,vals) = unzip (toList pairs)

year = take 4
month = take 2 . drop 4
day = take 2 . drop 6

group n = map (take n) . takeWhile (not.null) . iterate (drop n)

numeric = all (`elem` ['0'..'9'])


transform "band" s = Just ("band", escape s)
transform "call" s = Just ("call", escape s)
transform "mode" s = Just ("mode", escape s)
transform "name" s = Just ("name", escape s)
transform "qso_date" s = Just ("qso_date", escape $ 
			year s ++ "-" ++ month s ++ "-" ++ day s)
transform "time_on" s = Just ("time_on", escape (
		concat . intersperse ":" . group 2 $ s))
transform "rst_sent" s      | numeric s = Just ("rst_sent", s)
transform "rst_rcvd" s  | numeric s = Just ("rst_rcvd", s)
transform _ _ = Nothing

transformMap = fromList . catMaybes . map (uncurry transform) . toList

qso2sql id = concatMap (geninsert "qso" . insert "id_upload" id . transformMap)

main = do
	args <- getArgs
	case args of
		[list_id, file] -> do
			Right qso <- adifFromFile file
			putStr (qso2sql list_id qso)

		_ -> do putStrLn "Usage: adif2sql <list_id> <adif_file>"
		        return ()
			

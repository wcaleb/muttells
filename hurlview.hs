import Network.URI (isURI)
import Data.List (isInfixOf, elemIndex, zip3)
import System.IO

parseLines :: String -> [(Int, Bool, String)]
parseLines s = zip3 [1..] hasURL (lines s)
   where hasURL = foldr (\x xs -> if any isURI $ words x then True:xs else False:xs) [] (lines s)

sp :: Int -> String
sp n = concat $ take n $ repeat " "

numberLines :: [(Int, Bool, String)] -> [String]
numberLines [] = []
numberLines ((x,y,z):ts) = case y of
                              True -> (sp 2 ++ show x ++ sp 2 ++ z):numberLines ts
                              False -> (sp 5 ++ z):numberLines ts

getURL :: String -> Int -> [String]
getURL s n = foldr (\x xs -> if isURI x then x:xs else xs) [] (words $ lines s !! n)

main :: IO ()
main = do 
   contents <- getContents
   let listurls = parseLines contents
   let showurls = unlines (numberLines listurls)
   putStrLn showurls
   tty <- openFile "/dev/tty" ReadMode
   putStrLn "Which link do you want?"
   choice <- hGetLine tty
   let n = read choice :: Int
   hClose tty
   putStrLn $ "You chose " ++ unlines (getURL contents (n-1))

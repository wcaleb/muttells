import Network.URI (isURI)
import System.IO

type Line = String
type LineNumber = Int
type HasURL = Bool
type Lines = [(LineNumber, HasURL, Line)]

parseLines :: String -> Lines
parseLines s = zip3 [1..] checkURL (lines s)
   where checkURL = foldr (\x xs -> if any isURI $ words x then True:xs else False:xs) [] (lines s)

sp :: Int -> String
sp n = concat $ replicate n " "

-- Prints out Lines, numbering those that have a URL 
numberLines :: Lines -> [String]
numberLines [] = []
numberLines ((n,u,l):ts) = if u
                    then (sp 2 ++ show n ++ sp 2 ++ l):numberLines ts
                    else (sp 5 ++ l):numberLines ts

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

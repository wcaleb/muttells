import Data.Char (isSpace)
import Data.List.Split (splitOneOf)
import Network.URI (isURI)
import System.IO

type Line = String
type LineNumber = Int
type HasURL = Bool
type Lines = [(LineNumber, HasURL, Line)]

sp :: Int -> String
sp n = concat $ replicate n " "

isURI' :: String -> Bool
isURI' x = isURI x && (last x /= ':')

wrapLines :: String -> String
wrapLines s = unlines $ map trim $ concatMap (wrapLine 78) (lines s)

-- Builds Lines out of raw strings
parseLines :: String -> Lines
parseLines s = zip3 [1..] checkURL (lines s)
   where checkURL = foldr (\x xs -> if any isURI' $ splitOneOf " \"<>()" x
                                       then True:xs
                                       else False:xs) [] (lines s)

-- Number lines that have a URL
numberLines :: Lines -> [String]
numberLines [] = []
numberLines ((n,u,l):ts) = if u
                    then (sp 2 ++ show n ++ sp 2 ++ l):numberLines ts
                    else (sp 5 ++ l):numberLines ts

getURL :: String -> Int -> [String]
getURL s n = foldr (\x xs -> if isURI' x then x:xs else xs) [] (splitOneOf " \"<>()" $ lines s !! n)

-- Word wrapping functions
trim :: String -> String
trim = trimAndReverse . trimAndReverse
  where trimAndReverse = reverse . dropWhile isSpace
 
reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
  where (after, before) = break f $ reverse xs
 
wrapLine :: Int -> String -> [String]
wrapLine maxLen line 
  | length line <= maxLen  = [line]
  | any isSpace beforeMax  = beforeSpace : wrapLine maxLen (afterSpace ++ afterMax)
  | otherwise              = beforeMax : wrapLine maxLen afterMax
    where (beforeMax, afterMax) = splitAt maxLen line
          (beforeSpace, afterSpace) = reverseBreak isSpace beforeMax

main :: IO ()
main = do 
   contents <- getContents
   let contents' = wrapLines contents
   let parsedLines = parseLines contents'
   putStrLn $ unlines (numberLines parsedLines)
   tty <- openFile "/dev/tty" ReadMode
   putStrLn "Which link do you want?"
   choice <- hGetLine tty
   let n = read choice :: Int
   hClose tty
   putStrLn $ "You chose " ++ unlines (getURL contents' (n-1))

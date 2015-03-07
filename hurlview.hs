import Data.Char (isSpace)
import Data.List.Split (splitOneOf)
import Network.URI (isURI)
import System.IO
import System.Process

type Line = String
type LineNumber = Int
type HasURL = Bool
type Lines = [(LineNumber, HasURL, Line)]

sp :: Int -> String
sp n = concat $ replicate n " "

-- Make isURI less greedy by throwing away bare schemes
isURI' :: String -> Bool
isURI' x = isURI x && (last x /= ':')

-- Split words on spaces and punctuation that might surround a URL
words' :: String -> [String]
words' = splitOneOf " \"<>()"

-- Builds Lines out of raw strings
parseLines :: [String] -> Lines
parseLines s = zip3 [1..] checkURL s
   where checkURL = foldr (\x xs -> if any isURI' $ words' x
                                       then True:xs
                                       else False:xs) [] s

-- Number lines that have a URL
numberLines :: Lines -> [String]
numberLines [] = []
numberLines ((n,u,l):ts) = if u
                    then (sp (marg n) ++ show n ++ " -> " ++ l):numberLines ts
                    else (sp (marg n) ++ sp (4 - marg n) ++ sp 4 ++ l):numberLines ts
            where marg x | x > 9 = 2 | x > 99 = 1 | otherwise = 3

-- Word wrapping functions
-- Adapted from https://gist.github.com/moreindirection/524460
trim :: String -> String
trim = trimAndReverse . trimAndReverse
  where trimAndReverse = reverse . dropWhile isSpace
 
reverseBreak :: (a -> Bool) -> [a] -> ([a], [a])
reverseBreak f xs = (reverse before, reverse after)
  where (after, before) = break f $ reverse xs
 
wrapLine :: String -> [String]
wrapLine line 
  | length line <= maxLen  = [line]
  | any isSpace beforeMax  = beforeSpace : wrapLine (afterSpace ++ afterMax)
  | otherwise              = [line]
    where (beforeMax, afterMax) = splitAt maxLen line
          (beforeSpace, afterSpace) = reverseBreak isSpace beforeMax
          maxLen = 78

wrapLines :: String -> [String]
wrapLines s = map trim $ concatMap wrapLine (lines s)

-- Retrieve URL(s) from a line
getURL :: [String] -> Int -> [String]
getURL s n = foldr (\x xs -> if isURI' x then x:xs else xs) [] (words' $ s !! n)

main :: IO ()
main = do 
   contents <- getContents
   let wrappedLines = wrapLines contents
   let parsedLines = parseLines wrappedLines
   putStrLn $ unlines (numberLines parsedLines)
   tty <- openFile "/dev/tty" ReadMode
   putStrLn "Which link do you want?"
   choice <- hGetLine tty
   let n = read choice :: Int
   hClose tty
   putStrLn $ "You chose " ++ unlines (getURL wrappedLines (n-1))
   callCommand ("open \"" ++ init (unlines (getURL wrappedLines (n-1))) ++ "\"")

-- TODO: capturing the output of `tput lines` and `tput cols` to make pager for output

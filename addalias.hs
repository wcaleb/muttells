import Muttells
import Control.Monad
import Data.Char
import Data.List
import System.Environment

main :: IO ()
main = do
    message <- getContents
    filepath <- getArgs
    case filepath of
      [aliasfile] -> do
         let newalias = fromToAlias message ++ "\n"
         aliases <- readFile aliasfile
         unless (newalias `isInfixOf` aliases) $ appendFile aliasfile newalias 
         putStr message
      _ -> putStr message

-- Take list of header strings and return value of "From:" field
fromValue :: [String] -> String
fromValue = dropWhile (/= ' ') . head . filter (isPrefixOf "From: ")

-- Turn the value of the "From:" header field into an alias
makeAlias :: String -> String
makeAlias s 
    | length w == 1 = "alias" ++ l s ++ s
    | length w == 2 = "alias" ++ l (head w) ++ s
    | last (head w) == ',' = "alias " ++ l (init $ head w) ++ '-' : l (head $ tail w) ++ s
    | otherwise = "alias " ++ l (last $ init w) ++ '-' : l (head w) ++ s
    where w = words (filter (/= '"') s)
          l = map toLower

fromToAlias :: String -> String
fromToAlias = checkAlias . makeAlias . fromValue . getHeaders 

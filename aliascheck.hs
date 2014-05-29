import Data.List (sort)
import Muttells

main :: IO ()
main = interact (unlines . sort . map checkAlias . lines)

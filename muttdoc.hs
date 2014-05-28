
main :: IO ()
main = interact markdownDoc

headerToYaml :: String -> String
headerToYaml =  unlines . (:) "---" . quoteVals . breakVals . takeWhile (/= []) . lines 

breakVals = map (break (== ' '))

quoteVals xs = [ a ++ " " ++ ('\'':b ++ "\'") | (a, b) <- xs]

getBody :: String -> String
getBody = unlines . dropWhile (/= []) . lines

markdownDoc :: String -> String
markdownDoc s = headerToYaml s ++ "...\n\n" ++ "```" ++ getBody s ++ "```\n"

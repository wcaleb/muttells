import Muttells

--- UNFINISHED CODE
--- Goal is to prepare a message for pretty printing, a la muttprint
--- Right now, this basically munges message into Markdown suitable
--- for processing by Pandoc with an email template

main :: IO ()
main = interact markdownDoc

headerToYaml :: String -> String
headerToYaml =  unlines . (:) "---" . quoteVals . breakVals . getHeaders

breakVals = map (break (== ' '))

quoteVals xs = [ a ++ " " ++ "|\n" ++ b | (a, b) <- xs]


markdownDoc :: String -> String
markdownDoc s = headerToYaml s ++ "...\n\n" ++ "```" ++ getBody s ++ "```\n"

-- Take a list of header strings, get the "From:" one, and drop "From: " with breakVals
-- extractFrom = snd . head . breakVals . filter (isPrefixOf "From: ")
-- 
-- makeAlias :: String -> String
-- makeAlias s 
--     | length w == 1 = "alias " ++ l s ++ ' ' : s
--     | length w == 2 = "alias " ++ l (head w) ++ ' ' : s
--     | last (head w) == ',' = "alias " ++ l (init $ head w) ++ '-' : l (head $ tail w) ++ ' ' : s
--     | otherwise = "alias " ++ l (last $ init w) ++ '-' : l (head w) ++ ' ' : s
--     where w = words (filter (/= '"') s)
--           l = map toLower

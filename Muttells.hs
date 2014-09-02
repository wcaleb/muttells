module Muttells
( checkAlias
, getHeaders
, getBody
) where

import Text.Parsec 
import Data.Maybe (maybeToList)
import Data.List (intercalate, isInfixOf)

type Parser t s = Parsec t s

------------------------
-- Mutt email parsers --
------------------------

getHeaders :: String -> [String]
getHeaders = takeWhile (/= []) . lines

-- Will eventually accounts for headers that span multiple lines
-- joinMultiLineHeaders [] = []
-- joinMultiLineHeaders (x:xs)
--     | " " `isPrefixOf` head xs = (x ++ head xs) : (tail xs)
--     | "\t" `isPrefixOf` head xs = (x ++ head xs) : (tail xs)
--     | otherwise = x : joinMultiLineHeaders xs

getBody :: String -> String
getBody = unlines . dropWhile (/= []) . lines

------------------------
-- Mutt alias parsers --
------------------------

stopWords :: [String]
stopWords = [ "notification", "notifications", "do-not-reply", "no-reply", "noreply", "donotreply" ]

checkAlias :: String -> String
checkAlias input = case parse validLine "" input of 
    Left _ -> '#':input
    Right _ -> if any (`isInfixOf` input) stopWords then '#':input else input
    -- NB: if you don't want to use stopwords, use "Right _ -> input" instead
    where validLine = try comment <|> try groupAlias <|> validAlias
preAlias, validAlias, comment, groupAlias :: Parser String st String

preAlias = string "alias " >> manyTill anyChar space

comment = string "#" >> many (noneOf "\n")

groupAlias = do 
    _ <- preAlias >> nickname >> nicknameSep 
    _ <- sepBy nickname nicknameSep
    return "valid group alias"
    where
       nickname = many (noneOf " ,\n")
       nicknameSep = skipMany (char ' ') >> char ',' >> skipMany (char ' ')

validAlias = do
    _ <- preAlias >>
     manyTill (noneOf "<>\n") (try ( angEmail <|> emailAddress ))
    notFollowedBy anyToken <?> "end of input"
    return "valid single alias"

---------------------------
-- Email address parsers --
---------------------------

-- Using modified old emailAddress parser from Pandoc
-- https://github.com/jgm/pandoc/blob/a71641a2a04c1d324163e16299f1e9821a26c9f9/src/Text/Pandoc/Parsing.hs

angEmail :: Parser String st String
angEmail = do
    _ <- char '<' >> emailAddress >> char '>'
    return "bracketed email address"

emailChar :: Parser String st Char
emailChar = alphaNum <|> oneOf "!\"#$%&'*+-/0123456789=?^_{|}~"

domain :: Parser String st String
domain = do
    x <- subdomain
    xs <- many (try $ char '.' >> subdomain)
    return $ intercalate "." (x:xs)

subdomain :: Parser String st String
subdomain = many1 (emailChar <|> char '@')

emailWord :: Parser String st String
emailWord = many1 emailChar -- ignores possibility of quoted strings

emailAddress :: Parser String st String
emailAddress = try $ do
    firstLetter <- alphaNum
    firstDot <- optionMaybe (char '.')
    x <- emailWord
    xs <- many (try $ char '.' >> emailWord)
    let addr = (firstLetter : maybeToList firstDot) ++ intercalate "." (x:xs)
    _ <- char '@'
    dom <- domain
    let full = addr ++ '@':dom
    return full

import Text.Parsec 
import Data.Char
import Data.List (intercalate, concat)

type Parser t s = Parsec t s

readLine :: [Char] -> [Char]
readLine input = case parse line "" input of
	Left err -> '#':input
	Right val -> input

line :: Parser [Char] st [Char]
line = comment <|> validAlias

comment :: Parser [Char] st [Char]
comment = string "#" >> manyTill (noneOf "\n\r") newline

preAlias :: Parser [Char] st [Char]
preAlias = string "alias " >> manyTill anyChar space

validAlias :: Parser [Char] st [Char]
validAlias = try $ do
	preAlias
   	-- next line mostly works!
   	rest <- manyTill word (try $ (emailAddress <|> angEmail) >> newline)
   	return (concat rest)

word = manyTill (noneOf "\n") space

angEmail = do
	char '<'
	address <- emailAddress
	char '>' <?> "closing angle bracket"
	return address

-- Using old emailAddress parser from Pandoc
-- https://github.com/jgm/pandoc/blob/a71641a2a04c1d324163e16299f1e9821a26c9f9/src/Text/Pandoc/Parsing.hs

emailChar :: Parser [Char] st Char
emailChar = alphaNum <|> oneOf "!\"#$%&'*+-/0123456789=?^_{|}~"

domain = do
  x <- subdomain
  xs <- many (try $ char '.' >> subdomain)
  return $ intercalate "." (x:xs)

subdomain :: Parser [Char] st String
subdomain = many1 (emailChar <|> char '@')

emailWord :: Parser [Char] st String
emailWord = many1 emailChar  -- ignores possibility of quoted strings

emailAddress :: Parser [Char] st String
emailAddress = try $ do
    firstLetter <- alphaNum
    x <- emailWord
    xs <- many (try $ char '.' >> emailWord)
    let addr = firstLetter : (intercalate "." (x:xs))
    char '@'
    dom <- domain
    let full = addr ++ '@':dom
    return full

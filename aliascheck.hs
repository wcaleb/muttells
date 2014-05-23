import Text.Parsec 
-- import Data.Char
import Data.List (intercalate)

type Parser t s = Parsec t s

rawLine, validLine, preAlias, validAlias, comment, groupAlias ::
  	Parser [Char] st [Char]

aliasFile = endBy rawLine newline
rawLine = many (noneOf "\n")

validLine = try comment <|> try groupAlias <|> validAlias

readLine :: [Char] -> [Char]
readLine input = case parse validLine "" input of 
	Left _ -> '#':input
	Right _ -> input 

readFile :: [Char] -> [[Char]]
readFile input = map readLine (either (const $ error "Couldn't parse file") id (parse aliasFile "" input))

validAlias = do
	_ <- preAlias
   	-- next line mostly works!
   	rest <- manyTill (noneOf "\n") (try $ ( angEmail <|> emailAddress ))
   	return rest

groupAlias = do 
	_ <- preAlias
	nicknames <- sepBy nickname (skipMany (char ' ') >> char ',' >> skipMany (char ' '))
	-- this isn't actually what I want to return
	return (concat nicknames)

comment = string "#" >> many (noneOf "\n")

nickname = many (noneOf " ,\n")

preAlias = string "alias " >> manyTill anyChar space

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


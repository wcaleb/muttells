import Text.Parsec 
import Data.List (intercalate, sort)

type Parser t s = Parsec t s

main :: IO ()
main = interact (unlines . sort . map checkLine . lines)

preAlias, validAlias, comment, groupAlias, angEmail :: Parsec [Char] st [Char]

checkLine :: [Char] -> [Char]
checkLine input = case parse validLine "" input of 
	Left _ -> '#':input
	Right _ -> input 
	where validLine = try comment <|> try groupAlias <|> validAlias

validAlias = do
	_ <- preAlias >>
         manyTill (noneOf "<>\n") (try $ ( angEmail <|> emailAddress ))
	notFollowedBy anyToken <?> "end of input"
   	return "valid single alias"

groupAlias = do 
	_ <- preAlias >> nickname >> nicknameSep 
	_ <- sepBy nickname nicknameSep
	return "valid group alias"
	where
      nickname = many (noneOf " ,\n")
      nicknameSep = skipMany (char ' ') >> char ',' >> skipMany (char ' ')

comment = string "#" >> many (noneOf "\n")

preAlias = string "alias " >> manyTill anyChar space

angEmail = do
	_ <- char '<' >> emailAddress >> char '>'
	return "bracketed email address"

-- Using modified old emailAddress parser from Pandoc
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
	-- next line accounts for emails starting with one initial and dot
	-- but such "early dot" cases not returned in full
    x <- try $ char '.' >> emailWord
    xs <- many (try $ char '.' >> emailWord)
    let addr = firstLetter : (intercalate "." (x:xs))
    char '@'
    dom <- domain
    let full = addr ++ '@':dom
    return full


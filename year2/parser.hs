module Parser where

import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid
import Control.Applicative
import Data.Char (isControl, isDigit, isSpace, digitToInt)

newtype Parser i o = Parser { runParser :: (i -> Either i (o,i) ) }

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \i -> fmap (\(o, i') -> (f o, i')) $ p i

instance Applicative (Parser i) where
  pure x = Parser $ \i -> Right (x, i)
  (Parser f) <*> (Parser v) = Parser $ \i ->
    case f i of
      Right (fo, i') -> fmap (\(vo, i'') -> (fo vo, i'')) $ v i'
      Left i      -> Left i

instance Alternative (Parser i) where
  empty = Parser Left
  (Parser p1) <|> (Parser p2) = Parser (\i ->
    case p1 i of
      Right (o, i') -> Right (o, i')
      _            -> p2 i)

predParser :: (a -> Bool) -> Parser [a] a
predParser p = Parser (\i ->
  case i of
    (x : xs) | p x -> Right (x, xs)
    _              -> Left i)

charParser :: Char -> Parser String Char
charParser c = predParser (c == )

notCharParser :: Char -> Parser String Char
notCharParser c = predParser (c /= )

oneOfParser :: Eq a => [a] -> Parser [a] a
oneOfParser x = predParser (`elem` x)

notOneOfParser :: Eq a => [a] -> Parser [a] a
notOneOfParser x = predParser (`notElem` x)

stringParser :: String -> Parser String String
stringParser ""         = Parser (\i -> Right ("", i))
stringParser (c : rest) = (:) <$> charParser c <*> stringParser rest

eoiParser :: Eq a => Parser [a] [a]
eoiParser = Parser (\i -> if i == [] then Right ([], []) else Left i)

surroundedBy :: Parser String a -> Parser String b -> Parser String a
surroundedBy field surrounding = surrounding *> field <* surrounding

separatedBy :: Parser String a -> Parser String b -> Parser String [a]
separatedBy field delim = (:) <$> field <*> many (delim *> field)

endsWith :: Parser String a -> Parser String b -> Parser String [a]
endsWith field delim = many (field <* delim)


newtype Digit = Digit { getDigit :: Int } deriving (Show)

charToDigit :: Char -> Digit
charToDigit = Digit . fromIntegral . digitToInt

digitParser :: Parser String Digit
digitParser = charToDigit <$> predParser isDigit

digitsToInt :: [Digit] -> Integer
digitsToInt = foldl (\acc x -> (10 * acc) + fromIntegral (getDigit x)) 0

numberParser :: Parser String Integer
numberParser = digitsToInt <$> some digitParser

digitsToFrac :: [Digit] -> Double
digitsToFrac ds = (foldr (\x acc -> (acc / 10.0) + (realToFrac . getDigit $ x)) 0.0 ds) / 10

fractionParser :: Parser String Double
fractionParser = digitsToFrac <$> (charParser '.' *> some digitParser)


notFieldChars :: String
notFieldChars = ",\n\r\\"

quoteChars :: String
quoteChars = "'\""

fieldCharParser :: Parser String Char
fieldCharParser = notOneOfParser (notFieldChars ++ quoteChars)


escapeParser :: Char -> Parser String Char
escapeParser q = charParser '\\' *> charParser q

newlineParser :: Parser String String
newlineParser = stringParser "\r\n" <|> stringParser "\n\r" <|> stringParser "\n" <|> stringParser "\r"

unquotedFieldParser :: Parser String String
unquotedFieldParser = many fieldCharParser

quotedFieldParser :: Char -> Parser String String
quotedFieldParser q = many escapedFieldParser `surroundedBy` charParser q
  where
    escapedFieldParser = (notOneOfParser (q : notFieldChars) <|> escapeParser q)

fieldParser :: Parser String String
fieldParser = quotedFieldParser '"' <|> quotedFieldParser '\'' <|> unquotedFieldParser

lineParser :: Parser String [String]
lineParser = fieldParser `separatedBy` charParser ','

csvParser :: Parser String [[String]]
csvParser = (lineParser `endsWith` newlineParser) <* eoiParser

-- FIXME: We should represent all of the actual parts of the frcation, instead of converting ro double
data JsonValue = JsonNull
               | JsonBool Bool
               | JsonInteger Integer
               | JsonDouble Double
               | JsonString String
               | JsonList [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show)

jsonWhiteSpaceParser :: Parser String String
jsonWhiteSpaceParser = many . oneOfParser $ jsonWhiteSpaceChars
  where
    jsonWhiteSpaceChars :: [Char]
    jsonWhiteSpaceChars = "\t\n\r "

jsonNullParser :: Parser String JsonValue
jsonNullParser = stringParser "null" *> pure JsonNull

jsonBoolParser :: Parser String JsonValue
jsonBoolParser = (stringParser "true" *> pure (JsonBool True)) <|> (stringParser "false" *> pure (JsonBool False))

data Sign = SignNegative | SignPositive deriving (Show, Eq)

signParser :: String -> Parser String Sign
signParser signs = (\c -> if c == '-' then SignNegative else SignPositive)
                 <$> oneOfParser signs

signToInt :: Sign -> Integer
signToInt s = if s == SignNegative then -1 else 1

-- FIXME: We don't exclude the first digit from being '0'
intParser :: Parser String Integer
intParser = (\s n -> if s == SignNegative then -n else n)
          <$> (signParser "-" <|> pure SignPositive)
          <*> numberParser

doubleParser :: Parser String Double
doubleParser =
  (\sign int frac exp -> (realToFrac . signToInt $ sign) * (int + frac) * (10 ** exp))
  <$> (signParser "+-" <|> pure SignPositive)
  <*> (realToFrac <$> numberParser)
  <*> (realToFrac <$> (fractionParser <|> pure 0.0))
  <*> (realToFrac <$> (((charParser 'e' <|> charParser 'E') *> intParser) <|> pure 0))

jsonIntegerParser :: Parser String JsonValue
jsonIntegerParser = JsonInteger <$> intParser

jsonDoubleParser :: Parser String JsonValue
jsonDoubleParser = JsonDouble <$> doubleParser

-- FIXME: Not dealing with unicode yet
jsonStringParserRaw :: Parser String String
jsonStringParserRaw = (many jsonCharParser) `surroundedBy` charParser '"'
  where
    jsonCharParser = jsonEscapeParser <|> predParser (\x -> x /= '"' && x /= '\\' && (not $ isControl x))
    jsonEscapeParser = charParser '\\'
                     *> (fromJust . (`lookup` jsonEscapes) <$> oneOfParser (map fst jsonEscapes))
    jsonEscapes = [ ('\"', '\"'),
                    ('\\', '\\'),
                    ('/' , '/' ),
                    ('b' , '\b'),
                    ('f' , '\f'),
                    ('n' , '\n'),
                    ('r' , '\r'),
                    ('t' , '\t') ]

jsonStringParser :: Parser String JsonValue
jsonStringParser = JsonString <$> jsonStringParserRaw

jsonEatWhiteSpaces :: Parser String a -> Parser String a
jsonEatWhiteSpaces p = jsonWhiteSpaceParser *> p <* jsonWhiteSpaceParser

jsonPair :: Parser String (String, JsonValue)
jsonPair = (,) <$> (jsonEatWhiteSpaces jsonStringParserRaw) <*> (charParser ':' *> jsonValueParser)

jsonObjectParser :: Parser String JsonValue
jsonObjectParser = JsonObject . fromMaybe [] <$> (charParser '{' *> optional (jsonPair `separatedBy` charParser ',') <* charParser '}')

jsonListParser :: Parser String JsonValue
jsonListParser = JsonList <$> (charParser '[' *> (jsonValueParser `separatedBy` charParser ',') <* charParser ']')

jsonValueParser :: Parser String JsonValue
jsonValueParser = jsonEatWhiteSpaces
           (jsonNullParser
              <|> jsonBoolParser
              <|> jsonIntegerParser
              <|> jsonDoubleParser
              <|> jsonStringParser
              <|> jsonListParser
              <|> jsonObjectParser)

jsonParser :: Parser String JsonValue
jsonParser = jsonValueParser <* eoiParser

--
-- List parser:
-- 	1 ab 1 c 4 def ghi jkl mno -> [[ab], [c], [def, ghi, jkl, mno]]
--
-- 	(n <n words>)+

whitespaceParser :: Parser String String
whitespaceParser = many $ predParser isSpace

wordParser :: Parser String String
wordParser = whitespaceParser *> (some $ predParser $ not . isSpace)

--wordsParser :: Integer -> Parser String [String]
--wordsParser 0 = pure []
--wordsParser n = (:) <$> wordParser <*> wordsParser (n - 1)
--
--class (Applicative m) => Smashable m where
--  join :: m (m a) -> m a
--  join v = bind v id
--
--  bind :: m a -> (a -> m b) -> m b
--  bind v f = join (f <$> v)
--
--instance Smashable (Parser i) where
--  join p = Parser $ \i -> case runParser p i of
--                                 Left l'        -> Left l'
--                                 Right (p', i') -> runParser p' i'
--
--itemJoinParser :: Parser String [String]
--itemJoinParser = join (wordsParser <$> numberParser)
--
--itemBindParser :: Parser String [String]
--itemBindParser = numberParser `bind` wordsParser
--
--itemsParser :: Parser String [[String]]
--itemsParser = many itemBindParser

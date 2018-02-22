module Json where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Char (chr, digitToInt, isControl, isDigit, isHexDigit, isSpace)
import Data.Functor

newtype JsonStr = JsonStr String

instance Show JsonStr where
  show (JsonStr s) = show s

data JsonNum = JsonNum { negative :: Bool
                       , signif   :: Integer
                       , expo     :: Integer
                       }

instance Show JsonNum where
  show num@(JsonNum n s e) = (if n then "-" else "") ++
                             show s ++ "e" ++ show e ++
                             " (" ++ show (toDouble num) ++ ")"

toDouble :: JsonNum -> Double
toDouble (JsonNum n s e) = (if n then negate else id) $ fromIntegral s * 10 ^^ e

data JsonValue = JsonString JsonStr
               | JsonNumber JsonNum
               | JsonObject [(JsonStr, JsonValue)]
               | JsonArray [JsonValue]
               | JsonBool Bool
               | JsonNull

instance Show JsonValue where
  show v = case v of
                (JsonString s)   -> show s
                (JsonNumber n)   -> show n
                (JsonBool True)  -> "true"
                (JsonBool False) -> "false"
                JsonNull         -> "null"
                (JsonObject o)   -> "{ " ++ showElements showMember o ++ " }"
                (JsonArray a)    -> "[ " ++ showElements show a ++ " ]"
    where
      showMember (k,v) = show k ++ " : " ++ show v

      showElements _ []     = ""
      showElements s [v]    = s v
      showElements s (v:vs) = s v ++ ", " ++ showElements s vs


newtype Parser i o = Parser { runParser :: i -> (Maybe o, i) }

instance Functor (Parser i) where
  fmap f p = Parser $ \input ->
    let (mo, i) = runParser p input
    in (f <$> mo, i)

instance Applicative (Parser i) where
  pure x = Parser $ \input -> (Just x, input)

  pf <*> po = Parser $ \input ->
    case runParser pf input of
         (Just f, rest) -> case runParser po rest of
                                (Just o, rest') -> (Just (f o), rest')
                                (Nothing, _)    -> (Nothing, input)
         (Nothing, _)   -> (Nothing, input)

instance Alternative (Parser i) where
  empty = Parser $ \input -> (Nothing, input)
  p1 <|> p2 = Parser $ \input ->
      case runParser p1 input of
           (Nothing, _) -> case runParser p2 input of
                                (Nothing, _) -> (Nothing, input)
                                justValue    -> justValue
           justValue    -> justValue

satisfy :: (Char -> Bool) -> Parser String Char
satisfy p = Parser $ \input ->
  case input of
       (c:cs) | p c -> (Just c, cs)
       _            -> (Nothing, input)

char :: Char -> Parser String Char
char x = satisfy (x ==)

word :: String -> Parser String String
word "" = Parser $ \input -> (Just "", input)
word (c:cs) = (:) <$> char c <*> word cs

digit :: Parser String Int
digit = digitToInt <$> satisfy isDigit

digit19 :: Parser String Int
digit19 = digitToInt <$> satisfy (\x -> isDigit x && x /= '0')

hexDigit :: Parser String Int
hexDigit = digitToInt <$> satisfy isHexDigit

digitsToNumber :: Int -> [Int] -> Integer
digitsToNumber base = foldl (\num d -> num * fromIntegral base + fromIntegral d) 0

spaces :: Parser String String
spaces = many $ satisfy isSpace

eatSpaces :: Parser String a -> Parser String a
eatSpaces p = spaces *> p <* spaces

sepBy :: Parser i v -> Parser i s -> Parser i [v]
sepBy v s = (:) <$> v <*> many (s *> v) <|> pure []

jsonNull :: Parser String JsonValue
jsonNull = word "null" $> JsonNull

jsonBool :: Parser String JsonValue
jsonBool = (word "true" $> JsonBool True) <|> (word "false" $> JsonBool False)

jsonString :: Parser String JsonValue
jsonString = JsonString <$> jsonStr

jsonStr :: Parser String JsonStr
jsonStr = JsonStr <$> (char '"' *> many jsonChar <* char '"')
  where
    jsonChar =  satisfy (\c -> not (c == '\"' || c == '\\' || isControl c))
            <|> word "\\\"" $> '"'
            <|> word "\\\\" $> '\\'
            <|> word "\\/"  $> '/'
            <|> word "\\b"  $> '\b'
            <|> word "\\f"  $> '\f'
            <|> word "\\n"  $> '\n'
            <|> word "\\r"  $> '\r'
            <|> word "\\t"  $> '\t'
            <|> chr . fromIntegral . digitsToNumber 16 <$> (word "\\u" *> replicateM 4 hexDigit)

jsonNumber :: Parser String JsonValue
jsonNumber = JsonNumber <$> (jsonIntFracExp <|> jsonIntExp <|> jsonIntFrac <|> jsonInt')
  where
    jsonIntExp = JsonNum <$> jsonSign <*> jsonInt False <*> jsonExp

    jsonSign = (char '-' $> True) <|> pure False

    jsonIntFrac = makeIntFrac <$> jsonSign <*> jsonInt True <*> jsonFrac
    -- makeIntFrac significand mantissa
    makeIntFrac n s m = JsonNum n
                                (foldl (\num d -> num * 10 + fromIntegral d) s m)
                                (fromIntegral . negate . length $ m)

    jsonIntFracExp = (\(JsonNum n s e) exp -> JsonNum n s (e + exp))
                     <$> jsonIntFrac
                     <*> jsonExp

    jsonInt' = JsonNum <$> jsonSign <*>  jsonInt False <*> pure 0
    jsonInt leadingZero = negate <$> (char '-' *> jsonUint leadingZero)
                                 <|> jsonUint leadingZero

    jsonUint lz = (\d ds -> digitsToNumber 10 (d:ds)) <$> (if lz then digit19 else digit)
                                                      <*> some digit
              <|> fromIntegral <$> digit

    jsonFrac = char '.' *> some digit

    jsonExp = (char 'e' <|> char 'E') *>
              ((optional (char '+') *> jsonUint False) <|> jsonInt False)

jsonArray :: Parser String JsonValue
jsonArray = JsonArray <$> (char '[' *> (json `sepBy` char ',') <* char ']')

jsonObject :: Parser String JsonValue
jsonObject = JsonObject <$> (char '{' *> pair `sepBy` char ',' <* char '}')
  where
    pair = (,) <$> eatSpaces jsonStr <* char ':' <*> json

json :: Parser String JsonValue
json = eatSpaces json'
  where
    json' =  jsonNull
         <|> jsonBool
         <|> jsonString
         <|> jsonNumber
         <|> jsonArray
         <|> jsonObject

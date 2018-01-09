module Json where

newtype JsonString = JsonStr String deriving (Show)

data JsonNumber = JsonInt Integer
                | JsonIntFrac Integer Integer
                | JsonIntExp Integer Integer
                | JsonIntFracExp Integer Integer Integer

instance Show JsonNumber where
  show (JsonInt i)            = show i
  show (JsonIntFrac i f)      = show i ++ "." ++ show f
  show (JsonIntExp i e)       = show i ++ "e" ++ show e
  show (JsonIntFracExp i f e) = show i ++ "." ++ show f ++ "e" ++ show e

data JsonValue = JsonString JsonString
               | JsonNumber JsonNumber
               | JsonObject [(JsonString, JsonValue)]
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
                (JsonObject o)   -> "{" ++ showElements showMember o ++ "}"
                (JsonArray a)    -> "[" ++ showElements show a ++ "]"
    where
      showMember (k,v) = show k ++ " : " ++ show v

      showElements _ []     = ""
      showElements s [v]    = s v
      showElements s (v:vs) = s v ++ ", " ++ showElements s vs

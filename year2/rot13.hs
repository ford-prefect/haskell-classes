import Data.Char

rot13 :: String -> String
--- rot13 s = [ rot13c c | c <- s] 
rot13 s = case length s of
  0 -> ""
  1 -> [rot13c (head s)]
  _ -> rot13c (head s) : rot13 (tail s)
 where
   rot13c c
    | isAsciiLower c = rot13b 'a' c
    | isAsciiUpper c = rot13b 'A' c
    | otherwise      = error "Non-ASCII character"
   rot13b b c = chr (ord b + (ord c + 13 - ord b) `mod` 26)

main :: IO()
main = print (rot13 "abcABCnopNOP")

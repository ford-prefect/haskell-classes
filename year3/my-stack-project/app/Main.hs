module Main where

import HTML.Dump (dump)

main :: IO ()
main = dump "http://httpbin.org/get" >>= print

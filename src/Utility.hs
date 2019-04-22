module Utility where

removeBracket :: String -> Char -> String
removeBracket s c = case c of
    '(' -> s
    ')' -> s
    _ -> s ++ [c]

cleanWavmResult :: String -> String
cleanWavmResult r = foldl removeBracket "" r
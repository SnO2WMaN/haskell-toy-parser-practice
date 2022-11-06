{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Char (isDigit, isLetter)

type Parser = StateT String (Either String) String

match1 :: (Char -> Bool) -> Parser
match1 f = StateT p
  where
    p (x : xs) | f x = Right ([x], xs)
    p (x : _) | not (f x) = Left (show x ++ ": ")
    p _ = Left "Too short"

combinator :: [Parser] -> Parser
combinator p = do
  l <- sequence p
  return (concat l)

(<+>) :: Parser -> Parser -> Parser
(StateT a) <+> (StateT b) = StateT $ \s ->
  a s <+> b s
  where
    Left a <+> Left b = Left $ a ++ b
    Left _ <+> b = b
    a <+> _ = a

exiter :: String -> Parser
exiter message = lift (Left message)

matchDigit :: Parser
matchDigit = match1 isDigit <+> exiter "not digit"

matchLetter :: Parser
matchLetter = match1 isLetter <+> exiter "not letter"

matchBraL :: Parser
matchBraL = match1 (== '[') <+> exiter "not '['"

matchBraR :: Parser
matchBraR = match1 (== ']') <+> exiter "not ']'"

matchCurL :: Parser
matchCurL = match1 (== '<') <+> exiter "not '<'"

matchCurR :: Parser
matchCurR = match1 (== '>') <+> exiter "not '>'"

matchDigitOrLetter :: Parser
matchDigitOrLetter = matchDigit <+> matchLetter

pick1 :: Parser
pick1 = match1 (const True)

pick2 :: Parser
pick2 =
  combinator
    [ pick1
    , pick1
    ]

pick3 :: Parser
pick3 =
  combinator
    [ matchBraL
    , matchBraR
    , pick1
    ]

pickN :: Int -> Parser
pickN n = combinator $ replicate n pick1

main :: IO ()
main = do
  print $ runStateT matchDigit "48are"
  print $ runStateT matchDigit "eeare"
  print $ runStateT matchLetter "a4rere"
  print $ runStateT matchBraL "[a"
  print $ runStateT matchBraR "]a87"
  print $ runStateT matchBraR "[a87"
  print $ runStateT matchDigitOrLetter "aa87"
  print $ runStateT matchDigitOrLetter "[a87"
  print $ runStateT pick2 "re"
  print $ runStateT pick2 "tere"
  print $ runStateT pick2 "e"
  print $ runStateT pick2 "e"
  print $ runStateT pick3 "[]bcdefg"
  print $ runStateT pick3 "]]bcdefg"
  print $ runStateT (pickN 5) "]]bcdefg"

-- print $ runState matchBraL "ater"

-- print $ firstChar ""
-- print $ firstChar "aa"
-- print $ satisfyBraL "[a"
-- print $ satisfyBraL "[abbere]"
-- print $ satisfyBraL "[abbe[[re]"

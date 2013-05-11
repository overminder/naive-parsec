module Regex (
  Regex,
  compile,
  match
) where

import Data.List (intercalate)
import Control.Monad
import Control.Applicative hiding (many)

import Parser

data Regex
  = RChar Char
  | RAnyChar
  | RRange [(Char, Char)]
  | RSeq Regex Regex
  | RNil
  | RPlus Regex
  | RStar Regex
  | RGroup Regex
  | RAlter [Regex]
  | RReplicate Int (Maybe Int) Regex
  | REnd

instance Show Regex where
  show (RChar c) = [c]
  show (RAnyChar) = "."
  show (RRange cps) = "[" ++ concatMap (\ (f, t) -> [f, '-', t]) cps ++ "]"
  show (RSeq r1 r2) = show r1 ++ show r2
  show (RNil) = ""
  show (RPlus r) = show r ++ "+"
  show (RStar r) = show r ++ "*"
  show (RGroup r) = "(" ++ show r ++ ")"
  show (RAlter rs) = intercalate "|" (map show rs)
  show (RReplicate f Nothing r) = show r ++ "{" ++ show f ++ "}"
  show (RReplicate f (Just t) r) = show r ++ "{" ++ show f ++ "," ++
                                   show t ++ "}"
  show (REnd) = "$"

noneOf xs = one (`notElem` xs)
oneOf xs = one (`elem` xs)

pRegex :: StringParser Regex
pRegex = pSeq (pAlter (pModifier choices))
  where
    choices = pChar `orElse` pRange `orElse` pGroup

pChar :: StringParser Regex
pChar = toRegex <$> noneOf "()[]+*|"
  where
    toRegex c = case c of
      '.' -> RAnyChar
      '$' -> REnd
      _ ->   RChar c

pRange :: StringParser Regex
pRange = RRange <$> (char '[' *> many1 pSegment <* char ']')
  where
    pSegment = (,) <$> (noneOf "]" <* char '-') <*> anyOne

pGroup :: StringParser Regex
pGroup = RGroup <$> (char '(' *> pRegex <* char ')')

pModifier :: StringParser Regex -> StringParser Regex
pModifier pLhs = do
  lhs <- pLhs
  choice [pStar lhs, pPlus lhs, pReplicate lhs, pure lhs]

pStar lhs = RStar <$> (char '*' *> pure lhs)
pPlus lhs = RPlus <$> (char '+' *> pure lhs)
pReplicate lhs = RReplicate <$> (char '{' *> pInt) <*>
                                (pMaybe (char ',' *> pInt) <* char '}') <*>
                                pure lhs

pMaybe :: Parser s a -> Parser s (Maybe a)
pMaybe m = choice [Just <$> m, pure Nothing]

pInt :: StringParser Int
pInt = read <$> many1 digit

digit = oneOf "0123456789"

pAlter :: StringParser Regex -> StringParser Regex
pAlter x = toAlter <$> x `sepBy1` char '|'
  where
    toAlter xs@(_:_:_) = RAlter xs
    toAlter [x] = x

toSeq :: [Regex] -> Regex
toSeq = foldr RSeq RNil

pSeq :: StringParser Regex -> StringParser Regex
pSeq m = toSeq <$> many1 m

toParser :: Regex -> StringParser String
toParser (RChar c) = (: []) <$> char c
toParser (RAnyChar) = (: []) <$> anyOne
toParser (REnd) = end >> return []
toParser (RRange cps) = (: []) <$>
  one (foldr combine (const False) cps)
  where
    combine (f, t) p = \c -> p c || (f <= c && c <= t)
toParser (RSeq c1 c2) = (++) <$> toParser c1 <*> toParser c2
toParser (RNil) = pure []
toParser (RPlus r) = concat <$> many1 (toParser r)
toParser (RStar r) = concat <$> many (toParser r)
toParser (RGroup r) = toParser r
toParser (RAlter rs) = choice (map toParser rs)
toParser (RReplicate f mbTo r) = concat <$>
  choice (map (flip replicateM (toParser r)) [f..t])
  where
    t = maybe f id mbTo

compile :: String -> Maybe Regex
compile = parse pRegex

match :: Regex -> String -> Maybe String
match rawRegex str = parse (toParser rawRegex) str


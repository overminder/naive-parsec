module Parser where

import Control.Applicative hiding (many)
import Control.Monad

newtype Parser i o = Parser { runParser :: [i] -> [(o, [i])] }

parse :: Parser i o -> [i] -> Maybe o
parse p xs = case runParser p xs of
  (o, _):_ -> Just o
  _ -> Nothing

instance Monad (Parser i) where
  -- This is just an implementation of andThen
  Parser ma >>= f = Parser $ \ xs -> let aRes = ma xs
                                         runF (a, xs') = runParser (f a) xs'
                                      in concatMap runF aRes
  return a = Parser $ \ xs -> [(a, xs)]

instance Functor (Parser i) where
  fmap = liftM

instance Applicative (Parser i) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

type StringParser = Parser Char

one :: (a -> Bool) -> Parser a a
one f = Parser $ \ xs -> case xs of
  y:ys | f y -> [(y, ys)] 
  _          -> []

anyOne :: Parser a a
anyOne = one (const True)

noMatch :: Parser a a
noMatch = Parser $ const []

char :: Char -> StringParser Char
char c = one (==c)

string :: String -> StringParser String
string (c:cs) = (:) <$> char c <*> string cs
string _ = return []

orElse :: Parser i a -> Parser i a -> Parser i a
orElse (Parser ma) (Parser mb) = Parser $ \ xs -> ma xs ++ mb xs

-- For andThen, see the Monad instance def above

-- XXX: Currently cannot handle nested many,
--      due to the use of simple backtracking.
many :: Parser i c -> Parser i [c]
many ma = ((:) <$> ma <*> many ma) `orElse` return []

many1 :: Parser i c -> Parser i [c]
many1 ma = (:) <$> ma <*> many ma

choice = foldl1 orElse

sepBy1 x sep = (:) <$> x <*> many (sep *> x)
sepBy x sep = sepBy1 x sep `orElse` return []

end :: Parser a ()
end = Parser $ \ xs -> case xs of
  [] -> [((), [])]
  _ -> []


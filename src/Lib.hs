{-# LANGUAGE PatternSynonyms #-}

module Lib (eval) where

import Control.Applicative ((<|>))
import Data.Char (chr, ord)
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromJust)
import GHC.TopHandler (flushStdHandles)
import Prelude hiding (lookup)

pattern BFplus :: Char
pattern BFplus = '+'
pattern BFminus :: Char
pattern BFminus = '-'
pattern BFleft :: Char
pattern BFleft = '<'
pattern BFright :: Char
pattern BFright = '>'
pattern BFout :: Char
pattern BFout = '.'
pattern BFin :: Char
pattern BFin = ','
pattern BFbracket1 :: Char
pattern BFbracket1 = '['
pattern BFbracket2 :: Char
pattern BFbracket2 = ']'

eval :: ((Map Int Int, Int), String) -> IO ((Map Int Int, Int), String)
eval ((ss, s), BFout : cs) = do
    putChar . chr . fromJust $ lookup s ss <|> pure 0
    flushStdHandles
    eval ((ss, s), cs)
eval ((ss, s), BFin : cs) = do
    x <- getChar
    eval ((insert s (ord x) ss, s), cs)
eval ((ss, s), BFplus : cs) = do
    eval ((insert s ((v + 1) `mod` 256) ss, s), cs)
  where
    v = fromJust (lookup s ss <|> pure 0)
eval ((ss, s), BFminus : cs)
    | v == 0 = eval ((insert s 255 ss, s), cs)
    | otherwise = eval ((insert s (v - 1) ss, s), cs)
  where
    v = fromJust (lookup s ss <|> pure 0)
eval ((ss, s), BFleft : cs) = eval ((ss, s - 1), cs)
eval ((ss, s), BFright : cs) = eval ((ss, s + 1), cs)
eval ((ss, s), BFbracket1 : cs)
    | (lookup s ss <|> pure 0) == Just 0 = eval ((ss, s), f drop cs)
    | otherwise = do
        ((ss', s'), []) <- eval ((ss, s), f take cs)
        eval ((ss', s'), BFbracket1 : cs)
  where
    f x =
        uncurry x
            . ((<*>) . (<$>) (,))
                ( snd . snd
                    . foldl
                        ( \(s, (n, p)) c -> case () of
                            _
                                | s -> (s, (n, p))
                                | c `elem` [BFbracket1, BFbracket2] -> (n == 0 && c == BFbracket2, (n + ord BFbracket2 - ord c - 1, p + 1))
                                | otherwise -> (s, (n, p + 1))
                        )
                        (False, (0, 0))
                )
                id

eval (x, []) = pure (x, [])
eval (s', c : cs) = eval (s', cs)
-- somehow identical patterns with different variables are recognized as overlapping
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Lib (eval) where

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (chr, ord)
import Data.Map (Map, insert, lookup)
import Data.Maybe (fromJust, fromMaybe)
import GHC.TopHandler (flushStdHandles)
import Prelude hiding (lookup)

bfplus :: Char
bfplus = '+'

bfminus :: Char
bfminus = '-'

bfleft :: Char
bfleft = '<'

bfright :: Char
bfright = '>'

bfout :: Char
bfout = '.'

bfin :: Char
bfin = ','

bfbracket1 :: Char
bfbracket1 = '['

bfbracket2 :: Char
bfbracket2 = ']'

eval :: ((Map Int Int, Int), String) -> IO ((Map Int Int, Int), String)
eval ((ss, s), bfplus : cs) = do
    putChar $ chr (fromMaybe 0 $ lookup s ss)
    flushStdHandles
    eval ((ss, s), cs)
eval ((ss, s), bfin : cs) = do
    x <- getChar
    eval ((insert s (ord x) ss, s), cs)
eval ((ss, s), bfplus : cs) = do
    let v = fromMaybe 0 $ lookup s ss
    eval ((insert s ((v + 1) `mod` 256) ss, s), cs)
eval ((ss, s), bfminus : cs)
    | v == 0 = eval ((insert s 255 ss, s), cs)
    | otherwise = eval ((insert s (v - 1) ss, s), cs)
  where
    v = fromJust (lookup s ss <|> pure 0)
eval ((ss, s), bfleft : cs) = eval ((ss, s - 1), cs)
eval ((ss, s), bfright : cs) = eval ((ss, s + 1), cs)
eval ((ss, s), bfbracket1 : cs)
    | fromJust (lookup s ss <|> pure 0) == 0 = eval ((ss, s), uncurry drop . ((<*>) . (<$>) (,)) f id $ cs)
    | otherwise = do
        ((ss', s'), []) <- eval ((ss, s), uncurry take . ((<*>) . (<$>) (,)) f id $ cs)
        eval ((ss', s'), bfbracket2 : cs)
  where
    f =
        snd . snd
            . foldl
                ( \(s, (n, p)) c -> case () of
                    _
                        | s -> (s, (n, p))
                        | c `elem` [bfbracket1, bfbracket2] ->
                            ( n == 0 && c == bfbracket2
                            , (n + ord bfbracket2 - ord c - 1, p + 1)
                            )
                        | otherwise -> (s, (n, p + 1))
                )
                (False, (0, 0))
eval (x, []) = pure (x, [])
eval (s', _ : cs) = eval (s', cs)
{-# LANGUAGE PatternSynonyms #-}

module Lib where

import Control.Applicative ((<|>))
import Control.Monad.State (MonadIO (liftIO), MonadState (get, put), StateT (runStateT))
import Data.Char (chr, ord)
import Data.Map (Map, findWithDefault, insert)
import Data.Maybe (fromJust)
import Foreign.C (CUChar)
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

eval :: String -> StateT (Map Int CUChar, Int) IO String
eval [] = pure (pure $ toEnum 0)
eval (BFout : is) = do
    (ss, s) <- get
    liftIO . putChar . chr . fromEnum $ findWithDefault 0 s ss
    liftIO flushStdHandles
    eval is
eval (BFin : is) = do
    (ss, s) <- get
    i <- liftIO getChar
    put (insert s (toEnum $ ord i) ss, s)
    eval is
eval (BFplus : is) = do
    (ss, s) <- get
    put (insert s (findWithDefault 0 s ss + 1) ss, s)
    eval is
eval (BFminus : is) = do
    (ss, s) <- get
    put (insert s (findWithDefault 0 s ss - 1) ss, s)
    eval is
eval (BFleft : is) = do
    (ss, s) <- get
    put (ss, s + 1)
    eval is
eval (BFright : is) = do
    (ss, s) <- get
    put (ss, s - 1)
    eval is
eval (BFbracket1 : is) = do
    (ss, s) <- get
    if findWithDefault 0 s ss == 0
        then eval (f drop is)
        else do
            eval (f take is)
            eval (BFbracket1 : is)
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
eval (i : is) = eval is

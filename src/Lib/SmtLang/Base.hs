{-# LANGUAGE FlexibleInstances #-}

module Lib.SmtLang.Base 
    ( AST(..),
      prettyPrint
    ) where

-- An AST representing a language of parentheses containing statements

import Text.Show

data AST s = Name s | Paren [AST s]

type Program s = [AST s]

instance Show (AST String) where
    showsPrec _ (Name n) = showString n
    showsPrec d (Paren xs) = 
        showString "(" .
        showArgs xs .
        showString ")"
        where showArgs [] = id
              showArgs [x] = showsPrec d x
              showArgs (x:xs) = showsPrec d x . showString " " . showArgs xs

prettyPrint :: Program String -> String
prettyPrint xs = unlines (map show xs)

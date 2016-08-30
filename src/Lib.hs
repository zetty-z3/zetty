module Lib
    ( someFunc
    ) where

import Lib.SmtLang.Base
import Lib.SmtLang.Smt

someFunc :: IO ()
someFunc = putStrLn $ prettyPrint $ [
    Paren [Name ">", Name "1", Paren [Name "+", Name "1", Name "2"]],
    Paren [Name "=", Paren [Name "+", Name "y", Name "z"], Name "x"]
    ]

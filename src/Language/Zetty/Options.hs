--------------------------------------------------------------------------------
-- Zetty                                                                      --
-- (C) 2016 The Zetty Project                                                 --
--------------------------------------------------------------------------------

-- | This module contains types and parsers for command-line options.
module Language.Zetty.Options (
    Options(..),
    parseCmdArgs
) where

--------------------------------------------------------------------------------

import Options.Applicative

--------------------------------------------------------------------------------

-- | A type representing compiler options which may be passed to the compiler
-- via the command-line.
data Options =
    Options {
        optSourceFiles :: [String]
    }

-- | 'optionsP' is a parser for the `Options` type.
optionsP :: Parser Options
optionsP = Options
       <$> some (argument str (metavar "SOURCES"))

-- | 'parseCmdArgs' obtains and parses command-line arguments into a value of
-- type 'Options'.
parseCmdArgs :: IO Options
parseCmdArgs = execParser opts
    where
        opts = info (helper <*> optionsP)
             ( fullDesc
            <> progDesc "Prove some stuff about SOURCES"
            <> header "zetty - A simple theorem proving language powered by z3.")

--------------------------------------------------------------------------------

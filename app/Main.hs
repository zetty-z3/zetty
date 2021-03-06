--------------------------------------------------------------------------------
-- Zetty                                                                      --
-- (C) 2016 The Zetty Project                                                 --
--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import Language.Zetty
import Language.Zetty.Options

--------------------------------------------------------------------------------

-- | 'main' is the main entry point for this application.
main :: IO ()
main = do
    opts <- parseCmdArgs
    someFunc

--------------------------------------------------------------------------------

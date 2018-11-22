module Main where

import Options.Applicative
import qualified Config as C
import qualified Truth as T

main :: IO ()
main = execParser opts >>= T.generate
  where
    opts = info (C.parser <**> helper)
      ( fullDesc
     <> progDesc "program desc"
     <> header "header" )


module Main where

import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Text.Pandoc.Error

import Control.Monad.Except
import Options.Applicative
import qualified Config as C
import qualified Truth as T

main :: IO ()
main = do
  config <- execParser opts
  pandocErr <- runIO $ T.generate config
  handle pandocErr
  where
    opts = info (C.parser <**> helper)
      ( fullDesc
     <> progDesc "program desc"
     <> header "header" )

    handle (Right(pandoc)) = putStrLn $ show pandoc
    handle (Left(error)) = putStrLn $ show error


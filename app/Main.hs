module Main where

import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Error
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Control.Monad.Except
import Options.Applicative
import qualified Config as C
import qualified Truth as T
import Text.Pandoc.Writers.HTML
import Config

main :: IO ()
main = do
  putStrLn "Reading configuration..."
  config <- execParser opts
  html <- runIO $ program config
  serialize config html
  putStrLn "Done."
  where
    opts = info (C.parser <**> helper)
      ( fullDesc
     <> progDesc "program desc"
     <> header "header" )

program :: C.Config -> PandocIO Text
program config = do
  liftIO $ putStrLn "Generate document..."
  pandoc <- T.generate config
  liftIO $ putStrLn "Write document to HTML..."
  writeHtml4String def pandoc

serialize :: Config -> Either PandocError Text -> IO ()
serialize _ (Left(error)) = putStrLn $ show error
serialize config (Right(html)) = do
  let output = _output config
  putStrLn $ "Store HTML to " ++ output ++ "..."
  TIO.writeFile output html


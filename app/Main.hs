module Main where

import Control.Lens
import Text.Pandoc.Options
import Text.Pandoc.Class
import Text.Pandoc.Error
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Control.Monad.Except
import Options.Applicative
import qualified Config as C
import qualified Truth as T
import qualified Presentation as P
import Text.Pandoc.Writers.HTML
import Config

main :: IO ()
main = do
  putStrLn "Reading configuration..."
  config <- execParser opts
  runIO $ program config
  putStrLn "Done."
  where
    opts = info (C.parser <**> helper)
      ( fullDesc
     <> progDesc "program desc"
     <> header "header" )

program :: C.Config -> PandocIO ()
program config = do
  liftIO $ putStrLn "Generate document..."
  features <- T.generate config
  htmlRaw <- liftIO $ P.present features
  liftIO $ putStrLn "Write document to HTML..."
  liftIO $ serialize config htmlRaw

serialize :: Config -> String -> IO ()
serialize config html = do
  let output = config^.outputFile
  putStrLn $ "Store HTML to " ++ output ++ "..."
  TIO.writeFile output (pack html)

  


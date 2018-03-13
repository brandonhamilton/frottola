module Main where

import Control.Monad.IO.Class
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text
import Language.Frottola.Parser
import System.Console.Haskeline
import Text.Parsix

process :: MonadIO m => Text -> InputT m ()
process line = case parseProgram line of
    Failure e -> output (prettyError e)
    Success p -> mapM_ (outputStrLn . show) p
  where
    output t = outputStrLn . unpack $ renderStrict (layoutPretty defaultLayoutOptions t)

main :: IO ()
main = runInputT defaultSettings repl
  where
    repl = do
      minput <- getInputLine "frottola> "
      case minput of
        Nothing -> pure ()
        Just input -> process (pack input) >> repl

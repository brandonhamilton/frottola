module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text
import Data.Text.Lazy (toStrict)
import Language.Frottola.Parser
import Language.Frottola.Codegen
import Language.Frottola.Optimizer
import LLVM.Pretty (ppllvm)
import qualified LLVM.AST as AST
import System.Console.Haskeline
import System.Environment
import Text.Parsix

process
  :: MonadIO m => (Text -> m ()) -> AST.Module -> Text -> m (Maybe AST.Module)
process out mod line = case parseProgram line of
  Failure e -> output (prettyError e) >> return Nothing
  Success p -> do
    -- out . pack . show $ p
    ast <- liftIO (codegen mod p >>= runOpt)
    out . toStrict . ppllvm $ ast
    pure $ Just ast
  where output t = out $ renderStrict (layoutPretty defaultLayoutOptions t)

processFile :: String -> IO (Maybe AST.Module)
processFile fname =
  readFile fname >>= process (putStrLn . unpack) initModule . pack

interactive :: IO ()
interactive = runInputT defaultSettings (repl initModule)
 where
  repl mod = do
    minput <- getInputLine "frottola> "
    case minput of
      Nothing    -> pure ()
      Just input -> do
        modn <- process (outputStrLn . unpack) mod (pack input)
        repl $ fromMaybe mod modn

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> interactive
    [fname] -> void (processFile fname)

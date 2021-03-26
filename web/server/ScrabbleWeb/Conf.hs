{-# Language OverloadedStrings, ApplicativeDo #-}
module ScrabbleWeb.Conf (parseConf, defaultConf)
  where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)
import Config
import Config.Schema
import ScrabbleWeb.Types (Conf(..))

-- ======== Config parser =========== --

-- | The default config for the server. Hostname = "127.0.0.1", port = 9160, log_file
--   = "./log/scrabble.log". To alter these values edit the config file ./etc/scrabble.conf.
defaultConf :: Conf
defaultConf = Conf { hostname = "127.0.0.1"
                   , port = 9160
                   , log_file = "./log/scrabble.log"
                   , log_priority = "WARNING" }

-- The spec for the config file.
spec :: Conf -> ValueSpec Conf
spec def = sectionsSpec "scrabble-server conf" $
  do hn <- fromMaybe (hostname def) <$> optSection "hostname"
           "Supply the hostname as a string."
     pt <- fromMaybe (port def) <$> optSection "port" 
           "Supply the port as a number."
     lf <- fromMaybe (log_file def) <$> optSection "log_file"
           "Supply the path to the log file."
     pr <- fromMaybe (log_priority def) <$> optSection "log_priority"
                 "Supply the logging priority, a string representing a value of System.Log.Priority."
     return (Conf hn pt lf pr)

-- Print the spec for the config file.
printDoc :: IO ()
printDoc = print (generateDocs (spec defaultConf))

-- | Parse the config file with default.
parseConf :: FilePath -> Conf -> IO Conf
parseConf path def = do
  conf <- T.readFile path
  case parse conf of
    Left e  -> usageAndDefault e def
    Right v -> do case loadValue (spec def) v of
                    Left e  -> usageAndDefault e def
                    Right c -> pure c

-- Print the spec for the config file and return the default.
usageAndDefault :: Show a => a -> Conf -> IO Conf
usageAndDefault e def = T.putStrLn (T.pack $ show e)
                        >> printDoc >> T.putStrLn "Using default conf" >> pure def

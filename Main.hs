module Main where

import Control.Concurrent (MVar, forkIO, killThread)
import Control.Concurrent.MVar
import Happstack.Util.Cron (cron)
import Happstack.State (waitForTermination)
import Happstack.Server
  ( Conf(port)
  , simpleHTTP
  , nullConf
  , validator
  , wdgHTMLValidator
  )
import Happstack.State
  ( Component
  , Proxy(..)
  , Methods
  , TxControl
  , Saver(Queue, FileSaver)
  , runTxSystem
  , shutdownSystem
  , createCheckpoint
  , query 
  , update
  )
import System.Environment (getArgs)
import System.Log.Logger (Priority(..), logM)
import System.Exit (exitFailure)
import System.Console.GetOpt 

import State
import Controller

import Data.ByteString.Char8 (pack)

stateProxy :: Proxy Weblog
stateProxy  = Proxy


main :: IO ()
main = 
    do let progName = "weblog"

       args <- getArgs

       appConf <- case parseConfig args of 
                    (Left e)  -> do logM progName ERROR (unlines e)
                                    exitFailure
                    (Right f) -> return (f $ defaultConf progName)

       control <- startSystemState' (store appConf) stateProxy

--       art1
--       art2

       httpTid <- forkIO $ simpleHTTP (httpConf appConf) appHandler

       cronTid <- forkIO $ cron (60*60*24) (createCheckpoint control)

       waitForTermination

       killThread httpTid
       killThread cronTid
       createCheckpoint control
       shutdownSystem control

data AppConf
    = AppConf { httpConf :: Conf
              , store :: FilePath
              , static :: FilePath 
              }

defaultConf :: String -> AppConf
defaultConf progName
    = AppConf { httpConf = nullConf
              , store    = "_local/" ++ progName ++ "_state"
              , static   = "public"
              }

opts :: [OptDescr (AppConf -> AppConf)]
opts = [ Option [] ["http-port"]   (ReqArg (\h c -> c { httpConf = (httpConf c) {port = read h} }) "port") "port to bind http server"
       , Option [] ["no-validate"] (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Nothing } })) "Turn off HTML validation"
       , Option [] ["validate"]    (NoArg (\ c -> c { httpConf = (httpConf c) { validator = Just wdgHTMLValidator } })) "Turn on HTML validation"
       , Option [] ["store"]       (ReqArg (\h c -> c {store = h}) "PATH") "The directory used for database storage."
       , Option [] ["static"]      (ReqArg (\h c -> c {static = h}) "PATH") "The directory searched for static files" 
       ]

parseConfig :: [String] -> Either [String] (AppConf -> AppConf)
parseConfig args
    = case getOpt Permute opts args of
        (flags,_,[]) -> Right $ \appConf -> foldr ($) appConf flags
        (_,_,errs)   -> Left errs

startSystemState' :: (Component st, Methods st) => String -> Proxy st -> IO (MVar TxControl)
startSystemState' = runTxSystem . Queue . FileSaver

-- art1 = query ReadWeblog >>= \x -> update $ PostToArticle (pack "first Article") (pack "line1\nline2\nline3\tasdf\nende") (MkWeblog x)
-- art2 = query ReadWeblog >>= \x -> update $ PostToArticle (pack "secound Article") (pack "line1\nline2\nline3\tasdf\nende") (MkWeblog x) 

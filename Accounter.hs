module Accounter where

import Control.Concurrent
import Control.Concurrent.MVar
import System.Posix.Signals

import Happstack.State (createCheckpoint, shutdownSystem, runTxSystem)
import Happstack.State.Saver (Saver(..))
import Happstack.State.ComponentSystem (Component(), Methods())
import Happstack.Data.Proxy (Proxy(..))

import Happstack.State.Transaction (TxControl)

import Happstack.Auth

handler :: MVar TxControl -> IO ()
handler control = 
    do putStrLn "[Accounter]: Shutting down"
       createCheckpoint control
       shutdownSystem   control
       return ()

startSystemState' :: (Component st, Methods st) => String -> Proxy st -> IO (MVar TxControl)
startSystemState' = runTxSystem . Queue . FileSaver

authStateProxy :: Proxy AuthState
authStateProxy =  Proxy

data UserInfo = UserInfo Username String

data UserAction = Login UserAuthInfo
                | Logout SessionKey
                | Register UserInfo

runAccounter :: MVar UserAction -> MVar Bool -> IO ()
runAccounter mvUserAction mvResponse = 
    do let progName = "accounter"
       installHandler sigUSR1 (Catch $ handler) Nothing

       control <- startSystemState' (progName) authStateProxy

       usrAct  <- takeMVar mvUserAction

       loop usrAct
   where loop userAction = 
            do case userAction of 
                (Login    uai)            -> do mu <- query $ AuthUser user pass
                                                case mu of 
                                                   Just u  -> performLogin u >> 
                                                              putMVar mvResponse True
                                                   Nothing -> putMVar mvResponse False
                (Logout    sk)            -> do performLogout sk
                                                putMVar mvResponse True
                (Register (UserInfo u p)) -> do x <- register u p
                                                case x of
                                                    Just x  -> performLogin u >>
                                                               putMVar mvResponse True
                                                    Nothing -> putMVar mvResponse False   

               mu <- query $ AuthUser user pass
               case mu of 
                Just u  -> do performLogin u
                              putMVar mvResponse True
                Nothing -> do putMVar mvResponse False
               
               usrAct <- takeMVar mvUserAction

               loop usrAct

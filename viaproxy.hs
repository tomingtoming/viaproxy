{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network (withSocketsDo)
import Data.Conduit as C
import Data.Conduit.Binary as CB
import Data.Conduit.Network
import qualified Data.ByteString as Bs
import qualified Data.ByteString.Char8 as Bsc
import qualified Data.ByteString.Lazy as Lbs
import Control.Concurrent.Async (concurrently)
import System.Environment (getArgs)

main :: IO ()
main = withSocketsDo $ do
  (localPort:proxyHost:proxyPort:remoteHost:remotePort:_) <- getArgs
  let
    srvConf = serverSettings (read localPort) HostAny
    cliConf = clientSettings (read proxyPort) (Bsc.pack proxyHost)
    srvApp srvAppData = do
      putStrLn $ "Start:" ++ (show $ appSockAddr srvAppData)
      runTCPClient cliConf (cliApp srvAppData)
      putStrLn $ "End  :" ++ (show $ appSockAddr srvAppData)
    cliApp srvAppData cliAppData = do
      let
        srvSrc  = appSource srvAppData
        srvSink = appSink   srvAppData
        cliSrc  = appSource cliAppData
        cliSink = appSink   cliAppData
      proxyRequest  remoteHost remotePort $$ cliSink
      (cliSrc', _) <- cliSrc $$+ proxyResponse
      _ <- (srvSrc $$ cliSink) `concurrently` (cliSrc' $$+- srvSink)
      return ()
  runTCPServer srvConf srvApp

proxyRequest :: Monad m => String -> String -> Source m Bs.ByteString
proxyRequest host port = do
  yield request
  where
    hostBs  = Bsc.pack host
    portBs  = Bsc.pack port
    request = Bs.concat ["CONNECT ", hostBs, ":", portBs, " HTTP/1.1", "\r\n", "\r\n"]

proxyResponse :: Monad m => Sink Bs.ByteString m Bs.ByteString
proxyResponse = do
  res1 <- CB.takeWhile (/= 10) =$ sinkLbs
  CB.drop 1
  _    <- CB.takeWhile (/= 10) =$ sinkLbs
  CB.drop 1
  return $ Lbs.toStrict $ Lbs.init res1

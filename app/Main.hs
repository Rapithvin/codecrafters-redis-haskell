{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock, Socket, SockAddr, send)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(NoBuffering))

-- Setup output buffering
setupBuffering :: IO ()
setupBuffering = hSetBuffering stdout NoBuffering *> hSetBuffering stderr NoBuffering

-- Logging function
logMsg :: String -> IO ()
logMsg = putStrLn

-- Main entry point
main :: IO ()
main = 
    setupBuffering *> logMsg "Logs from your program will appear here" *>
    let port = "6379"
    in logMsg ("Redis server listening on port " ++ port) *>
    serve HostAny port handleClient

-- Handle client connections
handleClient :: (Socket, SockAddr) -> IO ()
handleClient (socket, address) = 
    logMsg ("Successfully connected client: " ++ show address) *>
    send socket "+PONG\r\n" *>
    closeSock socket
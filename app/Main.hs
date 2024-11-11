{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock, Socket, SockAddr, recv, send)
import System.IO (hSetBuffering, stdout, stderr, BufferMode(NoBuffering))
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Control.Monad (forever, void)

-- Core Logic: Pure function to handle commands using guards
handleCommand :: String -> String
handleCommand cmd
    | cmd == "PING" = "+PONG\r\n"
    | otherwise     = "+PONG\r\n"

-- Parsing Layer: Parse commands from socket data
parseCommand :: ByteString -> String
parseCommand = unpack

-- Logging Layer: Log messages
logMsg :: String -> IO ()
logMsg = putStrLn

-- IO Layer: Setup buffering
setupBuffering :: IO ()
setupBuffering = hSetBuffering stdout NoBuffering *> hSetBuffering stderr NoBuffering

-- IO Layer: Handle client connections
handleClient :: (Socket, SockAddr) -> IO ()
handleClient (socket, address) = 
    logMsg ("Successfully connected client: " ++ show address) *>
    clientLoop socket *> closeSock socket

-- IO Layer: Client loop for receiving and responding to commands
clientLoop :: Socket -> IO ()
clientLoop socket = forever $
    recv socket 1024 >>= maybe (return ()) (sendResponse socket . handleCommand . parseCommand)

-- IO Layer: Send response back to the client
sendResponse :: Socket -> String -> IO ()
sendResponse socket = void . send socket . pack

-- Main entry point
main :: IO ()
main =
    setupBuffering *> 
    logMsg "Logs from your program will appear here" *>
    let port = "6379" in
    logMsg ("Redis server listening on port " ++ port) *>
    serve HostAny port handleClient
{-----------------------------------------------------------------------------
 -
 -                FPR - Functional and Logic Programming
 -             Project 1: Lightweight XMPP server in Haskell
 -
 -                            Ondrej Lengal
 -                      xlenga00@stud.fit.vutbr.cz
 -
 -                   Faculty of Information Technology
 -                     Brno University of Technology
 -
 - This module contains the functions for handling commands that are processed
 - in the command loop.
 -
 -----------------------------------------------------------------------------}

{-|
  This module contains functions for handling commands that are
  processed in the command loop.
 -}
module CommandEngine (processCommand) where

import System.IO
  (Handle, hPutStrLn, hFlush, hClose)
import Control.Monad
  (forM, when)

import Global


{-|
  The 'processCommand' function receives a list of clients, a command and
  a handle (to identify the thread that sent the command) and processes the
  command (sends messages, etc.) and returns an updated list of clients.
 -}
processCommand :: [Client]      -- ^ The list of clients
               -> Handle        -- ^ The handle of the command source
               -> Command       -- ^ The command that is being processed
               -> IO [Client]   -- ^ The updated list of clients
processCommand clients handle command = do
  let cl = findByHandle handle clients
  putStrLn $ "data: " ++ command ++ " handle: " ++ show handle
  clients' <- forM clients $
    \(ch, h, state, jid) -> do
      hPutStrLn h command
      hFlush h
      return [(ch, h, state, jid)]
      `catch` const (hClose h >> return [])
  let dropped = length $ filter null clients'
  when (dropped > 0) $
    putStrLn ("clients lost: " ++ show dropped)
  return $ concat clients'


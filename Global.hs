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
 - This module contains global definitions to be used in all other modules.
 -
 -----------------------------------------------------------------------------}

{-|
  This module contains global definitions to be used in all other modules.
 -}
module Global where

import System.IO
  (Handle)
import Control.Concurrent.STM
  (TChan)


-- TODO
type Command = String

{-|
  The Jabber ID:

     * The node

     * The domain

     * The resource
 -}
type JID = (String, String, String)

{-|
  The data type for the state of the client.

  The state transition diagram:
 
  @
    START  +--------+       AUTHENTICATE        +------+
   ------> | UNAUTH | ------------------------> | AUTH |
           +--------+                           +------+
               |                                   |
               |                                   |
               |                                   |
               |   REJECT                 ERROR    |
               +-------------+        +------------+
                             |        |   CLOSE
                             |        |
                             |        |
                             |        |
                             V        V
                          CLOSE CONNECTION
  @
 -}
data State
  = Unauth      -- ^ The Unauthenticated state
  | Auth        -- ^ The Authenticated state

{-
 -}

{-|
  The data type with 'Client' information, i.e. the command channel that is
  used for communication with the main thread, the handle of the socket the
  client is connected to, the state of the client, and the client JID.
 -}
type Client = (TChan Command, Handle, State, JID)

{-|
  This is the constructor for the client data type. It takes the command
  channel and the client handler thread and returns a default client
  structure.
 -}
initClient :: TChan Command    -- ^ The channel the client sends commands to
           -> Handle           -- ^ The handle of the client processing thread
           -> Client           -- ^ An initialized client
initClient command handle = (command, handle, Unauth, ("", "", ""))

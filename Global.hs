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
 - This module contains global definitions to be used in all other modules
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

{-|
  The data type with 'Client' information, i.e. the channel that is used for
  communication with the main thread and handle of the socket the client is
  connected to.
 -}
type Client
 = (TChan Command, Handle, String)   -- ^ The pair with the channel that is used for
                             --   communication with the main thread and the
                             --   handle of the socket the client is connected
                             --   to

-- TODO
type Command = String



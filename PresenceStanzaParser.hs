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
 - This module contains the parser of presence stanza XMPP messages.
 -
 -----------------------------------------------------------------------------}

{-|
  This module contains the parser of presence stanza XMPP messages. See RFC
  3921 for details.
 -}
module PresenceStanzaParser (processPresence) where

import Control.Concurrent.STM
  (TChan, writeTChan, atomically)

import Text.XML.HaXml.SAX
  (SaxElement(..), saxParse)
import Text.XML.HaXml
  (Attribute, AttValue(..), Reference)

import Global
import ParserGlobal



processPresence :: String
                -> [Attribute]
                -> TChan Command
                -> [Maybe SaxElement]
                -> IO (Maybe [Maybe SaxElement])
processPresence prefix attrs chan elements = return $ Just elements


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
 - This module contains the parser of message stanza XMPP messages.
 -
 -----------------------------------------------------------------------------}

{-|
  This module contains the parser of message stanza XMPP messages. See RFC
  3921 for details.
 -}
module MessageStanzaParser (processMessage) where

import Control.Concurrent.STM
  (TChan, writeTChan, atomically)

import Text.XML.HaXml.SAX
  (SaxElement(..), saxParse)
import Text.XML.HaXml
  (Attribute, AttValue(..), Reference)

import Global
import ParserGlobal



processMessage :: String
               -> [Attribute]
               -> TChan Command
               -> [Maybe SaxElement]
               -> IO (Maybe [Maybe SaxElement])
processMessage prefix attrs chan elements = return $ Just elements


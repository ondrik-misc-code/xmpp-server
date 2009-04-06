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
 - This module contains the parser of IQ stanza XMPP messages.
 -
 -----------------------------------------------------------------------------}

{-|
  This module contains the parser of IQ stanza XMPP messages. See RFC 3920 for
  details.
 -}
module IQStanzaParser (processIq) where


import Control.Concurrent.STM
  (TChan, writeTChan, atomically)

import Text.XML.HaXml.SAX
  (SaxElement(..), saxParse)
import Text.XML.HaXml
  (Attribute, AttValue(..), Reference)

import Global
import ParserGlobal



processIq :: String
          -> [Attribute]
          -> TChan Command
          -> [Maybe SaxElement]
          -> IO ()
processIq prefix attrs chan elements = do
  safelyWorkWithAttribute attrs chan "id" $
    (\x -> processIqWithId prefix attrs chan elements x)
  where processIqWithId prefix attrs chan elements id =
          case (attrs `getValueOfAttribute` "id") of
            Nothing -> sendCommand chan $
              Error "An IQ stanza does not include \"id\" attribute!"
            (Just id) -> safely chan elements $
              (\x xs -> case x of
                (SaxElementOpen name new_attrs) -> do   -- opening XML tag
                  if (matchesStringForPrefix name "query" prefix) then
                      processAuthQuery prefix new_attrs chan elements
                    else
                      sendCommand chan $ Error ("Unknown IQ stanza: " ++ name)
                (SaxElementTag name new_attrs) -> do    -- empty XML tag
                  debugInfo $ "Empty tag: " ++ showSax x
                (SaxCharData _) -> do                   -- CDATA
                  processIqWithId prefix attrs chan xs id
                _ -> do                                 -- other
                  debugInfo $ "Malformed stream!" ++ showSax x
                  sendCommand chan $ Error "Malformed stream!"
              )


processAuthQuery :: String
                 -> [Attribute]
                 -> TChan Command
                 -> [Maybe SaxElement]
                 -> IO ()
processAuthQuery prefix attrs chan elements =
  safelyWorkWithAttribute attrs chan "xmlns" $
    (\xmlns ->
      if (xmlns /= authNamespace) then
          -- in case the namespace does not match the authentication namespace
          sendCommand chan $ Error "Invalid namespace!"
        else
          processAuthQueryWithXmlns prefix attrs chan elements
    )
  where processAuthQueryWithXmlns prefix attrs chan elements =
          safely chan elements $
            (\x xs -> case x of
              (SaxElementOpen name new_attrs) -> do     -- opening XML tag
                debugInfo $ "At " ++ name
              (SaxElementTag name new_attrs) -> do      -- empty XML tag
                debugInfo $ "Empty tag: " ++ showSax x
              (SaxCharData _) -> do                     -- CDATA
                processAuthQueryWithXmlns prefix attrs chan xs
              _ -> do                                   -- other
                debugInfo $ "Malformed stream!" ++ showSax x
                sendCommand chan $ Error "Malformed stream!"
            )

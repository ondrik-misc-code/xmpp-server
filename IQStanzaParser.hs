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
          -> IO (Maybe [Maybe SaxElement])
processIq prefix attrs chan elements = do
  safelyWorkWithAttribute attrs chan "id" $
    (\x -> processIqWithId prefix attrs chan elements x)
  where processIqWithId :: String
                        -> [Attribute]
                        -> TChan Command
                        -> [Maybe SaxElement]
                        -> String
                        -> IO (Maybe [Maybe SaxElement])
        processIqWithId prefix attrs chan elements ident =
          case (attrs `getValueOfAttribute` "id") of
            Nothing -> do
              sendCommand chan $
                Error "An IQ stanza does not include \"id\" attribute!"
              return Nothing
            (Just id) -> safely chan elements $
              (\x xs -> case x of
                (SaxElementOpen name n_attrs) -> do   -- opening XML tag
                  if (matchesStringForPrefix name "query" prefix) then do
                    -- in case of IQ stanza query
                      processAuthQuery prefix n_attrs chan xs ident
                    else do
                      sendCommand chan $ Error ("Unknown IQ stanza: " ++ name)
                      return Nothing
                (SaxElementTag name n_attrs) -> do    -- empty XML tag
                  if (matchesStringForPrefix name "query" prefix) then do
                    -- in case of empty IQ stanza query
                      --
                      -- TODO: check xml namespace!
                      --
                      sendCommand chan $ Authenticate initAuthStruct ident
                      -- consume the remaining closing IQ tag
                      rem_elements <- consumeClosingTag prefix chan xs "iq"
                      case rem_elements of
                        Nothing -> do            -- an error
                          return Nothing
                        (Just xss) -> do         -- everything allright
                          return $ Just xss
                    else do
                      sendCommand chan $ Error ("Unknown IQ stanza: " ++ name)
                      return Nothing
                (SaxCharData _) -> do                   -- ignore CDATA
                  processIqWithId prefix attrs chan xs ident
                _ -> do                                 -- other
                  debugInfo $ "Malformed stream!" ++ showSax x
                  sendCommand chan $ Error "Malformed stream!"
                  return Nothing
              )


{-|
  This function processes an authentication query. See XEP-0078 for further
  details.
 -}
processAuthQuery :: String
                 -> [Attribute]
                 -> TChan Command
                 -> [Maybe SaxElement]
                 -> String
                 -> IO (Maybe [Maybe SaxElement])
processAuthQuery prefix attrs chan elements ident =
  safelyWorkWithAttribute attrs chan "xmlns" $
    (\xmlns ->
      if (xmlns /= authNamespace) then do
          -- in case the namespace does not match the authentication namespace
          sendCommand chan $ Error "Invalid namespace!"
          return Nothing
        else
          processAuthQueryWithXmlns prefix chan elements ident initAuthStruct
    )
  where processAuthQueryWithXmlns :: String
                                  -> TChan Command
                                  -> [Maybe SaxElement]
                                  -> String
                                  -> AuthStruct
                                  -> IO (Maybe [Maybe SaxElement])
        processAuthQueryWithXmlns prefix chan elements ident auths =
          safely chan elements $
            (\x xs -> case x of
              (SaxElementOpen name new_attrs) -> do     -- opening XML tag
                if (matchesStringForPrefix name "username" prefix) then do
                    debugInfo $ "Username"
                    strList <- stringPlusList xs
                    case (strList) of
                      Nothing -> do                     -- no CDATA contents
                        sendCommand chan $
                          Error "The username tag has no CDATA contents!"
                        return Nothing
                      (Just (str, xss)) -> do           -- with CDATA contents
                        remaining <- consumeClosingTag prefix chan xss "username"
                        case remaining of
                          Nothing -> return Nothing
                          (Just xsss) -> do
                            rem_elements <- processAuthQueryWithXmlns prefix chan
                              xsss ident (auths `setUsername` str)
                            return rem_elements
                  else if (matchesStringForPrefix name "password" prefix) then do
                    debugInfo $ "Password"
                    strList <- stringPlusList xs
                    case (strList) of
                      Nothing -> do                     -- no CDATA contents
                        sendCommand chan $
                          Error "The password tag has no CDATA contents!"
                        return Nothing
                      (Just (str, xss)) -> do           -- with CDATA contents
                        rem_elements <- processAuthQueryWithXmlns prefix chan
                          xss ident (auths `setPassword` str)
                        return rem_elements
                  else if (matchesStringForPrefix name "resource" prefix) then do
                    debugInfo $ "Resource"
                    strList <- stringPlusList xs
                    case (strList) of
                      Nothing -> do                     -- no CDATA contents
                        sendCommand chan $
                          Error "The resource tag has no CDATA contents!"
                        return Nothing
                      (Just (str, xss)) -> do           -- with CDATA contents
                        rem_elements <- processAuthQueryWithXmlns prefix chan
                          xss ident (auths `setResource` str)
                        return rem_elements
                  else do
                    sendCommand chan $
                      Error $ "Invalid tag in authentication query: " ++ name
                    return Nothing
                --
                -- TODO: do something
                --
              (SaxElementClose name) ->                 -- closing XML tag
                if (matchesStringForPrefix name "query" prefix) then do
                    sendCommand chan $ Authenticate auths ident
                    return $ Just xs
                  else do
                    sendCommand chan $ Error $ "Malformed XML! "
                      ++ "Invalid XML close tag at processAuthQueryWithXmlns: "
                      ++ name
                    return Nothing
              (SaxElementTag name new_attrs) -> do      -- empty XML tag
                debugInfo $ "Empty tag: " ++ showSax x
                return Nothing
              (SaxCharData _) -> do                     -- ignore CDATA
                rem_elements <- processAuthQueryWithXmlns prefix chan xs ident auths
                return rem_elements
              _ -> do                                   -- other
                debugInfo $ "Malformed stream!" ++ showSax x
                sendCommand chan $ Error "Malformed stream!"
                return Nothing
            )
          where stringPlusList :: [Maybe SaxElement]
                               -> IO (Maybe (String, [Maybe SaxElement]))
                stringPlusList elems = safelyGetContentString chan elems


{-
emptyAuthQuery :: String
               -> TChan Command
               -> [Maybe SaxElement]
               -> String
               -> IO (Maybe [Maybe SaxElement])
emptyAuthQuery prefix chan elements ident = do
  debugInfo $ "Empty authentication query!"
  remaining_tags <- consumeClosingIqTag prefix chan elements
  sendCommand chan $ SendAuthFields ident
  return remaining_tags
-}


{-|
  This function safely consumes the given tag.
 -}
consumeClosingTag :: String                        -- ^ The stream prefix
                  -> TChan Command                 -- ^ Channel for commands
                  -> [Maybe SaxElement]            -- ^ List of SAX events
                  -> String                        -- ^ Tag name
                  -> IO (Maybe [Maybe SaxElement]) -- ^ The return value
consumeClosingTag prefix chan elements tag =
  safely chan elements $
    (\x xs -> case x of
      (SaxElementClose name) -> do    -- closing XML tag
        if (matchesStringForPrefix name tag name) then
            return $ Just xs
          else do
            debugInfo $ "Malformed stream!" ++ showSax x
            sendCommand chan $ Error "Malformed stream!"
            return $ Just xs
      (SaxCharData _) -> do           -- ignore CDATA
        consumeClosingTag prefix chan xs tag
      _ -> do                         -- other
        debugInfo $ "Malformed stream!" ++ showSax x
        sendCommand chan $ Error "Malformed stream!"
        return Nothing
    )

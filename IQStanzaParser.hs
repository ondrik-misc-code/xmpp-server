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
  (TChan)

import Text.XML.HaXml.SAX
  (SaxElement(..))
import Text.XML.HaXml
  (Attribute)

import Global
import ParserGlobal


processIq :: String
          -> [Attribute]
          -> TChan Command
          -> [Maybe SaxElement]
          -> IO (Maybe [Maybe SaxElement])
processIq prefix attrs chan elements = do
  safelyWorkWithAttribute attrs chan "id" $
    (\x -> processIqWithId prefix chan elements x)
  where processIqWithId :: String
                        -> TChan Command
                        -> [Maybe SaxElement]
                        -> String
                        -> IO (Maybe [Maybe SaxElement])
        processIqWithId pref ch elems ident =
          safely ch elems $
            (\x xs -> case x of
              (SaxElementOpen name n_attrs) -> do   -- opening XML tag
                safelyWorkWithAttribute n_attrs ch "xmlns" $
                  (\xmlns ->
                    if (xmlns == authNamespace) then do
                        -- in case the namespace does not match the authentication namespace
                        remaining <- processAuthQuery name pref ch xs ident
                        case remaining of
                          Nothing -> return Nothing
                          (Just xss) -> do
                            rem_no_iq <- consumeClosingTag pref ch xss "iq"
                            return rem_no_iq
                      else if (xmlns == rosterNamespace) then do
                        sendCommand ch $ SendRoster ident
                        remaining <- consumeClosingTag pref ch xs "iq"
                        return remaining
                      else do
                        sendCommand ch $ UnknownIqNamespace xmlns ident
                        remaining <- consumeTagsUpTo pref ch xs "iq"
                        return remaining
                  )
              (SaxElementTag _ n_attrs) -> do       -- empty XML tag
                safelyWorkWithAttribute n_attrs ch "xmlns" $
                  (\xmlns ->
                    if (xmlns == authNamespace) then do
                        -- in case the namespace does not match the authentication namespace
                        sendCommand ch $ Authenticate initAuthStruct ident
                        rem_elements <- consumeClosingTag pref ch xs "iq"
                        return rem_elements
                      else if (xmlns == rosterNamespace) then do
                        debugInfo $ "Huraaa roster"
                        sendCommand ch $ SendRoster ident
                        remaining <- consumeClosingTag pref ch xs "iq"
                        return remaining
                      else do
                        sendCommand ch $ UnknownIqNamespace xmlns ident
                        remaining <- consumeTagsUpTo pref ch xs "iq"
                        return remaining
                  )
              (SaxCharData _) -> do                   -- ignore CDATA
                processIqWithId pref ch xs ident
              _ -> do                                 -- other
                debugInfo $ "Malformed stream!" ++ showSax x
                sendCommand ch $ Error "Malformed stream!"
                return Nothing
            )


{-|
  This function processes an authentication query. See XEP-0078 for further
  details.
 -}
processAuthQuery :: String
                 -> String
                 -> TChan Command
                 -> [Maybe SaxElement]
                 -> String
                 -> IO (Maybe [Maybe SaxElement])
processAuthQuery name prefix chan elements ident =
  if (matchesStringForPrefix name "query" prefix) then do
    -- in case of IQ stanza query
      processAuthQueryWithQuery prefix chan elements ident initAuthStruct
    else do
      sendCommand chan $ Error "Query tag expected!"
      return Nothing
  where processAuthQueryWithQuery :: String
                                  -> TChan Command
                                  -> [Maybe SaxElement]
                                  -> String
                                  -> AuthStruct
                                  -> IO (Maybe [Maybe SaxElement])
        processAuthQueryWithQuery pref ch elems iden auths =
          safely ch elems $
            (\x xs -> case x of
              (SaxElementOpen n_name _) -> do           -- opening XML tag
                if (matchesStringForPrefix n_name "username" pref) then do
                    debugInfo $ "Username"
                    strList <- stringPlusList ch xs
                    case (strList) of
                      Nothing -> do                     -- no CDATA contents
                        sendCommand ch $
                          Error "The username tag has no CDATA contents!"
                        return Nothing
                      (Just (str, xss)) -> do           -- with CDATA contents
                        remaining <- consumeClosingTag pref ch xss "username"
                        case remaining of
                          Nothing -> return Nothing
                          (Just xsss) -> do
                            rem_elements <- processAuthQueryWithQuery pref ch
                              xsss iden (auths `setUsername` str)
                            return rem_elements
                  else if (matchesStringForPrefix n_name "password" pref) then do
                    debugInfo $ "Password"
                    strList <- stringPlusList ch xs
                    case (strList) of
                      Nothing -> do                     -- no CDATA contents
                        sendCommand ch $
                          Error "The password tag has no CDATA contents!"
                        return Nothing
                      (Just (str, xss)) -> do           -- with CDATA contents
                        remaining <- consumeClosingTag pref ch xss "password"
                        case remaining of
                          Nothing -> return Nothing
                          (Just xsss) -> do
                            rem_elements <- processAuthQueryWithQuery pref ch
                              xsss iden (auths `setPassword` str)
                            return rem_elements
                  else if (matchesStringForPrefix n_name "resource" pref) then do
                    debugInfo $ "Resource"
                    strList <- stringPlusList ch xs
                    case (strList) of
                      Nothing -> do                     -- no CDATA contents
                        sendCommand ch $
                          Error "The resource tag has no CDATA contents!"
                        return Nothing
                      (Just (str, xss)) -> do           -- with CDATA contents
                        remaining <- consumeClosingTag pref ch xss "resource"
                        case remaining of
                          Nothing -> return Nothing
                          (Just xsss) -> do
                            rem_elements <- processAuthQueryWithQuery pref ch
                              xsss iden (auths `setResource` str)
                            return rem_elements
                  else do
                    sendCommand ch $
                      Error $ "Invalid tag in authentication query: " ++ name
                    return Nothing
                --
                -- TODO: do something
                --
              (SaxElementClose n_name) ->                 -- closing XML tag
                if (matchesStringForPrefix n_name "query" pref) then do
                    sendCommand ch $ Authenticate auths ident
                    return $ Just xs
                  else do
                    sendCommand ch $ Error $ "Malformed XML! "
                      ++ "Invalid XML close tag at processAuthQueryWithXmlns: "
                      ++ n_name
                    return Nothing
              (SaxElementTag n_name _) -> do            -- empty XML tag
                debugInfo $ "Empty tag: " ++ n_name
                return Nothing
              (SaxCharData _) -> do                     -- ignore CDATA
                rem_elements <- processAuthQueryWithQuery pref ch xs iden auths
                return rem_elements
              _ -> do                                   -- other
                debugInfo $ "Malformed stream!" ++ showSax x
                sendCommand ch $ Error "Malformed stream!"
                return Nothing
            )


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

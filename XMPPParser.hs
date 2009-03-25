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
 - This module contains the parser of XMPP messages.
 -
 -----------------------------------------------------------------------------}


{-|
 This module contains the parser of XMPP messages. See RFC 3920 for details.
 -}
module XMPPParser (parseXMPP) where

import Text.XML.HaXml.SAX
  (SaxElement(..), saxParse)
import Text.XML.HaXml
  (Attribute, AttValue(..), Reference)
import Control.Concurrent.STM
  (TChan, writeTChan, atomically)

import Global

{-|
  The 'parseXMPP' function parser the client XMPP input and sends commands according to the parsed data to the main loop.
 -}
parseXMPP :: String          -- ^ The string that is loaded from the client socket
          -> TChan Command   -- ^ The channel for sending data to the command loop
          -> IO ()           -- ^ The return value
parseXMPP xmlstring chan = do
  putStrLn "Parsing..."
  atomically $ writeTChan chan "ahoj"
  putStrLn xmlstring
  printParse xmlstring

printParse :: String -> IO ()
printParse = putStr . showParse

{- this could maybe be used at some point... (when sending commands to the
 - main loop
listenLoop :: IO a -> TChan a -> IO ()
listenLoop act chan =
  sequence_ (repeat (act >>= atomically . writeTChan chan))
-}

showParse str = foldr ((++) . showSax) "" . fst $ runSax str

runSax xmlsource = saxParse "" xmlsource

{-|
  The 'showSax' function is a debug function for transformation of an
  arbitrary SAX element into a string.
 -}
showSax :: SaxElement     -- ^ The SAX element
        -> String         -- ^ The string representation of the SAX element
showSax element =
  case element of
    (SaxComment comment) ->
      "Comment: " ++ comment ++ " INVALID XMPP-XML ELEMENT!" ++ "\n"
    (SaxElementOpen open_tag_name attrs) ->
      "Element open: " ++ (cropQuotes $ show open_tag_name)
      ++ "\n  Attributes: " ++ (showAttributes attrs) ++ "\n"
    (SaxElementClose close_tag_name) ->
      "Element close: " ++ (cropQuotes $ show close_tag_name) ++ "\n"
    (SaxElementTag tag_name attrs) ->
      "Element empty: " ++ (cropQuotes $ show tag_name) ++ "\n  Attributes: "
      ++ (showAttributes attrs) ++ "\n"
    (SaxCharData char_data) ->
      "Character data: " ++ (cropQuotes $ show char_data) ++ "\n"
    (SaxDocTypeDecl doc_type) ->
      "DocType: " ++ " INVALID XMPP-XML ELEMENT!" ++ "\n"
    (SaxProcessingInstruction (target, str)) ->
      "Processing instruction: " ++ target ++ " --- " ++ str
      ++ " INVALID XMPP-XML ELEMENT!" ++ "\n"

showAttributes attrs = foldr ((++) . (++ " ") . showAttr ) "" attrs

showAttr :: Attribute -> String
showAttr (name, AttValue value) = name ++ "=\"" ++ (showAttrValue $ head value) ++ "\""

showAttrValue :: Either String Reference -> String
showAttrValue (Left str) = str
showAttrValue _ = error "XMPP does not support XML references!"

cropQuotes :: String -> String
cropQuotes = foldr (\x y -> if (x == '\"') then y else (x:y)) ""

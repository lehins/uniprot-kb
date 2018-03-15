{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Bio.Uniprot.Parser where

import           Bio.Uniprot.Type
import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text
import           Data.Bifunctor       (second)
import           Data.Functor         (($>))
import           Data.Text            (Text, pack, unpack)
import           Data.Void

parseID :: Parser ID
parseID = do
    string "ID"
    many1 space
    entryName <- parseTextToken
    many1 space
    status <- (string "Reviewed" $> Reviewed) <|> (string "Unreviewed" $> Unreviewed)
    char ';'
    many1 space
    seqLength <- decimal
    space >> string "AA."
    pure ID{..}

parseAC :: Parser AC
parseAC = do
    initAC <- concat <$> many' (parseOneAC <* endOfLine)
    lastAC <- parseOneAC
    let accessionNumbers = initAC ++ lastAC
    pure AC{..}
  where
    parseOneAC :: Parser [Text]
    parseOneAC = do
        string "AC"
        many1 space
        many1 (parseTextToken <* char ';' <* many' space)

parseDT :: Parser DT
parseDT = do
    (dbIntegrationDate, dbName) <- parseOneDT "integrated into UniProtKB/" <* endOfLine
    (seqVersionDate, seqVersion) <- second (read . unpack) <$> parseOneDT "sequence version " <* endOfLine
    (entryVersionDate, entryVersion) <- second (read . unpack) <$> parseOneDT "entry version "
    pure DT{..}
  where
    parseOneDT :: Text -> Parser (Text, Text)
    parseOneDT txt = do
        string "DE"
        many1 space
        day <- pack <$> many1 (satisfy $ inClass "A-Z0-9-")
        char ','
        many1 space
        string txt
        x <- pack <$> many1 (satisfy $ inClass "A-Za-z0-9_-")
        char '.'
        pure (day, x)

parseDE :: Parser ()
parseDE = parseAndSkip "DE"

-- Helpers

parseAndSkip :: Text -> Parser ()
parseAndSkip txt = do
    many' (parseOneAndSkip <* endOfLine) >> parseOneAndSkip
    pure ()
  where
    parseOneAndSkip :: Parser ()
    parseOneAndSkip = do
        string txt
        many1 anyChar
        pure ()
  
parseTextToken :: Parser Text
parseTextToken = pack <$> many1 (satisfy $ inClass "A-Z0-9_")

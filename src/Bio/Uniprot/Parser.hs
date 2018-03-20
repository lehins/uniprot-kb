{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Bio.Uniprot.Parser where

import           Prelude              hiding (null)

import           Bio.Uniprot.Type
import           Control.Applicative  ((<|>))
import           Control.Monad        (unless, when)
import           Data.Attoparsec.Text
import           Data.Bifunctor       (second)
import           Data.Functor         (($>))
import           Data.Text            (Text, append, init, null, pack, unpack,
                                       unwords)
import           Data.Void

import           Debug.Trace          (trace)

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
    parseStartAC
    initAC <- concat <$> many' (parseOneAC <* endOfLine <* parseStartAC)
    lastAC <- parseOneAC
    let accessionNumbers = initAC ++ lastAC
    pure AC{..}
  where
    parseStartAC :: Parser ()
    parseStartAC = string "AC" >> count 3 space >> pure ()

    parseOneAC :: Parser [Text]
    parseOneAC = many1 (parseTextToken <* char ';' <* option ' ' (satisfy isHorizontalSpace))

parseDT :: Parser DT
parseDT = do
    (dbIntegrationDate, dbName) <- parseOneDT "integrated into UniProtKB/" <* endOfLine
    (seqVersionDate, seqVersion) <- second (read . unpack) <$> parseOneDT "sequence version " <* endOfLine
    (entryVersionDate, entryVersion) <- second (read . unpack) <$> parseOneDT "entry version "
    pure DT{..}
  where
    parseOneDT :: Text -> Parser (Text, Text)
    parseOneDT txt = do
        string "DT"
        many1 space
        day <- pack <$> many1 (satisfy $ inClass "A-Z0-9-")
        char ','
        many1 space
        string txt
        x <- pack <$> many1 (satisfy $ inClass "A-Za-z0-9_-")
        char '.'
        pure (day, x)

parseDE :: Parser DE
parseDE = do
    recName  <- optional $ parseNameDE "RecName"
    altNames <- many' (endOfLine *> parseAltDE)
    subNames <- many' (endOfLine *> parseNameDE "SubName")
    includes <- pure []
    contains <- pure []
    flags <- fmap (read . unpack) <$> optional (endOfLine *> parseDELine (Just "Flags") "")
    pure DE{..}

parseSQ :: Parser SQ
parseSQ = do
    string "SQ" >> count 3 space >> string "SEQUENCE"
    seqLength <- many1 space *> decimal <* space <* string "AA;"
    molWeight <- many1 space *> decimal <* space <* string "MW;"
    crc64 <- pack <$> (many1 space *> many1 (satisfy $ inClass "A-F0-9") <* space <* string "CRC64;")
    endOfLine
    sequence <- pack . concat <$> many1 (skipSpace *> parseAminoAcids)
    pure SQ{..}

parseEnd :: Parser ()
parseEnd = string "//" >> pure ()

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

parseAminoAcids :: Parser String
parseAminoAcids = many1 (satisfy $ inClass "ACDEFGHIKLMNPQRSTVWY")

parseTextToken :: Parser Text
parseTextToken = pack <$> many1 (satisfy $ inClass "A-Z0-9_")

optional :: Parser a -> Parser (Maybe a)
optional par = option Nothing (Just <$> par)

-- DE helper parsers

parseNameDE :: Text -> Parser Name
parseNameDE nameType = do
    fullName <- parseDELine (Just nameType) "Full"
    shortName <- many' $ endOfLine *> parseDELine Nothing "Short"
    ecNumber <- many' $ endOfLine *> parseDELine Nothing "EC"
    pure Name{..}

parseAltDE :: Parser AltName
parseAltDE =
  (Simple <$> parseNameDE "AltName") <|>
  (Allergen <$> parseDELine (Just "AltName") "Allergen") <|>
  (Biotech <$> parseDELine (Just "AltName") "Biotech") <|>
  (CDAntigen <$> parseDELine (Just "AltName") "CD_antigen") <|>
  (INN <$> parseDELine (Just "AltName") "INN")

parseDELine :: Maybe Text -> Text -> Parser Text
parseDELine name tpe = do
    string "DE   "
    string $ maybe "         " (`append` ": ") name
    unless (null tpe) $ do
        string tpe
        string "="
        pure ()
    res <- pack <$> many1 (notChar ';')
    char ';'
    pure res

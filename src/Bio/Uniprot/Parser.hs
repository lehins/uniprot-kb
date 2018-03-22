{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Bio.Uniprot.Parser where

import           Prelude              hiding (null)

import           Bio.Uniprot.Type
import           Control.Applicative  ((<|>), liftA2)
import           Control.Monad        (unless, when)
import           Data.Attoparsec.Text
import           Data.Bifunctor       (second)
import           Data.Functor         (($>))
import           Data.Text            (Text, append, init, null, pack, unpack,
                                       unwords, splitOn)
import           Data.Void

import           Debug.Trace          (trace)

-- |Describes possible name type of DE section.
data NameType = RecName | AltName | SubName | Flags | None
  deriving (Show)

-- |Parses ID line of UniProt-KB text file.
parseID :: Parser ID
parseID = do
    string "ID"
    many1 space
    entryName <- pack <$> many1 (satisfy $ inClass "A-Z0-9_")
    many1 space
    status <- (string "Reviewed" $> Reviewed) <|>
              (string "Unreviewed" $> Unreviewed)
    char ';'
    many1 space
    seqLength <- decimal
    space >> string "AA."
    pure ID{..}

-- |Parses AC lines of UniProt-KB text file.
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
    parseOneAC = many1 $ do
        res <- pack <$> many1 (satisfy $ inClass "A-Z0-9_")
        char ';'
        option ' ' (satisfy isHorizontalSpace)
        pure res

-- |Parses 3 DT lines of UniProt-KB text file.
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

-- |Parses DE lines of UniProt-KB text file.
parseDE :: Parser DE
parseDE = do
    recName  <- optional $ parseNameDE RecName
    altNames <- many' (endOfLine *> parseAltDE)
    subNames <- many' (endOfLine *> parseNameDE SubName)
    includes <- pure []
    contains <- pure []
    flags    <- optional (endOfLine *> parseFlagsDE)
    pure DE{..}
  where
    -- |Parses name section like RecName, AltName or SubName.
    parseNameDE :: NameType -> Parser Name
    parseNameDE nameType = do
        fullName <- parseDELine nameType "Full"
        shortName <- many' $ endOfLine *> parseDELine None "Short"
        ecNumber <- many' $ endOfLine *> parseDELine None "EC"
        pure Name{..}

    -- |Parses flag line of DE section
    parseFlagsDE :: Parser Flag
    parseFlagsDE = read . unpack <$> parseDELine Flags ""

    -- |Parses AltName lines of DE section
    parseAltDE :: Parser AltName
    parseAltDE =
      (Simple <$> parseNameDE AltName) <|>
      (Allergen <$> parseDELine AltName "Allergen") <|>
      (Biotech <$> parseDELine AltName "Biotech") <|>
      (CDAntigen <$> parseDELine AltName "CD_antigen") <|>
      (INN <$> parseDELine AltName "INN")

    -- |Parses any DE line
    parseDELine :: NameType -> Text -> Parser Text
    parseDELine nameType tpe = do
        string "DE   "
        case nameType of
          None -> string "         "
          a    -> string $ append (pack $ show a) ": "
        unless (null tpe) $ do
            string tpe
            string "="
            pure ()
        res <- pack <$> many1 (notChar ';')
        char ';'
        pure res

parseGN :: Parser [GN]
parseGN = do
    string "GN   "
    geneName <- optional parseGNName
    parseGNOptionBreak
    synonyms <- option [] $ parseGNList "Synonyms"
    parseGNOptionBreak
    orderedLocusNames <- option [] $ parseGNList "OrderedLocusNames"
    parseGNOptionBreak
    orfNames <- option [] $ parseGNList "ORFNames"
    let gn = GN{..}
    rest <- option [] $ string "and" *> endOfLine *> parseGN
    pure $ gn:rest
  where
    parseGNName :: Parser Text
    parseGNName = parseGNItem "Name" (Prelude.id <$>)

    parseGNOptionBreak :: Parser ()
    parseGNOptionBreak = optional ((endOfLine *> string "GN   ") <|> " ") >> pure ()

    parseGNList :: Text -> Parser [Text]
    parseGNList name = parseGNItem name (splitOn ", " <$>)

    parseGNItem :: Text -> (Parser Text -> Parser a) -> Parser a
    parseGNItem name f = do
        string name >> char '='
        f (pack <$> many1 (notChar ';')) <* char ';'

-- |Parses OS lines for one record of UniProt-KB text file.
parseOS :: Parser OS
parseOS = OS . pack <$> parseOSStr
  where
    parseOSStr :: Parser String
    parseOSStr = do
        string "OS   "
        namePart <- many1 (satisfy $ not . isEndOfLine)
        if last namePart == '.'
          then pure $ Prelude.init namePart
          else do
              rest <- endOfLine *> parseOSStr
              pure $ namePart ++ rest

-- |Parser OG line of UniProt-KB text file.
parseOG :: Parser OG
parseOG = parseOGNonPlasmid <|> (Plasmid <$> parseOGPlasmid)
  where
    parseOGNonPlasmid :: Parser OG
    parseOGNonPlasmid = string "OG   " *>
      (string "Hydrogenosome." $> Hydrogenosome) <|>
      (string "Mitochondrion." $> Mitochondrion) <|>
      (string "Nucleomorph." $> Nucleomorph) <|>
      (string "Plastid." $> Plastid PlastidSimple) <|>
      (string "Plastid; Apicoplast." $> Plastid PlastidApicoplast) <|>
      (string "Plastid; Chloroplast." $> Plastid PlastidChloroplast) <|>
      (string "Plastid; Organellar chromatophore." $> Plastid PlastidOrganellarChromatophore) <|>
      (string "Plastid; Cyanelle." $> Plastid PlastidCyanelle) <|>
      (string "Plastid; Non-photosynthetic plastid." $> Plastid PlastidNonPhotosynthetic)
      
    parseOGPlasmid :: Parser [Text]
    parseOGPlasmid = do
        string "OG   "
        name <- parseOnePlasmid
        rest <- many' $ (string ", " <|> string ", and ") *> parseOnePlasmid
        rest2 <- (char '.' $> []) <|> (endOfLine *> parseOGPlasmid)
        pure $ name : rest ++ rest2

    parseOnePlasmid :: Parser Text
    parseOnePlasmid = do
        string "Plasmid"
        pack <$> many1 (satisfy $ liftA2 (&&) (notInClass ",.") (not . isEndOfLine))

-- |Parses SQ lines of UniProt-KB text file.
parseSQ :: Parser SQ
parseSQ = do
    string "SQ" >> count 3 space >> string "SEQUENCE"
    many1 space
    seqLength <- decimal
    space >> string "AA;"
    many1 space
    molWeight <- decimal
    space >> string "MW;"
    many1 space
    crc64 <- pack <$> many1 (satisfy $ inClass "A-F0-9")
    space >> string "CRC64;"
    endOfLine
    sequence <- pack . concat <$>
                  many1 (skipSpace *> many1 (satisfy $ inClass "ACDEFGHIKLMNPQRSTVWY"))
    pure SQ{..}

-- |Parses end of one UniProt record.
parseEnd :: Parser ()
parseEnd = string "//" >> pure ()

-- = Helper parsers

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

-- |Transforms any parser to a parser of maybe value.
-- >>> parseOnly (optional digit) "1"
-- Right (Just 1)
--
-- >>> parseOnly (optional digit) ""
-- Right Nothing
optional :: Parser a -> Parser (Maybe a)
optional par = option Nothing (Just <$> par)

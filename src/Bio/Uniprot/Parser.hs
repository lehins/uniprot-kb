{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Bio.Uniprot.Parser where

import           Prelude              hiding (null, init)
import qualified Prelude as P (init, id)

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
    string "ID   "
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
        string "DT   "
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

-- |Parses DE lines of UniProt-KB text file.
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
    -- |Parses `Name` item of GN line
    parseGNName :: Parser Text
    parseGNName = parseGNItem "Name" (P.id <$>)

    -- |Parses line break for multiline GN section
    parseGNOptionBreak :: Parser ()
    parseGNOptionBreak = option () $ ((endOfLine *> string "GN   ") <|> " ") $> ()

    -- |Parses any list item of GN line (like `Synonyms` or `ORFNames`)
    parseGNList :: Text -> Parser [Text]
    parseGNList name = parseGNItem name (splitOn ", " <$>)

    -- |Parses one item of GN line
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
          then pure $ P.init namePart
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
        let separator = string "," >> optional " and"
        rest <- many' $ separator *> space *> parseOnePlasmid
        rest2 <- (char '.' $> []) <|> (separator *> endOfLine *> parseOGPlasmid)
        pure $ name : rest ++ rest2

    parseOnePlasmid :: Parser Text
    parseOnePlasmid = do
        string "Plasmid "
        pack <$> many1 (satisfy $ liftA2 (&&) (notInClass ",.") (not . isEndOfLine))

-- |Parser OG line of UniProt-KB text file.
parseOC :: Parser OC
parseOC = parseNodes "OC" OC

-- |Parses OX lines of UniProt-KB text file.
parseOX :: Parser OX
parseOX = do
    string "OX   "
    databaseQualifier <- pack <$> many1 (notChar '=')
    char '='
    taxonomicCode <- pack <$> many1 (notChar ';')
    char ';'
    pure OX{..}

-- |Parses OH line of UniProt-KB text file.
parseOH :: Parser OH
parseOH = do
    string "OH   NCBI_TaxID="
    taxId <- pack <$> many1 (notChar ';')
    string "; "
    hostName <- pack <$> many1 (notChar '.')
    char '.'
    pure OH{..}

parseRef :: Parser [Reference]
parseRef = undefined

parseCC :: Parser [CC]
parseCC = undefined

parseDR :: Parser DR
parseDR = do
    string "DR   "
    resourceAbbr <- parseToken
    string "; "
    resourceId <- parseToken
    string "; "
    optionalInfo' <- (:) <$> parseToken <*> many' (string "; " *> parseToken)
    let optionalInfo = P.init optionalInfo' ++ [init . last $ optionalInfo']
    pure DR{..}
  where
    parseToken :: Parser Text
    parseToken = pack <$> many1 (satisfy $ liftA2 (&&) (notInClass ";") (not . isEndOfLine))

-- |Parses PE line of UniProt-KB text file.
parsePE :: Parser PE
parsePE = (string "PE   1: Evidence at protein level" $> EvidenceAtProteinLevel) <|>
          (string "PE   2: Evidence at transcript level" $> EvidenceAtTranscriptLevel) <|>
          (string "PE   3: Inferred from homology" $> InferredFromHomology) <|>
          (string "PE   4: Predicted" $> Predicted) <|>
          (string "PE   5: Uncertain" $> Uncertain)

-- |Parses KW lines of UniProt-KB text file.
parseKW :: Parser KW
parseKW = parseNodes "KW" KW

parseFT :: Parser [FT]
parseFT = do
    string "FT   "
    keyName <- pack <$> many1 (satisfy $ inClass "A-Z_")
    many' space
    pure undefined
    -- pure FT{..}
  where
    parseFTEndpoint :: Parser Endpoint
    parseFTEndpoint = (UncertainEP <$> (char '?' *> decimal)) <|>
                      (NTerminalEP <$> (char '<' *> decimal)) <|>
                      (CTerminalEP <$> (char '>' *> decimal)) <|>
                      (ExactEP     <$> decimal) <|>
                      (char '?' $> UnknownEP)

-- |Parses SQ lines of UniProt-KB text file.
parseSQ :: Parser SQ
parseSQ = do
    string "SQ   SEQUENCE"
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

-- |Parses lines, that contain nodes splitted by ';' and ended by '.'.
parseNodes :: Text -> ([Text] -> a) -> Parser a
parseNodes start f = do
    string start >> count 3 space
    name <- parseNode
    rest <- many' $ do
        string ";"
        string " " <|> (endOfLine >> string start >> count 3 space >> pure "")
        parseNode
    char '.'
    pure $ f (name:rest)
  where
    parseNode :: Parser Text
    parseNode = pack <$> many1 (satisfy $ liftA2 (&&) (notInClass ";.") (not . isEndOfLine))

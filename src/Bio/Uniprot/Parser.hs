{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Bio.Uniprot.Parser where

import           Prelude              hiding (init, null)
import qualified Prelude              as P (concat, id, init)

import           Bio.Uniprot.Type
import           Control.Applicative  (liftA2, (<|>))
import           Control.Monad        (unless, when)
import           Data.Attoparsec.Text
import           Data.Bifunctor       (second)
import           Data.Functor         (($>))
import           Data.Text            (Text, append, concat, init, null, pack,
                                       splitOn, unpack, unwords)
import           Data.Void

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
    initAC <- P.concat <$> many' (parseOneAC <* endOfLine <* parseStartAC)
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
    optional $ parseBreak "GN"
    synonyms <- option [] $ parseGNList "Synonyms"
    optional $ parseBreak "GN"
    orderedLocusNames <- option [] $ parseGNList "OrderedLocusNames"
    optional $ parseBreak "GN"
    orfNames <- option [] $ parseGNList "ORFNames"
    let gn = GN{..}
    rest <- option [] $ string "and" *> endOfLine *> parseGN
    pure $ gn:rest
  where
    -- |Parses `Name` item of GN line
    parseGNName :: Parser Text
    parseGNName = parseDefItem "Name" (P.id <$>)

    -- |Parses any list item of GN line (like `Synonyms` or `ORFNames`)
    parseGNList :: Text -> Parser [Text]
    parseGNList name = parseDefItem name (splitOn ", " <$>)

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

-- |Parser OC line of UniProt-KB text file.
parseOC :: Parser OC
parseOC = parseNodes ';' '.' "OC" OC

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

-- |Parses RN, RP, RC, RX, RG, RA, RT and RL lines of UniProt-KB text file.
parseRef :: Parser Reference
parseRef = do
    rn <- parseRN
    endOfLine
    rp <- parseRP
    endOfLine
    rc <- option [] (parseRCX STRAIN "RC" <* endOfLine)
    rx <- option [] (parseRCX MEDLINE "RX" <* endOfLine)
    rg <- optional  (parseRG <* endOfLine)
    ra <- option [] (parseNodes ',' ';' "RA" P.id <* endOfLine)
    rt <- optional  (parseRT <* endOfLine)
    rl <- parseRL
    pure Reference{..}
  where
    parseRN :: Parser Int
    parseRN = (string "RN   [" *> decimal) <* char ']'

    parseRP :: Parser Text
    parseRP = do
        string "RP   "
        pack . P.init <$> parseMultiLineComment "RP" 3

    parseRCX :: (Enum a, Show a) => a -> Text -> Parser [(a, Text)]
    parseRCX start name = do
       string name >> string "   "
       (:) <$> parseTokPair start
           <*> many' (parseBreak name *> parseTokPair start)
     where
       parseTokPair :: (Enum a, Show a) => a -> Parser (a, Text)
       parseTokPair x = foldl1 (<|>) $
                          (\x -> (x,) <$> parseDefItem (pack . show $ x) (P.id <$>)) <$> [x..]

    parseRG :: Parser Text
    parseRG = pack <$> (string "RG   " *> many1 (satisfy $ not . isEndOfLine))

    parseRT :: Parser Text
    parseRT = do
        string "RT   \""
        let p = many1 $ satisfy $ liftA2 (&&) (not . isEndOfLine) (notInClass "\"")
        referenceTitle <- (:) <$> p <*> many' (endOfLine *> string "RT  " *> p)
        string "\";"
        pure $ pack . hyphenConcat $ referenceTitle

    parseRL :: Parser Text
    parseRL = do
        string "RL   "
        pack . P.init <$> parseMultiLineComment "RL" 3

-- |Parses CC lines of UniProt-KB text file.
parseCC :: Parser CC
parseCC = do
    string "CC   -!- "
    topic <- pack <$> many1 (notChar ':')
    char ':'
    (char ' ' $> ()) <|> (endOfLine >> string "CC" >> count 7 space $> ())
    comment <- pack <$> parseMultiLineComment "CC" 7
    pure CC{..}

-- |Parses DR lines of UniProt-KB text file.
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
parsePE = (string "PE   1: Evidence at protein level;" $> EvidenceAtProteinLevel) <|>
          (string "PE   2: Evidence at transcript level;" $> EvidenceAtTranscriptLevel) <|>
          (string "PE   3: Inferred from homology;" $> InferredFromHomology) <|>
          (string "PE   4: Predicted;" $> Predicted) <|>
          (string "PE   5: Uncertain;" $> Uncertain)

-- |Parses KW lines of UniProt-KB text file.
parseKW :: Parser KW
parseKW = parseNodes ';' '.' "KW" KW

-- |Parses FT lines of UniProt-KB text file. One FT section is parsed.
parseFT :: Parser FT
parseFT = do
    string "FT   "
    keyName <- pack <$> many1 (satisfy $ inClass "A-Z_")
    many1 space
    fromEP <- parseFTEndpoint
    many1 space
    toEP <- parseFTEndpoint
    many1 space
    description <- splitByMagic <$> parseMultiLineComment "FT" 32
    pure FT{..}
  where
    -- |Parse FT endpoint
    parseFTEndpoint :: Parser Endpoint
    parseFTEndpoint = (UncertainEP <$> (char '?' *> decimal)) <|>
                      (NTerminalEP <$> (char '<' *> decimal)) <|>
                      (CTerminalEP <$> (char '>' *> decimal)) <|>
                      (ExactEP     <$> decimal) <|>
                      (char '?' $> UnknownEP)

    -- |Split string to tokens by periods outside brackets.
    splitByMagic :: String -> [Text]
    splitByMagic txt = pack <$> splitStr 0 [] txt
      where
        splitStr :: Int -> String -> String -> [String]
        splitStr 0 acc ['.']        = [reverse acc]
        splitStr 0 acc ('.':' ':xs) = reverse acc : splitStr 0 [] xs
        splitStr 0 acc ('.':xs)     = reverse acc : splitStr 0 [] xs
        splitStr n acc ('(':xs)     = splitStr (n+1) ('(':acc) xs
        splitStr n acc (')':xs)     = splitStr (n-1) (')':acc) xs
        splitStr n acc (x:xs)       = splitStr n (x:acc) xs

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
    sequence <- pack . P.concat <$>
                  many1 (skipSpace *> many1 (satisfy $ inClass "ACDEFGHIKLMNPQRSTVWY"))
    pure SQ{..}

-- |Parses end of one UniProt record.
parseEnd :: Parser ()
parseEnd = string "//" >> pure ()

-- |Parses whole UniProt-KB record.
parseRecord :: Parser Record
parseRecord = Record <$>           (parseID  <* endOfLine)
                     <*>           (parseAC  <* endOfLine)
                     <*>           (parseDT  <* endOfLine)
                     <*>           (parseDE  <* endOfLine)
                     <*> option [] (parseGN  <* endOfLine)
                     <*>           (parseOS  <* endOfLine)
                     <*> optional  (parseOG  <* endOfLine)
                     <*>           (parseOC  <* endOfLine)
                     <*>           (parseOX  <* endOfLine)
                     <*> many'     (parseOH  <* endOfLine)
                     <*> many'     (parseRef <* endOfLine)
                     <*> many'     (parseCC  <* endOfLine)
                     <*> many'     (parseDR  <* endOfLine)
                     <*>           (parsePE  <* endOfLine)
                     <*> optional  (parseKW  <* endOfLine)
                     <*> many'     (parseFT  <* endOfLine)
                     <*>           (parseSQ  <* endOfLine)
                     <*            parseEnd

-- = Helper parsers

-- |Transforms any parser to a parser of maybe value.
--
-- >>> parseOnly (optional digit) "1"
-- Right (Just 1)
--
-- >>> parseOnly (optional digit) ""
-- Right Nothing
optional :: Parser a -> Parser (Maybe a)
optional par = option Nothing (Just <$> par)

-- |Parses lines, that contain nodes splitted by ';' and ended by '.'.
parseNodes :: Char          -- ^Delimeter char, that splits the nodes.
           -> Char          -- ^Terminal char, that ends the node list.
           -> Text          -- ^Start 2-letter mark.
           -> ([Text] -> a) -- ^Text modifier
           -> Parser a
parseNodes del end start f = do
    string start >> count 3 space
    name <- parseNode
    rest <- many' $ do
        char del
        string " " <|> (endOfLine >> string start >> count 3 space >> pure "")
        parseNode
    char end
    pure $ f (name:rest)
  where
    parseNode :: Parser Text
    parseNode = pack <$> many1 (satisfy $ liftA2 (&&) (notInClass [del,end]) (not . isEndOfLine))

-- |Parses line till the end.
parseTillEnd :: Parser String
parseTillEnd = many1 $ satisfy (not . isEndOfLine)

-- |Parses multiline comment as one string.
parseMultiLineComment :: Text -> Int -> Parser String
parseMultiLineComment start skip = do
    comm <- (:) <$> parseTillEnd
                <*> many' (do endOfLine
                              string start
                              count (skip - 1) (char ' ') -- leave one space to separate words
                              parseTillEnd)
    pure $ hyphenConcat comm

-- |Parses line break for multiline section.
parseBreak :: Text -> Parser ()
parseBreak txt = ((endOfLine >> string txt >> string "   ") <|> string " ") $> ()

-- |Parses one item like "Something=Something else;"
parseDefItem :: Text -> (Parser Text -> Parser a) -> Parser a
parseDefItem name f = do
    string name >> char '='
    f (pack <$> many1 (notChar ';')) <* char ';'

-- |Delete needless space after hyphen on concat.
hyphenConcat :: [String] -> String
hyphenConcat []       = []
hyphenConcat [x]      = x
hyphenConcat (x:y:ys) = x ++ hyphenConcat (sy:ys)
  where
    sy :: String
    sy | last x == '-'                  = tail y
       | isAA (last x) && isAA (y !! 1) = tail y
       | otherwise                      = y
    
    isAA :: Char -> Bool
    isAA = inClass "ACDEFGHIKLMNPQRSTVWY"

{-# LANGUAGE DuplicateRecordFields #-}
module Bio.Uniprot.Type where

import           Data.Text          (Text)

-- |Which taxonomic 'kingdom' an
-- organism belongs to.
data Kingdom
  = Archea    -- ^'A' for archaea (=archaebacteria)
  | Bacteria  -- ^'B' for bacteria (=prokaryota or eubacteria)
  | Eukaryota -- ^'E' for eukaryota (=eukarya)
  | Virus     -- ^'V' for viruses and phages (=viridae)
  | Other     -- ^'O' for others (such as artificial sequences)
  deriving (Show, Eq, Ord, Bounded, Enum)

-- |Controlled vocabulary of species
data Organism = Organism
  { code         :: Text       -- ^Code of organism (up to 5 symbols)
  , kingdom      :: Kingdom    -- ^Kingdom of organism
  , officialName :: Text       -- ^Official (scientific) name
  , commonName   :: Maybe Text -- ^Common name
  , synonym      :: Maybe Text -- ^Synonym name
  } deriving (Show, Eq, Ord)

-- |To distinguish the fully annotated entries in the Swiss-Prot
-- section of the UniProt Knowledgebase from the computer-annotated
-- entries in the TrEMBL section, the 'status' of each entry is
-- indicated in the first (ID) line of each entry
data Status
  = Reviewed   -- ^Entries that have been manually reviewed and annotated by UniProtKB curators
  | Unreviewed -- ^Computer-annotated entries that have not been reviewed by UniProtKB curators
  deriving (Show, Eq, Ord, Bounded, Enum)

-- |IDentification
data ID = ID
  { entryName :: Text   -- ^This name is a useful means of identifying a sequence, but it is not a stable identifier as is the accession number.
  , status    :: Status -- ^The status of the entry
  , seqLength :: Int    -- ^The length of the molecule, which is the total number of amino acids in the sequence. This number includes the positions reported to be present but which have not been determined (coded as 'X').
  } deriving (Show, Eq, Ord)

-- |ACcession numbers.
-- The purpose of accession numbers is to provide a stable way of
-- identifying entries from release to release. It is sometimes
-- necessary for reasons of consistency to change the names of the
-- entries, for example, to ensure that related entries have similar
-- names. However, an accession number is always conserved, and
-- therefore allows unambiguous citation of entries.
-- Researchers who wish to cite entries in their publications should
-- always cite the first accession number. This is commonly referred
-- to as the 'primary accession number'. 'Secondary accession numbers'
-- are sorted alphanumerically.
newtype AC = AC
  { accessionNumbers :: [Text]
  } deriving (Show, Eq, Ord)

-- |DaTe: the date of creation and last modification of the database
-- entry.
data DT = DT
  { dbIntegrationDate :: Text -- ^Indicates when the entry first appeared in the database.
  , dbName            :: Text -- ^Indicates in which section of UniProtKB, Swiss-Prot or TrEMBL, the entry can be found.
  , seqVersionDate    :: Text -- ^Indicates when the sequence data was last modified.
  , seqVersion        :: Int  -- ^The sequence version number of an entry is incremented by one when the amino acid sequence shown in the sequence record is modified.
  , entryVersionDate  :: Text -- ^Indicates when data other than the sequence was last modified.
  , entryVersion      :: Int  -- ^The entry version number is incremented by one whenever any data in the flat file representation of the entry is modified.
  } deriving (Show, Eq, Ord)

data RecName = RecName
  { fullName  :: Text   -- ^The full name.
  , shortName :: [Text] -- ^A set of abbreviations of the full name or acronyms.
  , ecNumber  :: [Int]  -- ^A set of Enzyme Commission numbers.
  } deriving (Show, Eq, Ord)

data AltName = AltName
  { fullName  :: Maybe Text -- ^The full name.
  , shortName :: [Text]     -- ^A set of abbreviations of the full name or acronyms.
  , ecNumber  :: [Int]      -- ^A set of Enzyme Commission numbers.
  , allergen  :: Maybe Text -- ^Antigens (allergens) that cause IgE-mediated atopic allergies in humans.
  , biotech   :: Maybe Text -- ^A name used in a biotechnological context.
  , cdAntigen :: [Text]     -- ^??
  , inn       :: [Text]     -- ^The international nonproprietary name: A generic name for a pharmaceutical substance or active pharmaceutical ingredient that is globally recognized and is a public property.
  } deriving (Show, Eq, Ord)

data SubName = SubName
  { fullName :: Text  -- ^The full name.
  , ecNumber :: [Int] -- ^A set of Enzyme Commission numbers.
  } deriving (Show, Eq, Ord)

data Flag
  = Fragment  -- ^The complete sequence is not determined.
  | Fragments -- ^The complete sequence is not determined.
  | Precursor -- ^The sequence displayed does not correspond to the mature form of the protein.
  deriving (Show, Eq, Ord, Bounded, Enum)

-- |DEscription - general descriptive information about the sequence
-- stored.
data DE = DE
  { recName  :: Maybe RecName -- ^The name recommended by the UniProt consortium.
  , altNames :: [AltName]     -- ^A synonym of the recommended name.
  , subNames :: [SubName]     -- ^A name provided by the submitter of the underlying nucleotide sequence.
  , includes :: [DE]          -- ^A protein is known to include multiple functional domains each of which is described by a different name.
  , contains :: [DE]          -- ^The functional domains of an enzyme are cleaved, but the catalytic activity can only be observed, when the individual chains reorganize in a complex.
  , flags    :: [Flag]        -- ^Flags whether the entire is a precursor or/and a fragment.
  } deriving (Show, Eq, Ord)

-- |Gene Name - the name(s) of the gene(s) that code for the stored
-- protein sequence.
data GN = GN
  { geneName          :: Text   -- ^The name used to represent a gene.
  , synonyms          :: [Text] -- ^Other (unofficial) names of a gene.
  , orderedLocusNames :: [Text] -- ^A name used to represent an ORF in a completely sequenced genome or chromosome.
  , orfNames          :: [Text] -- ^A name temporarily attributed by a sequencing project to an open reading frame.
  } deriving (Show, Eq, Ord)

-- |Organism Species - the organism which was the source of the
-- stored sequence.
newtype OS = OS
  { specie :: Text
  } deriving (Show, Eq, Ord)

-- |OrGanelle - indicates if the gene coding for a protein originates
-- from mitochondria, a plastid, a nucleomorph or a plasmid.
newtype OG = OG
  { organellas :: [Text]
  } deriving (Show, Eq, Ord)

-- |Organism Classification - the taxonomic classification of the
-- source organism.
newtype OC = OC
  { nodes :: [Text]
  } deriving (Show, Eq, Ord)

-- |Organism taxonomy cross-reference indicates the identifier of a
-- specific organism in a taxonomic database.
data OX = OX
  { databaseQualifier :: Text -- ^Taxonomy database Qualifier
  , taxonomicCode     :: Text -- ^Taxonomic code
  } deriving (Show, Eq, Ord)

-- |Organism Host - indicates the host organism(s) that are
-- susceptible to be infected by a virus. Appears only in viral
-- entries.
data OH = OH
  { taxId    :: Text
  , hostName :: Text
  } deriving (Show, Eq, Ord)

-- |Reference Number - a sequential number to each reference
-- citation in an entry.
newtype RN = RN
  { referenceNumber :: Int
  } deriving (Show, Eq, Ord)

-- |Reference Position - the extent of the work relevant to
-- the entry carried out by the authors.
newtype RP = RP
  { referencePosition :: Text
  } deriving (Show, Eq, Ord)

data Token = STRAIN
           | PLASMID
           | TRANSPOSON
           | TISSUE
  deriving (Show, Eq, Ord, Bounded, Enum)

-- |Reference Comment - comments relevant to the reference cited.
newtype RC = RC
  { referenceComments :: [(Token, Text)]
  } deriving (Show, Eq, Ord)

-- |Bibliographic database names.
data BibliographicDB = MEDLINE
                     | PubMed
                     | DOI
                     | AGRICOLA
  deriving (Show, Eq, Ord, Bounded, Enum)

-- |Reference cross-reference - the identifier assigned to a specific
-- reference in a bibliographic database.
newtype RX = RX
  { referenceCrossrefs :: [(BibliographicDB, Text)]
  } deriving (Show, Eq, Ord)

-- |Reference Group - the consortium name associated with a given
-- citation.
newtype RG = RG
  { referenceGroup :: Text
  } deriving (Show, Eq, Ord)

-- |Reference Author - authors of the paper (or other work) cited.
newtype RA = RA
  { referenceAuthors :: [Text]
  } deriving (Show, Eq, Ord)

-- |Reference Title - the title of the paper (or other work) cited as
-- exactly as possible given the limitations of the computer character
-- set.
newtype RT = RT
  { referenceTitle :: Text
  } deriving (Show, Eq, Ord)

-- |Reference Location - he conventional citation information for the
-- reference.
newtype RL = RL
  { referenceLocation :: Text
  } deriving (Show, Eq, Ord)

-- |The comment blocks are arranged according to what we designate as
-- 'topics'.
type Topic = Text

-- |Free text comments on the entry, and are used to convey any useful
-- information.
newtype CC = CC
  { comments :: [(Topic, Text)]
  } deriving (Show, Eq, Ord)

-- |Database cross-Reference - pointers to information in external
-- data resources that is related to UniProtKB entries.
data DR = DR
  { resourceAbbr :: Text   -- ^The abbreviated name of the referenced resource (e.g. PDB).
  , resourceId   :: Text   -- ^An unambiguous pointer to a record in the referenced resource.
  , optionalInfo :: [Text] -- ^Used to provide optional information.
  } deriving (Show, Eq, Ord)

-- |Protein existence - indication on the evidences that we currently
-- have for the existence of a protein. Because most protein sequences
-- are derived from translation of nucleotide sequences and are mere
-- predictions, the PE line indicates what the evidences are of the
-- existence of a protein.
data PE = EvidenceAtProteinLevel
        | EvidenceAtTranscriptLevel
        | InferredFromHomology
        | Predicted
        | Uncertain
  deriving (Show, Eq, Ord)

-- |KeyWord - information that can be used to generate indexes of the
-- sequence entries based on functional, structural, or other
-- categories.
newtype KW = KW
  { keywords :: [Text]
  } deriving (Show, Eq, Ord)

-- |Feature Table - means for the annotation of the sequence data.
data FT = FT
  { keyName     :: Text -- ^Key name.
  , fromEP      :: Int  -- ^'From' endpoint.
  , toEP        :: Int  -- ^'To' endpoint.
  , description :: Text -- ^Description.
  } deriving (Show, Eq, Ord)

-- |SeQuence header - sequence data and a quick summary of its content.
data SQ = SQ
  { seqLength :: Int  -- ^Length of the sequence in amino acids.
  , molWeight :: Int  -- ^Molecular weight rounded to the nearest mass unit (Dalton).
  , crc64     :: Text -- ^Sequence 64-bit CRC (Cyclic Redundancy Check) value.
  , sequence  :: Text -- ^Sequence of the protein
  } deriving (Show, Eq, Ord)

data Reference = Reference
  { rn :: RN
  , rp :: RP
  , rc :: RC
  , rx :: RX
  , rg :: Maybe RG
  , ra :: RA
  , rt :: RT
  , rl :: RL
  } deriving (Show, Eq, Ord)

-- |Full UniProt record in UniProt-KB format.
data Record = Record
  { id   :: ID
  , ac   :: AC
  , dt   :: DT
  , de   :: DE
  , gn   :: [GN]
  , os   :: [OS]
  , og   :: Maybe OG
  , oc   :: Maybe OC
  , ox   :: Maybe OX
  , oh   :: Maybe OH
  , refs :: [Reference]
  , cc   :: CC
  , dr   :: [DR]
  , pe   :: PE
  , kw   :: KW
  , ft   :: [FT]
  , sq   :: SQ
  } deriving (Show, Eq, Ord)

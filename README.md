# uniprot-kb

[![Travis](https://img.shields.io/travis/biocad/uniprot-kb.svg)](https://travis-ci.org/biocad/uniprot-kb)
[![hackage](https://img.shields.io/hackage/v/uniprot-kb.svg)](https://hackage.haskell.org/package/uniprot-kb)
[![hackage-deps](https://img.shields.io/hackage-deps/v/uniprot-kb.svg)](https://hackage.haskell.org/package/uniprot-kb)

A well-typed UniProt file format parser.

## Documentation

To build Haddock documentation run:
```
$ stack haddock
```

## Usage example

You can use this simple code to parse any UniProt file:

``` haskell
import           Bio.Uniprot
import           Data.Attoparsec.Text (parseOnly)
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO
import           Text.Pretty.Simple   (pPrint)

main :: IO ()
main = do
    uniprot <- TIO.getContents
    case parseOnly parseRecord uniprot of
      Left err  -> putStrLn "Error on parse"
      Right obj -> pPrint obj
```

So you can parse any stdin `Text` of UniProt by using `parseRecord` function. The result will be presented by
a `Record` datatype:
``` haskell
data Record = Record
  { id   :: ID
  , ac   :: AC
  , dt   :: DT
  , de   :: DE
  , gn   :: [GN]
  , os   :: OS
  , og   :: Maybe OG
  , oc   :: OC
  , ox   :: Maybe OX
  , oh   :: [OH]
  , refs :: [Reference]
  , cc   :: [CC]
  , dr   :: [DR]
  , pe   :: PE
  , kw   :: KW
  , ft   :: [FT]
  , sq   :: SQ
  } deriving (Show, Eq, Ord)
```

You can run this to test the implementation:
``` bash
stack runhaskell uniprot.hs < example/LOLA2_DROME.dat
```

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

To use the parser and types import:
```
λ> import Bio.Uniprot
```

Than you can parse any `Text` of UniProt by using `parseRecord` function. The result will be presented by
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

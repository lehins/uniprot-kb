# Changelog for uniprot-kb

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## 0.1.2.0
* Every record in UniProt-KB can be parsed
* Any capital english letter can be an amino acid
* Schema was changed a little
* Multiple fixes on specification bugs
* Evidence is not parsed any more as it's breaks specification in lots of ways.

## 0.1.1.2
* Empty FT description bugfix

## 0.1.1.1
* `Generic` deriving for all types

## 0.1.1.0
* Back compability lost for SQ section
* No more DuplicateRecordFields, so it can be built by 7.x
* Includes and Contains sections of DE tested

## 0.1.0.1
* Small fixes in description fields read
* Documentation fixes
* Usage example in README

## 0.1.0.0
* Any UniProt-KB file can be parsed

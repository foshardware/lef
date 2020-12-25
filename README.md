# LEF parser

Parses Library Exchange Format (LEF) into an Abstract Syntax Tree (AST). Intended for use with LibreSilicon Compiler (lsc).

## Usage

```haskell

import qualified Data.Text as Text

import Language.LEF.Parser (parseLEF)

main = do
  file <- Text.readFile "my.lef"
  putStrLn $ show $ parseLEF file


```

## Tests

```bash

git submodule update --init --recursive
stack test


```

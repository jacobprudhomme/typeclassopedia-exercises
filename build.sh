#!/bin/bash

# Fail on error and don't mask errors in sequences of pipes
set -eo pipefail

echo "Converting Literate Haskell to Markdown"
mkdir -p doc
for FILENAME in src/*.lhs; do
  cabal exec pandoc -- "$FILENAME" -f markdown+lhs -t gfm -o "doc/$(basename "$FILENAME" .lhs).md"
done

echo "Creating compilable copy of output"
mkdir -p build
for FILENAME in doc/*.md; do
  cp "$FILENAME" "build/$(basename "$FILENAME" .md).lhs"
done

echo "Testing code segments to see if they typecheck"
for FILENAME in build/*.lhs; do
  cabal exec ghc -- -pgmL markdown-unlit "$FILENAME"
done
rm -rf build

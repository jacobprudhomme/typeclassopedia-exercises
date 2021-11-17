#!/bin/bash

# Fail on error and don't mask errors in sequences of pipes
set -eo pipefail

# Name of the Typeclassopedia chapter completed in this PR, in PascalCase
CHAPTER=$(echo ${GITHUB_REF##*/} | sed -r 's/(^|_)([a-z])/\U\2/g')
echo "Executing build for chapter $CHAPTER"

echo "Converting Literate Haskell to Markdown"
mkdir -p doc
pandoc "src/$CHAPTER.lhs.md" -f markdown+lhs -t gfm -o "doc/$CHAPTER.md"

echo "Creating compilable copy of output"
mkdir -p build
cp "doc/$CHAPTER.md" "build/$CHAPTER.lhs"

echo "Testing code segments to see if they typecheck"
ghc -pgmL markdown-unlit "build/$CHAPTER.lhs"
rm -rf build

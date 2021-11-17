# Typeclassopedia Exercises

I have written up the notes and solutions I made following the Haskell Typeclassopedia, in a literate format. This allows all my solutions to be compiled and typechecked, while also producing written documentation with syntax highlighting and LaTeX support.


## Sections

* [Functor](doc/Functor.md)


## Requirements

* VSCode plugin Math to Images (to convert LaTeX math to embedded images)
* Pandoc (to convert from literate source files to Markdown with code blocks)
* markdown-unlit (to typecheck literate code)


## Steps to Replicate Build Process Locally

* Convert any LaTeX math in the literate source files to images using the above VSCode plugin
* Use `pandoc src/<filename>.lhs.md -f markdown+lhs -t gfm -o doc/<filename>.md` to get output Markdown
* Copy the above output file to have new extension `.lhs` (I create a `build` folder that I copy this file to)
* Run Haskell code fragments using `ghc -pgmL markdown-unlit <wherever you copied to>/<filename>.lhs`. If it compiles, your code typechecks!

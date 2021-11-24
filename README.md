# Typeclassopedia Exercises

I have written up the notes and solutions I made following the Haskell Typeclassopedia, in a literate format. This allows all my solutions to be compiled and typechecked, while also producing written documentation with syntax highlighting and LaTeX support.


## Sections

* [`Functor`](doc/Functor.md)
* [`Applicative`](doc/Applicative.md)
* [`Monad`](doc/Monad.md)
* [`MonadFail`](doc/MonadFail.md)
* [Monad Transformers](doc/MonadTransformers.md)
* [`MonadFix`](doc/MonadFix.md)
* [`Semigroup`](doc/Semigroup.md)
* [`Monoid`](doc/Monoid.md)
* [Failure and Choice](doc/FailureAndChoice.md)
* [`Foldable`](doc/Foldable.md)
* [`Traversable`](doc/Traversable.md)
* [`Bifunctor`](doc/Bifunctor.md)
* [`Category`](doc/Category.md)
* [`Arrow`](doc/Arrow.md)

Throughout the documents, I use a Capitalized and `monospaced` version of typeclass names to describe the Haskell typeclass (e.g. `Maybe` is an instance of `Monad`), and the original word to describe the theoretical or mathematical concept (e.g. associativity is a monad law). I try to be as consistent as possible, but sometimes I forget, and some instances may be ambiguous or difficult to differentiate.


## Requirements

* VSCode plugin Math to Images (to convert LaTeX math to embedded images)
* GHC 8.10
* Pandoc (to convert from literate source files to Markdown with code blocks)
* markdown-unlit (to typecheck literate code)


## Steps to Replicate Build Process Locally

* Convert any LaTeX math in the literate source files to images using the above VSCode plugin
  * Note: this extension produces comments. When generating an image for inline math, the comment produced is all on one line. For some reason, `pandoc` doesn't like this and interprets these lines as code blocks. To fix this, just split the comment up over multiple lines)
* Use `pandoc src/<filename>.lhs -f markdown+lhs -t gfm -o doc/<filename>.md` to get output Markdown
* Copy the above output file to have new extension `.lhs` (I create a `build` folder that I copy this file to)
* Run Haskell code fragments using `ghc -pgmL markdown-unlit <wherever you copied to>/<filename>.lhs`. If it compiles, your code typechecks!


## NOTE

Markdown syntax highlighting seems quite a bit better than the Literate Haskell one (it technically doesn't support writing literate files in Markdown), so I recommend setting `*.lhs` files to use the Markdown language association for the workspace. It also allows for conveniences, such as using the rendered preview offered by the Markdown All in One extension.


## Todo

* Normalize presentation of laws (some use LaTeX, some use Markdown)
* Add laws where applicable (for example, `ArrowLoop` has many laws that are not mentioned in the Typeclassopedia, but are in the documentation)
* Check over rendered documents to make sure everything is consistent

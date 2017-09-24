# Instaparse Change Log

## Unreleased

### Enhancements

* ABNF parsers' string case-insensitivity can now be disabled by setting `:string-ci false`.

## 1.4.8

### Updates

* Update to support Clojurescript 1.9.854 and above, due to a breaking change in Clojurescript to use tools.reader.

## 1.4.7

### Enhancements

* `visualize` now supports `:output-file :buffered-image`, which returns a java.awt.image.BufferedImage object.

### Bugfixes

* Fixed problem where `visualize` with `:output-file` didn't work on rootless trees.

## 1.4.6

### Performance improvements

* Better performance for ABNF grammars in Clojurescript.

## 1.4.5

### Bugfixes

* Fixed regression in 1.4.4 involving parsers based off of URIs.

* defparser now supports the full range of relevant parser options.

## 1.4.4

### Enhancements

* Instaparse is now cross-platform compatible between Clojure and Clojurescript.

### Features

* defparser - builds parser at compile time

## 1.4.3

### Bugfixes

* Fixed bug with insta/transform on tree with hidden root tag and strings at the top level of the tree.

## 1.4.2

### Bugfixes

* Fixed problem with counted repetitions in ABNF.

## 1.4.1

### Features

* New function `add-line-and-column-info-to-metadata` in the instaparse.core namespace.

### Enhancements

* Added new combinators for unicode character ranges, for better portability to Clojurescript.

### Bugfixes

* Improved compatibility with boot, which allows having multiple versions of Clojure on the classpath, by making change to string-reader which needs to
be aware of what version of Clojure it is running due to a breaking change in Clojure 1.7.

* Fixed bug with the way failure messages were printed in certain cases.

## 1.4.0

### Bugfixes

* In 1.3.6, parsing of any CharSequence was introduced, however, the error messages
  for failed parses weren't printing properly.  This has been fixed.    

* 1.4.0 uses a more robust algorithm for handling nested negative lookaheads, in
  response to a bug report where the existing mechanism produced incorrect parses
  (in addition to the correct parse) for a very unusual case.   

### Enhancements

* New support for tracing the steps the parser goes through.  Call your parser with
  the optional flag `:trace true`.  The first time you use this flag, it triggers a
  recompilation of the code with additional tracing and profiling steps.
  To restore the code to its non-instrumented form, call `(insta/disable-tracing!)`.

## 1.3.6

### Enhancements

* Modified for compatibility with Clojure 1.7.0-alpha6
* Instaparse now can parse anything supporting the CharSequence interface, not just strings.
  Specifically, this allows instaparse to operate on StringBuilder objects. 

## 1.3.5

### Bugfixes

* Fixed bug with `transform` on hiccup data structures with numbers or other atomic data as leaves.

* Fixed bug with character concatenation support in ABNF grammar

### Enhancements

* Added support for Unicode characters to ABNF.

## 1.3.4

### Enhancements

* Modified for compatibility with Clojure 1.7.0-alpha2.

## 1.3.3

### Enhancements

Made two changes to make it possible to use instaparse on Google App Engine.

* Removed dependency on javax.swing.text.Segment class.
* Added `:no-slurp true` keyword option to `insta/parser` to disable URI slurping behavior, since GAE does not support slurp.

## 1.3.2

### Bugfixes

* Regular expressions on empty strings weren't properly returning a failure.

## 1.3.1

### Enhancements

* Updated tests to use Clojure 1.6.0's final release.
* Added `:ci-string true` flag to `insta/parser`.

## 1.3.0

### Compatibility with Clojure 1.6

## 1.2.16

### Bugfixes

* Calling `empty` on a FlattenOnDemandVector now returns [].

## 1.2.15

### Enhancements

* :auto-whitespace can now take the keyword :standard or :comma to access one of the predefined whitespace parsers.

### Bugfixes

* Fixed newline problem visualizing parse trees on Linux.
* Fixed problem with visualizing rootless trees.

## 1.2.11

### Minor enhancements

* Further refinements to the way ordered choice interacts with epsilon parsers.

## 1.2.10

### Bugfixes

* Fixed bug introduced by 1.2.9 affecting ordered choice.

## 1.2.9

### Bugfixes

* Fixed bug where ordered choice was ignoring epsilon parser.

## 1.2.8

### Bugfixes

* Fixed bug introduced by 1.2.7, affecting printing of grammars with regexes.

### Enhancements

* Parser printing format now includes <> hidden information and tags.

## 1.2.7

### Bugfixes

* Fixed bug when regular expression contains | character.

## 1.2.6

### Bugfixes

* Changed pre-condition assertion for auto-whitespace option which was causing a problem with "lein jar".

## 1.2.5

### Bugfixes

* Improved handling of unusual characters in ABNF grammars.

## 1.2.4

### Bugfixes

* When parsing in :total mode with :enlive as the output format, changed the content of failure node from vector to list to match the rest of the enlive output.

## 1.2.3

### Bugfixes

* Fixed problem when epsilon was the only thing in a nonterminal, e.g., "S = epsilon"

### Features

* Added experimental `:auto-whitespace` feature.  See the [Experimental Features Document](docs/ExperimentalFeatures.md) for more details.

## 1.2.2

### Bugfixes

* Fixed reflection warning.

## 1.2.1

### Bugfixes

* I had accidentally left a dependency on tools.trace in the repeat.clj file, used while I was debugging that namespace.  Removed it.

## 1.2.0

### New Features

* `span` function returns substring indexes into the parsed text for a portion of the parse tree.
* `visualize` function draws the parse tree, using rhizome and graphviz if installed.
* `:optimize :memory` flag that, for suitable parsers, will perform the parsing in discrete chunks, using less memory.
* New parsing flag to undo the effect of the <> hide notation.
    + `(my-parser text :unhide :tags)` - reveals tags, i.e., `<>` applied on the left-hand sides of rules. 
    + `(my-parser text :unhide :content)` - reveals content hidden on the right-hand side of rules with `<>`
    + `(my-parser text :unhide :all)` - reveals both tags and content.

### Notable Performance Improvements

* Dramatic performance improvement (quadratic time reduced to linear) when repetition parsers (+ or *) operate on text whose parse tree contains a large number of repetitions.
* Performance improvement for regular expressions. 

### Minor Enhancements

* Added more support to IncrementalVector for a wider variety of vector operations, including subvec, nth, and vec.

## 1.1.0

### Breaking Changes

* When you run a parser in "total" mode, the failure node is no longer tagged with `:failure`, but instead is tagged with `:instaparse/failure`.

### New Features

* Comments now supported in CFGs.  Use (* and *) notation.
* Added `ebnf` combinator to the `instaparse/combinators` namespace.  This new combinator converts string specifications to the combinator-built equivalent.  See combinator section of the updated tutorial for details.
* ABNF: can now create a parser from a specification using `:input-format :abnf` for ABNF parser syntax.
    * New combinators related to ABNF:
        1. `abnf` -- converts ABNF string fragments to combinators.
        2. `string-ci` -- case-insensitive strings.
        3. `rep` -- between m and n repetitions.
    * New core function related to ABNF:
        `set-default-input-format!` -- initially defaults to :ebnf

### Minor Enhancements

* Added comments to regexes used by the parser that processes the context-free grammar syntax, improving the readability of error messages if you have a faulty grammar specification.

### Bug Fixes

* Backslashes in front of quotation mark were escaping the quotation mark, even if the backslash itself was escaped.
* Unescaped double-quote marks weren't properly handled, e.g., (parser "A = '\"'").
* Nullable Plus: ((parser "S = ('a'?)+") "") previously returned a failure, now returns [:S]
* Fixed problem with failure reporting that would occur if parse failed on an input that ended with a newline character.

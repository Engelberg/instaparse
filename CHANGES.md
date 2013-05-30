# Instaparse Change Log

## 1.2.0

### Notable Performance Improvements

* Dramatic performance improvement (quadratic time reduced to linear) when repetition parsers (+ or *) operate on text whose parse tree contains a large number of repetitions.

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
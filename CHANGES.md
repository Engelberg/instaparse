# Instaparse Change Log

## 1.1.0

### Breaking Changes

* When you run a parser in "total" mode, the failure node is no longer tagged with `:failure`, but instead is tagged with `:instaparse/failure`.

### New Features

* Comments now supported in CFGs.  Use (* and *) notation.
* Added `ebnf` combinator to the `instaparse/combinators` namespace.  This new combinator converts string specifications to the combinator-built equivalent.  See combinator section of the updated tutorial for details.

### Minor Enhancements

* Added comments to regexes used by the parser that processes the context-free grammar syntax, improving the readability of error messages if you have a faulty grammar specification.

### Bug Fixes

* Backslashes in front of quotation mark were escaping the quotation mark, even if the backslash itself was escaped.
* Unescaped double-quote marks weren't properly handled, e.g., (parser "A = '\"'").
* Nullable Plus: ((parser "S = ('a'?)+") "") previously returned a failure, now returns [:S]
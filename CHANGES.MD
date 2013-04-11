# Instaparse Change Log

## 1.1.0

### Enhancements

* Comments now supported in CFGs.  Use (* and *) notation.
* Added comments to regexes used by context-free grammar.

### Bug Fixes

* Backslashes in front of quotation mark were escaping the quotation mark, even if the backslash itself was escaped.

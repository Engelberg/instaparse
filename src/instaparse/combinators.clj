(ns instaparse.combinators
  (:require [instaparse.combinators-source :as c])
  (:require [instaparse.cfg :as cfg])
  (:use potemkin))

;; The actual source is in combinators-source.
;; This was necessary to avoid a cyclical dependency in the namespaces.

(import-def c/Epsilon)
(import-fn c/opt)
(import-fn c/plus)
(import-fn c/star)
(import-fn c/alt) 
(import-fn c/ord)
(import-fn c/cat)
(import-fn c/string)
(import-fn c/regexp)
(import-fn c/nt)
(import-fn c/look)
(import-fn c/neg)
(import-fn c/hide)
(import-fn c/hide-tag)

(import-fn cfg/ebnf)
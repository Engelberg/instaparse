(ns instaparse.combinators
  "The combinator public API for instaparse"
  (:refer-clojure :exclude [cat])
  (:require [instaparse.combinators-source :as c]
            [instaparse.cfg :as cfg]
            [instaparse.abnf :as abnf]))

;; The actual source is in combinators-source.
;; This was necessary to avoid a cyclical dependency in the namespaces.

(def Epsilon c/Epsilon)
(def opt c/opt)
(def plus c/plus)
(def star c/star)
(def rep c/rep)
(def alt c/alt) 
(def ord c/ord)
(def cat c/cat)
(def string c/string)
(def string-ci c/string-ci)
(def unicode-char c/unicode-char)
(def regexp c/regexp)
(def nt c/nt)
(def look c/look)
(def neg c/neg)
(def hide c/hide)
(def hide-tag c/hide-tag)

(def ebnf cfg/ebnf)
(def abnf abnf/abnf)
       

(ns instaparse.combinators
  "The combinator public API for instaparse"
  (:refer-clojure :exclude [cat])
  #?(:clj (:use instaparse.macros)
     :cljs (:require-macros
            [instaparse.macros :refer [defclone]]))
  (:require [instaparse.combinators-source :as c]
            [instaparse.cfg :as cfg]
            [instaparse.abnf :as abnf]))

;; The actual source is in combinators-source.
;; This was necessary to avoid a cyclical dependency in the namespaces.

(defclone Epsilon c/Epsilon)
(defclone opt c/opt)
(defclone plus c/plus)
(defclone star c/star)
(defclone rep c/rep)
(defclone alt c/alt) 
(defclone ord c/ord)
(defclone cat c/cat)
(defclone string c/string)
(defclone string-ci c/string-ci)
(defclone unicode-char c/unicode-char)
(defclone regexp c/regexp)
(defclone nt c/nt)
(defclone look c/look)
(defclone neg c/neg)
(defclone hide c/hide)
(defclone hide-tag c/hide-tag)

(defclone ebnf cfg/ebnf)
(defclone abnf abnf/abnf)
       

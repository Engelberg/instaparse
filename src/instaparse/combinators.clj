(ns instaparse.combinators
  "The combinator public API for instaparse"
  (:require [instaparse.combinators-source :as c])
  (:require [instaparse.cfg :as cfg]))

;; The actual source is in combinators-source.
;; This was necessary to avoid a cyclical dependency in the namespaces.

(defmacro defclone [here there]
  `(do 
     (def ~here ~there)
     (alter-meta! (var ~here) assoc
                  :doc (:doc (meta (var ~there)))
                  :arglists (:arglists (meta (var ~there)))
                  :file (:file (meta (var ~there)))
                  :line (:line (meta (var ~there)))
                  :column (:column (meta (var ~there))))
     (var ~here)))
                 
(defclone Epsilon c/Epsilon)
(defclone opt c/opt)
(defclone plus c/plus)
(defclone star c/star)
(defclone alt c/alt) 
(defclone ord c/ord)
(defclone cat c/cat)
(defclone string c/string)
(defclone regexp c/regexp)
(defclone nt c/nt)
(defclone look c/look)
(defclone neg c/neg)
(defclone hide c/hide)
(defclone hide-tag c/hide-tag)

(defclone ebnf cfg/ebnf)
       

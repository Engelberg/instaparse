(ns instaparse.viz)   

(defn span
  "Takes a subtree of the parse tree and returns a [start-index end-index] pair
   indicating the span of text parsed by this subtree.
   start-index is inclusive and end-index is exclusive, as is customary
   with substrings.
   Returns nil if no span metadata is attached."
  [tree]
  (let [m (meta tree)
        s (:instaparse.gll/start-index m)
        e (:instaparse.gll/end-index m)]
    (when (and s e)
      [s e])))


(ns instaparse.transform
  "Functions to transform parse trees"
  (:require instaparse.gll))

(defn- enlive-transform
  [transform-map parse-tree]
  (let [transform (transform-map (:tag parse-tree))]
    (cond
      transform
      (apply transform (map (partial enlive-transform transform-map)
                            (:content parse-tree)))
      (:tag parse-tree)
      (assoc parse-tree :content (map (partial enlive-transform transform-map)
                                       (:content parse-tree)))
      :else
      parse-tree)))

(defn- hiccup-transform
  [transform-map parse-tree]
  (let [transform (transform-map (first parse-tree))]
    (cond
      transform
      (apply transform (map (partial hiccup-transform transform-map)
                            (next parse-tree)))
      (and (sequential? parse-tree) (seq parse-tree))
      (into [(first parse-tree)]
            (map (partial hiccup-transform transform-map) 
                 (next parse-tree)))
      :else
      parse-tree)))

(defn- map-preserving-meta [f l]
  (with-meta (map f l) (meta l)))

(defn transform
  "Takes a transform map and a parse tree (or seq of parse-trees).
   A transform map is a mapping from tags to 
   functions that take a node's contents and return
   a replacement for the node, i.e.,
   {:node-tag (fn [child1 child2 ...] node-replacement),
    :another-node-tag (fn [child1 child2 ...] node-replacement)}"
  [transform-map parse-tree]
  ; Detect what kind of tree this is
  (cond
    (and (map? parse-tree) (:tag parse-tree))
    ; This is an enlive tree-seq
    (enlive-transform transform-map parse-tree)
    
    (and (vector? parse-tree) (keyword? (first parse-tree)))
    ; This is a hiccup tree-seq
    (hiccup-transform transform-map parse-tree)
    
    (sequential? parse-tree)
    ; This is either a sequence of parse results, or a tree
    ; with a hidden root tag.
    (map-preserving-meta (partial transform transform-map) parse-tree)
    
    (instance? instaparse.gll.Failure parse-tree)
    ; pass failures through unchanged
    parse-tree
    
    :else
    (throw (IllegalArgumentException. "Invalid parse-tree, not recognized as either enlive or hiccup format."))))
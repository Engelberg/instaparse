(ns instaparse.reduction
  (:require [instaparse.auto-flatten-seq :as afs]))

;; utilities

(defn singleton? [s]
  (and (seq s) (not (next s))))

;; red is a reduction combinator for expert use only
;; because it is used internally to control the tree tags that
;; are displayed, so adding a different reduction would change
;; that behavior.

(defn red [parser f] (assoc parser :red f))

;; Flattening and reductions

(def raw-non-terminal-reduction {:reduction-type :raw})

(defn HiccupNonTerminalReduction [key]
  {:reduction-type :hiccup :key key})

(defn EnliveNonTerminalReduction [key] 
  {:reduction-type :enlive, :key key})

(def ^:constant reduction-types 
  {:hiccup HiccupNonTerminalReduction
   :enlive EnliveNonTerminalReduction})
                    
(def ^:constant node-builders
  ; A map of functions for building a node that only has one item
  ; These functions are used in total-parse mode to build failure nodes
  {:enlive (fn [tag item] {:tag tag :content (list item)})
   :hiccup (fn [tag item] [tag item])})

(def standard-non-terminal-reduction :hiccup)

(defn apply-reduction [f result]
  (case (:reduction-type f)
    :raw (afs/conj-flat afs/EMPTY result)               
    :hiccup (afs/convert-afs-to-vec (afs/conj-flat (afs/auto-flatten-seq [(:key f)]) result))
    :enlive 
    (let [content (afs/conj-flat afs/EMPTY result)]
      {:tag (:key f), :content (if (zero? (count content)) nil content)})
    (f result)))
    
(defn apply-standard-reductions 
  ([grammar] (apply-standard-reductions standard-non-terminal-reduction grammar))
  ([reduction-type grammar]
    (if-let [reduction (reduction-types reduction-type)]
      (into {} (for [[k v] grammar]
                 (if (:red v) [k v]
                   [k (assoc v :red (reduction k))])))
      (throw (str "Invalid output format" reduction-type ". Use :enlive or :hiccup.")))))
    

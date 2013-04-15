(ns instaparse.reduction)

;; utilities

(defn singleton? [s]
  (and (seq s) (not (next s))))

;; red is a reduction combinator for expert use only
;; because it is used internally to control the tree tags that
;; are displayed, so adding a different reduction would change
;; that behavior.

(defn red [parser f] (assoc parser :red f))

;; Flattening and reductions

(defn make-flattenable [s]
  (with-meta s {:flattenable? true}))

(defn flattenable? [s]
  (:flattenable? (meta s)))

(defn nt-flatten [s]
  (when (seq s)
    (let [fs (first s)]
      (cond 
        (nil? fs)         (recur (next s))
        (flattenable? fs) (concat (nt-flatten fs) (nt-flatten (next s)))
        :else             (lazy-seq (cons fs (nt-flatten (next s))))))))

(defrecord RawNonTerminalReduction []
  clojure.lang.IFn
  (applyTo [self parse-result]
    (if parse-result
      (make-flattenable parse-result)
      nil)) )

(def raw-non-terminal-reduction (RawNonTerminalReduction.))

(defrecord HiccupNonTerminalReduction [key]
  clojure.lang.IFn
  (applyTo [self parse-result]
    (into [key] parse-result)))

(defrecord EnliveNonTerminalReduction [key] 
  clojure.lang.IFn
  (applyTo [self parse-result]
    {:tag key, :content parse-result}))

(def ^:constant reduction-types 
  {:hiccup ->HiccupNonTerminalReduction
   :enlive ->EnliveNonTerminalReduction})
                    
(def ^:constant node-builders
  ; A map of functions for building a node that only has one item
  ; These functions are used in total-parse mode to build failure nodes
  {:enlive (fn [tag item] {:tag tag :content [item]})
   :hiccup (fn [tag item] [tag item])})

(def standard-non-terminal-reduction :hiccup)

(defn apply-reduction [f result]
  (apply f (nt-flatten (make-flattenable [result]))))

(defn apply-standard-reductions 
  ([grammar] (apply-standard-reductions standard-non-terminal-reduction grammar))
  ([reduction-type grammar]
    (if-let [reduction (reduction-types reduction-type)]
      (into {} (for [[k v] grammar]
                 (if (:red v) [k v]
                   [k (assoc v :red (reduction k))])))
      (throw (IllegalArgumentException. 
               (format "Invalid output format %s. Use :enlive or :hiccup." reduction-type))))))
    

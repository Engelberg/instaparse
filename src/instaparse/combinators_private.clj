(ns instaparse.combinators-private)

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

(defn raw-non-terminal-reduction [& parse-result] 
  (if parse-result
    (make-flattenable parse-result)
    nil)) 

(defn hiccup-non-terminal-reduction [key] 
  (fn [& parse-result]
    ;(cons key parse-result)))
    (into [key] parse-result)))

(defn enlive-non-terminal-reduction [key] 
  (fn [& parse-result]
    {:tag key, :content parse-result}))

(def standard-non-terminal-reduction hiccup-non-terminal-reduction)

(defn apply-reduction [f result]
  (apply f (nt-flatten (make-flattenable [result]))))

(defn apply-standard-reductions [grammar]
  (into {} (for [[k v] grammar]
             (if (:red v) [k v]
               [k (assoc v :red (standard-non-terminal-reduction k))]))))

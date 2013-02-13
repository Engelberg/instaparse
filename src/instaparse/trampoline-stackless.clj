(ns instaparse.trampoline-stackless
  (:use clojure.test))

(defn make-tramp
  []
  {:stack (atom [])
   :parser-table (atom {})})
; stack is an atom of a vector containing [function args ...]
; parser-table is an atom containing a map from parsers to memo-tables.
; a parser is a function that takes a string, a trampoline, and a continuation.
; a memo-table is an atom containing a map from strings to entries.
; an entry is a map with two keys, :results and :conts.
; a :results is an atom of a set.
; a :conts is an atom of a vector.

(defn has-next?
  [tramp]
  (not (empty? @(:stack tramp))))

(defn push-stack
  [tramp f & args]
  (let [call (cons f args)]
    (swap! (:stack tramp) conj call)))
    ;(println (count @(:stack tramp)))))

(defn step
  [tramp]
  (when (has-next? tramp)
    (let [[f & args] (peek @(:stack tramp))]
      (swap! (:stack tramp) pop)
      (apply f args))))

(defn run
  [tramp]
  (while (has-next? tramp)
    (step tramp)))

; entry helper functions
(def push-continuation! (fn [entry cont]
                             (swap! (:conts entry) conj cont)))
(def push-result! (fn [entry result]                    
                       (swap! (:results entry) conj result)))
(def result-subsumed? (fn [entry result]
                        ;{:pre [(set? @(:results entry)) (map? result)]
                        ; :post [(do (println %) true)]}
                        ;(println result)
                        (@(:results entry) result)))
(def make-entry (fn []
                     {:results (atom #{}) :conts (atom [])}))
(def make-memo-table (fn []
                       (atom {})))
(def empty-entry? (fn [entry]
                       (every? empty? (map deref (vals entry)))))

(def memo-table-ref (fn [memo-table str]
                      (if-let [try-get (@memo-table str)]
                        try-get
                        (do (swap! memo-table assoc str (make-entry))
                          (@memo-table str)))))
(defn parser-table-ref
  [parser-table f]
  (if-let [try-get (@parser-table f)]
    try-get
    (do (swap! parser-table assoc f (make-memo-table))
      (@parser-table f))))
  


(defn push
  [tramp f str cont]
  ;(print tramp)
  (let [memo-table (-> tramp :parser-table (parser-table-ref f))
        entry (memo-table-ref memo-table str)]
    (if (empty-entry? entry)
      (do (push-continuation! entry cont)
        (push-stack tramp f str tramp (fn [result]
                                        (when-not (result-subsumed? entry result)
                                          (push-result! entry result) 
                                          ;(printf "Conts: %d\n" (count @(:conts entry))) 
                                          (doseq [cont @(:conts entry)]
                                            (push-stack tramp cont result)))))) 
      (do (push-continuation! entry cont) 
        ;(printf "Results: %d" (count @(:results entry)))
        (doseq [result @(:results entry)]
          (push-stack tramp cont result))))))
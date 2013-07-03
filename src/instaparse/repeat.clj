(ns instaparse.repeat
  (:use clojure.tools.trace)
  (:require [instaparse.gll :as gll] 
            [instaparse.combinators-source :as c]
            [instaparse.auto-flatten-seq :as afs]
            [instaparse.viz :as viz]
            [instaparse.reduction :as red]
            [instaparse.failure :as fail]))

(defn empty-result? [result]
  (or (and (vector? result) (= (count result) 1))
      (and (map? result) (contains? result :tag) (empty? (get result :contents)))))        

(def ^:constant failure-signal (gll/->Failure nil nil))

(defn parse-from-index [grammar initial-parser text segment index]
  (let [tramp (gll/make-tramp grammar text segment)]
    (gll/push-listener tramp [index initial-parser] (gll/TopListener tramp))
    (if-let [all-parses (gll/run tramp)]
      (apply max-key (comp second viz/span) all-parses) 
      failure-signal)))

(defn repeat-parse-hiccup [grammar initial-parser root-tag text]
  (let [length (count text)
        segment (gll/string->segment text)]
    (loop [index 0
           parses (afs/auto-flatten-seq [root-tag])]
      (if (= index length) (gll/safe-with-meta
                             (afs/convert-afs-to-vec parses)
                             {:instaparse.gll/start-index 0
                              :instaparse.gll/end-index length})
        (let [first-result (parse-from-index grammar initial-parser text segment index)
              [start end] (viz/span first-result)
              end (if end (long end) (+ index (count first-result)))]
          (cond 
            (instance? instaparse.gll.Failure first-result) first-result
            (= index end) failure-signal
            :else (recur end (afs/conj-flat parses first-result))))))))
          
(defn repeat-parse-enlive [grammar initial-parser root-tag text]
  (let [length (count text)
        segment (gll/string->segment text)]
    (loop [index 0
           parses afs/EMPTY]
      (if (= index length) (gll/safe-with-meta
                             {:tag root-tag :content (seq parses)}
                             {:instaparse.gll/start-index 0
                              :instaparse.gll/end-index length})
        (let [first-result (parse-from-index grammar initial-parser text segment index)
              [start end] (viz/span first-result)
              end (if end (long end) (+ index (count first-result)))]
          (cond 
            (instance? instaparse.gll.Failure first-result) first-result
            (= index end) failure-signal
            :else (recur end (afs/conj-flat parses first-result))))))))

(defn repeat-parse-no-tag [grammar initial-parser text]
  (let [length (count text)
        segment (gll/string->segment text)]
    (loop [index 0
           parses afs/EMPTY]
      (if (= index length) (gll/safe-with-meta 
                             parses
                             {:instaparse.gll/start-index 0
                              :instaparse.gll/end-index length})
        (let [first-result (parse-from-index grammar initial-parser text segment index)
              [start end] (viz/span first-result)
              end (if end (long end) (+ index (count first-result)))]
          (cond 
            (instance? instaparse.gll.Failure first-result) first-result
            (= index end) failure-signal
            :else (recur end (afs/conj-flat parses first-result))))))))

(defn repeat-parse 
  ([grammar initial-parser output-format text] (repeat-parse-no-tag grammar initial-parser text))
  ([grammar initial-parser output-format root-tag text]
    {:pre [(#{:hiccup :enlive} output-format)]} 
    (cond
      (= output-format :hiccup)
      (repeat-parse-hiccup grammar initial-parser root-tag text)
      (= output-format :enlive)
      (repeat-parse-enlive grammar initial-parser root-tag text))))

(defn try-repeating-parse-strategy
  [parser text start-production]
  (let [grammar (:grammar parser)
        output-format (:output-format parser)
        start-rule (get grammar start-production)]
    (cond
      (= (:hide start-rule) true) failure-signal
      (= (:red start-rule) red/raw-non-terminal-reduction)
      (cond
        (= (:tag start-rule) :star)
        (repeat-parse grammar (:parser start-rule) output-format text)
        (= (:tag start-rule) :plus)
        (let [result (repeat-parse grammar (:parser start-rule) output-format text)]
          (if (empty? result)
            failure-signal ; simply a failure signal
            result))
        :else failure-signal)
              
      (= (:tag start-rule) :star)
      (repeat-parse grammar (:parser start-rule) output-format start-production text)
      (= (:tag start-rule) :plus)
      (let [result (repeat-parse grammar (:parser start-rule) output-format start-production text)]
        (if (empty-result? result)
          failure-signal ; simply a failure signal
          result))
      :else failure-signal)))
           
          
    
    
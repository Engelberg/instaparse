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

(defn parse-from-index [grammar initial-parser text segment index]
  (let [tramp (gll/make-tramp grammar text segment)]
    (gll/push-listener tramp [index initial-parser] (gll/TopListener tramp))
    (gll/run tramp)))

(defn select-parse
  "Returns either:
   [a-parse end-index a-list-of-valid-follow-up-parses]
   [a-parse end-index nil] (successfully reached end of text)
   nil (hit a dead-end with this strategy)"
  [grammar initial-parser text segment index parses]
  ;(clojure.pprint/pprint parses)
  (let [length (count text)]
    (loop [parses (seq parses)]
      (when parses
        (let [parse (first parses)
              [start end] (viz/span parse)
              end (if end end (+ index (count parse)))]
          (cond
            (= end length) [parse end nil]
            :else 
            (if-let [follow-ups (seq (parse-from-index grammar initial-parser text segment end))]
              [parse end follow-ups]
              (recur (next parses)))))))))

(def failure-signal (gll/->Failure nil nil))
                                 
(defn repeat-parse-hiccup [grammar initial-parser root-tag text]
  (let [length (count text)
        segment (gll/string->segment text)
        first-result (parse-from-index grammar initial-parser text segment 0)]
    (loop [index 0
           parses (afs/auto-flatten-seq [root-tag])
           
           [parse end follow-ups :as selection]
           (select-parse grammar initial-parser text segment index first-result)]
      (cond
        (nil? selection) failure-signal
        (nil? follow-ups) (gll/safe-with-meta
                            (afs/convert-afs-to-vec 
                              (afs/conj-flat parses parse))
                            {:optimize true
                             :instaparse.gll/start-index 0
                             :instaparse.gll/end-index length})
        :else (recur (long end)
                     (afs/conj-flat parses parse)
                     (select-parse grammar initial-parser text segment end follow-ups))))))

(defn repeat-parse-enlive [grammar initial-parser root-tag text]
  (let [length (count text)
        segment (gll/string->segment text)
        first-result (parse-from-index grammar initial-parser text segment 0)]
    (loop [index 0
           parses afs/EMPTY
           
           [parse end follow-ups :as selection]
           (select-parse grammar initial-parser text segment index first-result)]
      (cond
        (nil? selection) failure-signal
        (nil? follow-ups) (gll/safe-with-meta
                            {:tag root-tag 
                             :content (seq (afs/conj-flat parses parse))}
                            {:optimize true
                             :instaparse.gll/start-index 0
                             :instaparse.gll/end-index length})
        :else (recur (long end)
                     (afs/conj-flat parses parse)
                     (select-parse grammar initial-parser text segment end follow-ups))))))

(defn repeat-parse-no-tag [grammar initial-parser text]
  (let [length (count text)
        segment (gll/string->segment text)
        first-result (parse-from-index grammar initial-parser text segment 0)]
    (loop [index 0
           parses afs/EMPTY
           
           [parse end follow-ups :as selection]
           (select-parse grammar initial-parser text segment index first-result)]
      (cond
        (nil? selection) failure-signal
        (nil? follow-ups) (gll/safe-with-meta
                            (afs/conj-flat parses parse)
                            {:optimize true
                             :instaparse.gll/start-index 0
                             :instaparse.gll/end-index length})
        :else (recur (long end)
                     (afs/conj-flat parses parse)
                     (select-parse grammar initial-parser text segment end follow-ups))))))

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
      (= (:hide start-rule) true) (gll/->Failure nil nil)
      (= (:red start-rule) red/raw-non-terminal-reduction)
      (cond
        (= (:tag start-rule) :star)
        (repeat-parse grammar (:parser start-rule) output-format text)
        (= (:tag start-rule) :plus)
        (let [result (repeat-parse grammar (:parser start-rule) output-format text)]
          (if (empty? result)
            (gll/->Failure nil nil) ; simply a failure signal
            result))
        :else (gll/->Failure nil nil))
              
      (= (:tag start-rule) :star)
      (repeat-parse grammar (:parser start-rule) output-format start-production text)
      (= (:tag start-rule) :plus)
      (let [result (repeat-parse grammar (:parser start-rule) output-format start-production text)]
        (if (empty-result? result)
          (gll/->Failure nil nil) ; simply a failure signal
          result))
      :else (gll/->Failure nil nil))))
           
          
    
    
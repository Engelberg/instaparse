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

(defn get-end 
  (^long [parse]
    (let [[start end] (viz/span parse)]
      (if end (long end) (count parse))))
  (^long [parse ^long index]
    (let [[start end] (viz/span parse)]
      (if end (long end) (+ index (count parse))))))

(deftrace parse-from-index [grammar initial-parser text segment index]
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
                                 
(defn repeat-parse-hiccup 
  ([grammar initial-parser root-tag text segment]
    (repeat-parse-hiccup grammar initial-parser root-tag text segment 0))
  ([grammar initial-parser root-tag text segment index]
    (let [length (count text)
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
                       (select-parse grammar initial-parser text segment end follow-ups)))))))

(defn repeat-parse-enlive
  ([grammar initial-parser root-tag text segment]
    (repeat-parse-enlive grammar initial-parser root-tag text segment 0))
  ([grammar initial-parser root-tag text segment index]
    (let [length (count text)
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
                       (select-parse grammar initial-parser text segment end follow-ups)))))))

(defn repeat-parse-no-tag 
  ([grammar initial-parser text segment]
    (repeat-parse-no-tag grammar initial-parser text segment 0))
  ([grammar initial-parser text segment index]
    (let [length (count text)
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
                       (select-parse grammar initial-parser text segment end follow-ups)))))))

(defn repeat-parse 
  ([grammar initial-parser output-format text] (repeat-parse-no-tag grammar initial-parser text (gll/string->segment text)))
  ([grammar initial-parser output-format root-tag text]
    {:pre [(#{:hiccup :enlive} output-format)]} 
    (cond
      (= output-format :hiccup)
      (repeat-parse-hiccup grammar initial-parser root-tag text (gll/string->segment text))
      (= output-format :enlive)
      (repeat-parse-enlive grammar initial-parser root-tag text (gll/string->segment text)))))

(defn repeat-parse-with-header
  ([grammar header-parser repeating-parser output-format root-tag text]
    (println "rpwh")
    (let [segment (gll/string->segment text)
          length (count text)
          header-result (parse-from-index grammar header-parser text segment 0),
          end (get-end header-result)
          repeat-result (repeat-parse-no-tag grammar repeating-parser text segment end)
          concat-result (concat header-result repeat-result)
          span-meta {:optimize :true-with-header
                     :instaparse.gll/start-index 0
                     :instaparse.gll/end-index length}]
      (case output-format
        :enlive
        (gll/safe-with-meta
          {:tag root-tag :content concat-result}
          span-meta)
        :hiccup
        (gll/safe-with-meta
          (into [root-tag] concat-result)
          span-meta)
        (gll/safe-with-meta concat-result span-meta)))))
        
(defn try-repeating-parse-strategy-with-header
  [grammar text start-production start-rule output-format]
  (if
    (not= (:tag start-rule) :cat) failure-signal
    (let [parsers (:parsers start-rule)
          header-parser (apply c/cat (butlast parsers))
          repeating-parser (last parsers)]
      (if (= (:red start-rule) red/raw-non-terminal-reduction)
        (repeat-parse-with-header grammar header-parser repeating-parser nil start-production text)
        (repeat-parse-with-header grammar header-parser repeating-parser output-format start-production text)))))

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
            (try-repeating-parse-strategy-with-header 
                grammar text start-production start-rule output-format)
            result))
        :else (try-repeating-parse-strategy-with-header 
                grammar text start-production start-rule output-format))
              
      (= (:tag start-rule) :star)
      (repeat-parse grammar (:parser start-rule) output-format start-production text)
      (= (:tag start-rule) :plus)
      (let [result (repeat-parse grammar (:parser start-rule) output-format start-production text)]
        (if (empty-result? result)
          (try-repeating-parse-strategy-with-header 
                grammar text start-production start-rule output-format)
          result))
      
      :else (try-repeating-parse-strategy-with-header 
                grammar text start-production start-rule output-format))))
           
          
    
    

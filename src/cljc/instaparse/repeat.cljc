(ns instaparse.repeat
  (:require [instaparse.gll :as gll] 
            [instaparse.combinators-source :as c]
            [instaparse.auto-flatten-seq :as afs]
            [instaparse.viz :as viz]
            [instaparse.reduction :as red]
            [instaparse.failure :as fail])
  (:require-macros [instaparse.gll-macros :refer [profile]]))

(defn empty-result? [result]
  (or (and (vector? result) (= (count result) 1))
      (and (map? result) (contains? result :tag) (empty? (get result :content)))
      (empty? result)))       

(def ^:constant failure-signal (gll/->Failure nil nil))

(defn get-end 
  (^number [parse]
    (let [[start end] (viz/span parse)]
      (if end (long end) (count parse))))
  (^number [parse ^number index]
    (let [[start end] (viz/span parse)]
      (if end (long end) (+ index (count parse))))))

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
                                 
(defn repeat-parse-hiccup 
  ([grammar initial-parser root-tag text segment]
    (repeat-parse-hiccup grammar initial-parser root-tag text segment 0))
  ([grammar initial-parser root-tag text segment index]
    (let [length (count text)
          first-result (parse-from-index grammar initial-parser text segment index)]
      (loop [index (long index)
             parses (afs/auto-flatten-seq [root-tag])
             
             [parse end follow-ups :as selection]
             (select-parse grammar initial-parser text segment index first-result)]
        (cond
          (nil? selection) failure-signal
          (= index end) failure-signal
          (nil? follow-ups) (gll/safe-with-meta
                              (afs/convert-afs-to-vec 
                                (afs/conj-flat parses parse))
                              {:optimize :memory
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
          first-result (parse-from-index grammar initial-parser text segment index)]
      (loop [index (long index)
             parses afs/EMPTY
             
             [parse end follow-ups :as selection]
             (select-parse grammar initial-parser text segment index first-result)]
        (cond
          (nil? selection) failure-signal
          (= index end) failure-signal          
          (nil? follow-ups) (gll/safe-with-meta
                              {:tag root-tag 
                               :content (seq (afs/conj-flat parses parse))}
                              {:optimize :memory
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
          first-result (parse-from-index grammar initial-parser text segment index)]
      (loop [index (long index)
             parses afs/EMPTY
             
             [parse end follow-ups :as selection]
             (select-parse grammar initial-parser text segment index first-result)]
        (cond
          (nil? selection) failure-signal
          (= index end) failure-signal          
          (nil? follow-ups) (gll/safe-with-meta
                              (afs/conj-flat parses parse)
                              {:optimize :memory
                               :instaparse.gll/start-index 0
                               :instaparse.gll/end-index length})
          :else (recur (long end)
                       (afs/conj-flat parses parse)
                       (select-parse grammar initial-parser text segment end follow-ups)))))))

(defn repeat-parse 
  ([grammar initial-parser output-format text] (repeat-parse-no-tag grammar initial-parser text (gll/text->segment text)))
  ([grammar initial-parser output-format root-tag text]
    {:pre [(#{:hiccup :enlive} output-format)]} 
    (cond
      (= output-format :hiccup)
      (repeat-parse-hiccup grammar initial-parser root-tag text (gll/text->segment text))
      (= output-format :enlive)
      (repeat-parse-enlive grammar initial-parser root-tag text (gll/text->segment text)))))

(defn repeat-parse-with-header
  ([grammar header-parser repeating-parser output-format root-tag text]
    (let [segment (gll/text->segment text)
          length (count text)
          header-results (parse-from-index grammar header-parser text segment 0)]
      (if (or (empty? header-results)
              (:hide header-parser))
        failure-signal
        (let [header-result (apply max-key get-end header-results)
              end (get-end header-result)
              repeat-result (repeat-parse-no-tag grammar (:parser repeating-parser) text segment end)
              span-meta {:optimize :memory
                         :instaparse.gll/start-index 0
                         :instaparse.gll/end-index length}]
          (if (or (instance? instaparse.gll.Failure repeat-result)
                  (and (= (:tag repeating-parser) :star)
                       (empty-result? repeat-result)))
            failure-signal
            (case output-format
              :enlive
              (gll/safe-with-meta
                {:tag root-tag 
                 :content
                 (afs/conj-flat (afs/conj-flat afs/EMPTY header-result) repeat-result)}
                span-meta)
              :hiccup
              (gll/safe-with-meta
                (afs/convert-afs-to-vec 
                  (afs/conj-flat (afs/conj-flat (afs/auto-flatten-seq [root-tag])
                                                header-result) 
                                 repeat-result))
                span-meta)
              (gll/safe-with-meta 
                (afs/conj-flat (afs/conj-flat afs/EMPTY header-result) repeat-result)
                span-meta))))))))
    
(defn try-repeating-parse-strategy-with-header
  [grammar text start-production start-rule output-format]
  (profile (gll/clear!))
  (let [parsers (:parsers start-rule)
        repeating-parser (last parsers)]
    (if
      (not (and (= (:tag start-rule) :cat)
                (#{:star :plus} (:tag repeating-parser))
                (not (:hide repeating-parser))
                (not (:hide (:parser repeating-parser)))))
      failure-signal
      (let [header-parser (apply c/cat (butlast parsers))]
        (if (= (:red start-rule) red/raw-non-terminal-reduction)
          (repeat-parse-with-header grammar header-parser repeating-parser nil start-production text)
          (repeat-parse-with-header grammar header-parser repeating-parser output-format start-production text))))))
  
(defn try-repeating-parse-strategy
  [parser text start-production]
  (let [grammar (:grammar parser)
        output-format (:output-format parser)
        start-rule (get grammar start-production)]
    (profile (gll/clear!))
    (cond
      (= (:hide start-rule) true) failure-signal
      (= (:red start-rule) red/raw-non-terminal-reduction)
      (cond
        (= (:tag start-rule) :star)
        (repeat-parse grammar (:parser start-rule) output-format text)
        (= (:tag start-rule) :plus)
        (let [result (repeat-parse grammar (:parser start-rule) output-format text)]
          (if (empty-result? result)
            failure-signal
            result))
        :else (try-repeating-parse-strategy-with-header 
                grammar text start-production start-rule output-format))
              
      (= (:tag start-rule) :star)
      (repeat-parse grammar (:parser start-rule) output-format start-production text)
      (= (:tag start-rule) :plus)      
      (let [result (repeat-parse grammar (:parser start-rule) output-format start-production text)]
        (if (empty-result? result)
          failure-signal
          result))
      
      :else (try-repeating-parse-strategy-with-header 
                grammar text start-production start-rule output-format))))

(defn used-memory-optimization? [tree]
  (= :memory (-> tree meta :optimize)))

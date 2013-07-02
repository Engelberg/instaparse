(ns instaparse.repeat
  (:use instaparse.clone)
  (:require [instaparse.gll :as gll] 
            [instaparse.combinators-source :as c]
            [instaparse.viz :as viz]
            [instaparse.failure :as fail]))

(defn parse-from-index [parser text segment index]
  (let [grammar (:grammar parser)
        start (:start-production parser)
        tramp (gll/make-tramp grammar text segment)
        initial-parser (c/nt start)]
    (gll/push-listener tramp [index initial-parser] (gll/TopListener tramp))
    (if-let [all-parses (gll/run tramp)]
      (apply max-key (comp second span) all-parses) 
      (fail/augment-failure @(:failure tramp) text))))

(defn repeat-parse-hiccup [parser root-tag text]
  (let [length (count text)
        segment (gll/string->segment text)]
    (loop [index 0
           parses [root-tag]]
      (if (= index length) (gll/safe-with-meta
                             parses
                             {:start-index 0
                              :end-index length})
        (let [first-result (parse-from-index parser text segment index)
              [start end] (viz/span first-result)]
          (if (instance? instaparse.gll.Failure first-result) first-result
            (recur end (conj parses first-result))))))))
          
(defn repeat-parse-enlive [parser root-tag text]
  (let [length (count text)
        segment (gll/string->segment text)]
    (loop [index 0
           parses []]
      (if (= index length) (gll/safe-with-meta
                             {:tag root-tag :content (seq parses)}
                             {:start-index 0
                              :end-index length})
        (let [first-result (parse-from-index parser text segment index)
              [start end] (viz/span first-result)]
          (if (instance? instaparse.gll.Failure first-result) first-result
            (recur end (conj parses first-result))))))))

(defn repeat-parse-no-tag [parser text]
  (let [length (count text)
        segment (gll/string->segment text)]
    (loop [index 0
           parses []]
      (if (= index length) (gll/safe-with-meta 
                             (seq parses)
                             {:start-index 0
                              :end-index length})
        (let [first-result (parse-from-index parser text segment index)
              [start end] (viz/span first-result)]
          (if (instance? instaparse.gll.Failure first-result) first-result
            (recur end (conj parses first-result))))))))

(defn repeat-parse 
  ([parser text] (repeat-parse-no-tag parser text))
  ([parser root-tag text]
    {:pre [(#{:hiccup :enlive} (:output-format parser))]} 
    (let [output-format (:output-format parser)]
      (cond
        (= output-format :hiccup)
        (repeat-parse-hiccup parser root-tag text)
        (= output-format :enlive)
        (repeat-parse-enlive parser root-tag text)))))
        

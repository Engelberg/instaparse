(ns instaparse.repeat
  (:use instaparse.clone)
  (:require [instaparse.gll :as gll] 
            [instaparse.combinators-source :as c]
            [instaparse.failure :as fail]))

(defn parse-from-index [parser text index]
  (let [grammar (:grammar parser)
        start (:start-production parser)
        tramp (gll/make-tramp grammar text)
        initial-parser (c/nt start)]
    (gll/push-listener tramp [index initial-parser] (gll/TopListener tramp))
    (if-let [all-parses (gll/run tramp)]
      (first all-parses) 
      (fail/augment-failure @(:failure tramp) text))))

; Make better use of segments here

(defn repeat-parse [parser text]
  (let [length (count text)]
    (loop [index 0
           parses []]
      (if (= index length) parses ; need to attach span info        
        (let [first-result (parse-from-index parser text index)
              [start end] (span first-result)]
          (if (instance? instaparse.gll.Failure first-result) first-result
            (recur end (conj parses first-result))))))))
          



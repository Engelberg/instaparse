(ns instaparse.failure
  "Facilities for printing and manipulating error messages."
  (:import java.io.BufferedReader java.io.StringReader)
  (:require [instaparse.print :as print]))

(defn index->line-column
  "Takes an index into text, and determines the line and column info"
  [index text]
  (loop [line 1, col 1, counter 0]
    (cond
      (= index counter) {:line line :column col}
      (= \newline (get text counter)) (recur (inc line) 1 (inc counter))
      :else (recur line (inc col) (inc counter)))))

(defn get-line
  "Returns nth line of text, 1-based"
  [n text]
  (try (nth (line-seq (BufferedReader. (StringReader. text))) (dec n))
    (catch Exception e "")))

(defn marker
  "Creates string with caret at nth position, 1-based"
  [n]
  (when (integer? n)
    (if (<= n 1) "^"
      (apply str (concat (repeat (dec n) \space) [\^]))))) 
      
(defn augment-failure
  "Adds text, line, and column info to failure object."
  [failure text]  
  (let [lc (index->line-column (:index failure) text)]
    (merge failure 
           lc
           {:text (get-line (:line lc) text)})))

(defn print-reason
  "Provides special case for printing negative lookahead reasons"
  [r]
  (cond
    (:NOT r)
    (do (print "NOT ")    
      (println (:NOT r))),
    (instance? java.util.regex.Pattern r)
    (println (print/regexp->str r))
    :else
    (prn r)))

(defn pprint-failure
  "Takes an augmented failure object and prints the error message"
  [{:keys [line column text reason]}]
  (printf "Parse error at line %d, column %d:\n"
          line column)
  (println text)
  (println (marker column))
  (let [full-reasons (distinct (map :expecting
                                    (filter :full reason)))
        partial-reasons (distinct (map :expecting
                                       (filter (complement :full) reason)))
        total (+ (count full-reasons) (count partial-reasons))]        
    (cond (zero? total) nil
          (= 1 total) (println "Expected:")
          :else (println "Expected one of:"))
    (doseq [r full-reasons]
      (pr r)
      (println " (followed by end-of-string)"))
    (doseq [r partial-reasons]
      (print-reason r))))
  
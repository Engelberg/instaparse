(ns instaparse.errors
  (:import java.io.BufferedReader java.io.StringReader))

;; Facilities for printing and manipulating error messages

(defn index->line-column
  "Takes an index into text, and determines the line and column info"
  [index text]
  (loop [line 0, col 0, counter 0]
    (cond
      (= index counter) {:line line :column col}
      (= \newline (get text counter)) (recur (inc line) 0 (inc counter))
      :else (recur line (inc col) (inc counter)))))

(defn get-line
  "Returns nth line of text"
  [n text]
  (nth (line-seq (BufferedReader. (StringReader. text))) n ))

(defn marker
  "Creates string with caret at nth position"
  [n]
  (if (zero? n) "^"
    (apply str (concat (repeat (dec n) \space) [\^])))) 
      
(defn augment-failure
  "Adds text, line, and column info to failure object."
  [failure text]  
  (merge failure 
         (index->line-column (:index failure) text)
         {:text text}))

(defn pprint-failure
  "Takes an augmented failure object and prints the error message"
  [{:keys [line column text reason]}]
  (printf "Parse error at line %d, column %d:\n"
          (inc line) (inc column))
  (println (get-line line text))
  (println (marker column))
  (println "Expected one of:")
  (doseq [r reason]
    (println (:expecting r))))
  
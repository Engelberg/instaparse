(ns instaparse.failure
  "Facilities for printing and manipulating error messages."
  #?(:clj (:import java.io.BufferedReader java.io.StringReader))
  (:require [instaparse.print :as print]))

(defn index->line-column
  "Takes an index into text, and determines the line and column info"
  [index text]
  (loop [line 1, col 1, counter 0]
    (cond
      (= index counter) {:line line :column col}
      (= \newline (get text counter)) (recur (inc line) 1 (inc counter))
      :else (recur line (inc col) (inc counter)))))

#?(:clj
   (defn get-line
     "Returns nth line of text, 1-based"
     [n text]
     (try (nth (line-seq (BufferedReader. (StringReader. (str text)))) (dec n))
          (catch Exception e "")))
   :cljs
   (defn get-line
     [n text]
     (loop [chars (seq (clojure.string/replace text "\r\n" "\n"))
            n n]
       (cond
         (empty? chars) ""
         (= n 1) (apply str (take-while (complement #{\newline}) chars))
         (= \newline (first chars)) (recur (next chars) (dec n))
         :else (recur (next chars) n)))))

(defn marker
  "Creates string with caret at nth position, 1-based
   and accounts for horizontal tabs which might change
   the alignment of the '^' to the error location."
  [text n]
  (when (and text (integer? n))
    (let [marker-text (clojure.string/replace text #"[^\s]" " ")]
      (if (<= n 1)
        "^"
        (str (subs marker-text 0 (dec n)) \^)))))

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
        (print (:NOT r))),
    (:char-range r)
    (print (print/char-range->str r))
    (instance? #?(:clj java.util.regex.Pattern
                  :cljs js/RegExp)
               r)
    (print (print/regexp->str r))
    :else
    (pr r)))

(defn pprint-failure
  "Takes an augmented failure object and prints the error message"
  [{:keys [line column text reason]}]
  (println (str "Parse error at line " line ", column " column ":"))
  (println text)
  (println (marker text column))
  (let [full-reasons (distinct (map :expecting
                                    (filter :full reason)))
        partial-reasons (distinct (map :expecting
                                       (filter (complement :full) reason)))
        total (+ (count full-reasons) (count partial-reasons))]
    (cond (zero? total) nil
          (= 1 total) (println "Expected:")
          :else (println "Expected one of:"))
    (doseq [r full-reasons]
      (print-reason r)
      (println " (followed by end-of-string)"))
    (doseq [r partial-reasons]
      (print-reason r)
      (println))))

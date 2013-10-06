(ns instaparse.cfg
  "This is the context free grammar that recognizes context free grammars."
  (:use instaparse.combinators-source)
  (:use [instaparse.reduction :only [apply-standard-reductions]])
  (:use [instaparse.gll :only [parse]])
  (:require [clojure.string :as str]))

(def single-quoted-string #"'[^'\\]*(?:\\.[^'\\]*)*'(?x) #Single-quoted string")
(def single-quoted-regexp #"#'[^'\\]*(?:\\.[^'\\]*)*'(?x) #Single-quoted regexp")
(def double-quoted-string #"\"[^\"\\]*(?:\\.[^\"\\]*)*\"(?x) #Double-quoted string")
(def double-quoted-regexp #"#\"[^\"\\]*(?:\\.[^\"\\]*)*\"(?x) #Double-quoted regexp")
(def inside-comment #"(?s)(?:(?!(?:\(\*|\*\))).)*(?x) #Comment text")
(def ws "[,\\s]*(?x) #optional whitespace")

(def opt-whitespace (hide (nt :opt-whitespace)))

(def cfg 
  (apply-standard-reductions 
    :hiccup    ; use the hiccup output format 
    {:rules (hide-tag (cat opt-whitespace
                           (plus (nt :rule))))
     :comment (cat (string "(*") (nt :inside-comment) (string "*)"))
     :inside-comment (cat (regexp inside-comment)
                          (star (cat (nt :comment)
                                     (regexp inside-comment))))
     :opt-whitespace (cat (regexp ws)
                          (star (cat (nt :comment)
                                     (regexp ws))))
     :rule-separator (alt (string ":")
                          (string ":=")
                          (string "::=")
                          (string "="))
     :rule (cat (alt (nt :nt)
                     (nt :hide-nt))
                opt-whitespace
                (hide (nt :rule-separator))
                opt-whitespace
                (nt :alt-or-ord)
                (hide (alt (nt :opt-whitespace)
                           (cat (nt :opt-whitespace) (alt (string ";") (string ".")) (nt :opt-whitespace)))))          
     :nt (cat
           (neg (nt :epsilon))
           (regexp "[^, \\r\\t\\n<>(){}\\[\\]+*?:=|'\"#&!;./]+(?x) #Non-terminal"))
          :hide-nt (cat (hide (string "<"))
                        opt-whitespace
                        (nt :nt)
                        opt-whitespace
                        (hide (string ">")))
          :alt-or-ord (hide-tag (alt (nt :alt) (nt :ord)))
          :alt (cat (nt :cat)                           
                    (star
                      (cat
                        opt-whitespace
                        (hide (string "|"))
                        opt-whitespace
                        (nt :cat))))
          :ord (cat (nt :cat)
                    (plus
                      (cat
                        opt-whitespace
                        (hide (string "/"))
                        opt-whitespace
                        (nt :cat))))
          :paren (cat (hide (string "("))
                      opt-whitespace
                      (nt :alt-or-ord)
                      opt-whitespace
                      (hide (string ")")))
          :hide (cat (hide (string "<"))
                     opt-whitespace	
                     (nt :alt-or-ord)
                     opt-whitespace
                     (hide (string ">")))
          :cat (plus (cat
                       opt-whitespace
                       (alt (nt :factor) (nt :look) (nt :neg))
                       opt-whitespace))
          :string (alt
                    (regexp single-quoted-string)
                    (regexp double-quoted-string))
          :regexp (alt
                    (regexp single-quoted-regexp)
                    (regexp double-quoted-regexp))
          :opt (alt
                 (cat (hide (string "["))
                      opt-whitespace
                      (nt :alt-or-ord)
                      opt-whitespace
                      (hide (string "]")))
                 (cat (nt :factor)
                      opt-whitespace
                      (hide (string "?"))))
          :star (alt
                  (cat (hide (string "{"))
                       opt-whitespace
                       (nt :alt-or-ord)
                       opt-whitespace
                       (hide (string "}")))
                  (cat (nt :factor)
                       opt-whitespace
                       (hide (string "*"))))
          :plus (cat (nt :factor)
                     opt-whitespace
                     (hide (string "+")))
          :look (cat (hide (string "&"))
                     opt-whitespace
                     (nt :factor))
          :neg (cat (hide (string "!"))
                    opt-whitespace
                    (nt :factor))
          :epsilon (alt (string "Epsilon")
                        (string "epsilon")
                        (string "EPSILON")
                        (string "eps")
                        (string "\u03b5"))
          :factor (hide-tag (alt (nt :nt)
                                 (nt :string)
                                 (nt :regexp)
                                 (nt :opt)     
                                 (nt :star)
                                 (nt :plus)
                                 (nt :paren)
                                 (nt :hide)
                                 (nt :epsilon)))}))

; Internally, we're converting the grammar into a hiccup parse tree
; Here's how you extract the relevant information
(def tag first)
(def contents next)
(def content fnext)

;;;; Helper functions for reading strings and regexes

(defn escape
  "Converts escaped single-quotes to unescaped, and unescaped double-quotes to escaped"
  [s]
  (loop [sq (seq s), v []]
    (if-let [c (first sq)]
      (case c
        \\ (if-let [c2 (second sq)]
             (if (= c2 \')
               (recur (drop 2 sq) (conj v c2))
               (recur (drop 2 sq) (conj v c c2)))
             (throw (RuntimeException. (format "Encountered backslash character at end of string: %s" s))))
        \" (recur (next sq) (conj v \\ \"))
        (recur (next sq) (conj v c)))
      (apply str v))))                     

;(defn safe-read-string [s]
;  (binding [*read-eval* false]
;    (read-string s)))

(let [string-reader (clojure.lang.LispReader$StringReader.)]
  (defn safe-read-string
    "Expects a double-quote at the end of the string"
    [s]
    (with-in-str s (string-reader *in* nil))))

; I think re-pattern is sufficient, but here's how to do it without.
;(let [regexp-reader (clojure.lang.LispReader$RegexReader.)]
;  (defn safe-read-regexp
;    "Expects a double-quote at the end of the string"
;    [s]
;    (with-in-str s (regexp-reader *in* nil))))

(defn process-string
  "Converts single quoted string to double-quoted"
  [s]
  (let [stripped
        (subs s 1 (dec (count s)))
        remove-escaped-single-quotes
        (escape stripped)
        final-string
        (safe-read-string (str remove-escaped-single-quotes \"))]            

    final-string))

(defn process-regexp
  "Converts single quoted regexp to double-quoted"
  [s]
  ;(println (with-out-str (pr s)))
  (let [stripped
        (subs s 2 (dec (count s)))
        remove-escaped-single-quotes
        (escape stripped)
        final-string
        (re-pattern remove-escaped-single-quotes)]
;        (safe-read-regexp (str remove-escaped-single-quotes \"))]
        
    final-string))

;;; Now we need to convert the grammar's parse tree into combinators

(defn build-rule
  "Convert one parsed rule from the grammar into combinators"
  [tree]
  (case (tag tree)
    :rule (let [[nt alt-or-ord] (contents tree)]
            (if (= (tag nt) :hide-nt)
              [(keyword (content (content nt)))
               (hide-tag (build-rule alt-or-ord))]
              [(keyword (content nt))
               (build-rule alt-or-ord)]))
    :nt (nt (keyword (content tree)))
    :alt (apply alt (map build-rule (contents tree)))
    :ord (apply ord (map build-rule (contents tree)))
    :paren (recur (content tree))
    :hide (hide (build-rule (content tree)))
    :cat (apply cat (map build-rule (contents tree)))
    :string (string (process-string (content tree)))
    :regexp (regexp (process-regexp (content tree)))
    :opt (opt (build-rule (content tree)))
    :star (star (build-rule (content tree)))
    :plus (plus (build-rule (content tree)))
    :look (look (build-rule (content tree)))
    :neg (neg (build-rule (content tree)))
    :epsilon Epsilon))

(defn seq-nt
  "Returns a sequence of all non-terminals in a parser built from combinators."
  [parser]
  (case (:tag parser)
    :nt [(:keyword parser)]
    (:string :string-ci :regexp :epsilon) []
    (:opt :plus :star :look :neg :rep) (recur (:parser parser))
    (:alt :cat) (mapcat seq-nt (:parsers parser))
    :ord (mapcat seq-nt 
                 [(:parser1 parser) (:parser2 parser)])))                 
    
(defn check-grammar
  "Throw error if grammar uses any invalid non-terminals in its productions"
  [grammar-map]
  (let [valid-nts (set (keys grammar-map))]
    (doseq [nt (distinct (mapcat seq-nt (vals grammar-map)))]
      (when-not (valid-nts nt)
        (throw (RuntimeException. (format "%s occurs on the right-hand side of your grammar, but not on the left"
                                          (subs (str nt) 1)))))))
  grammar-map)
          
(defn build-parser [spec output-format]
  (let [rules (parse cfg :rules spec false)]
    (if (instance? instaparse.gll.Failure rules)
      (throw (RuntimeException. (str "Error parsing grammar specification:\n"
                                    (with-out-str (println rules)))))
      (let [productions (map build-rule rules)
            start-production (first (first productions))] 
        {:grammar (check-grammar (apply-standard-reductions output-format (into {} productions)))
         :start-production start-production
         :output-format output-format}))))

(defn build-parser-from-combinators [grammar-map output-format start-production]
  (if (nil? start-production)
    (throw (IllegalArgumentException. 
             "When you build a parser from a map of parser combinators, you must provide a start production using the :start keyword argument."))
    {:grammar (check-grammar (apply-standard-reductions output-format grammar-map))
     :start-production start-production
     :output-format output-format}))

(defn ebnf
  "Takes an EBNF grammar specification string and returns the combinator version.
If you give it the right-hand side of a rule, it will return the combinator equivalent.
If you give it a series of rules, it will give you back a grammar map.   
Useful for combining with other combinators."
  [spec]
  (if (re-find #"[:=]" spec)    
    (let [rules (parse cfg :rules spec false)]
      (if (instance? instaparse.gll.Failure rules)
        (throw (RuntimeException. (str "Error parsing grammar specification:\n"
                                       (with-out-str (println rules)))))    
        (into {} (map build-rule rules))))
    (let [rhs (parse cfg :alt-or-ord spec false)]
      (if (instance? instaparse.gll.Failure rhs)
        (throw (RuntimeException. (str "Error parsing grammar specification:\n"
                                       (with-out-str (println rhs)))))          
        (build-rule (first rhs))))))      
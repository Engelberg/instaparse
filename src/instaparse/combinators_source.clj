(ns instaparse.combinators-source
  "This is the underlying implementation of the various combinators."
  (:use instaparse.reduction))

;; Ways to build parsers

(def Epsilon {:tag :epsilon})

(defn opt "Optional, i.e., parser?"
  [parser] 
  (if (= parser Epsilon) Epsilon
    {:tag :opt :parser parser}))

(defn plus "One or more, i.e., parser+"
  [parser]
  (if (= parser Epsilon) Epsilon
    {:tag :plus :parser parser}))

(defn star "Zero or more, i.e., parser*"
  [parser] 
  (if (= parser Epsilon) Epsilon
    {:tag :star :parser parser}))

(defn alt "Alternation, i.e., parser1 | parser2 | parser3 | ..."
  [& parsers] 
  (cond
    (every? (partial = Epsilon) parsers) Epsilon
    (singleton? parsers) (first parsers)
    :else {:tag :alt :parsers parsers}))

;(declare neg)
(defn- ord2 [parser1 parser2]
  (cond
    (= parser1 Epsilon) Epsilon
    (= parser2 Epsilon) parser1
    :else
    ;(alt parser1 (cat (neg parser1) parser2))))
    {:tag :ord :parser1 parser1 :parser2 parser2}))

(defn ord "Ordered choice, i.e., parser1 / parser2"
  [& parsers]
  (if (seq parsers)
    (ord2 (first parsers) (apply ord (rest parsers)))
    Epsilon))

(defn cat "Concatenation, i.e., parser1 parser2 ..."
  [& parsers]
  (if (every? (partial = Epsilon) parsers) Epsilon
    (let [parsers (remove #{Epsilon} parsers)]
      (if (singleton? parsers) (first parsers) ; apply vector reduction
        {:tag :cat :parsers parsers}))))

(defn string "Create a string terminal out of s" 
  [s] 
  (if (= s "") Epsilon
    {:tag :string :string s}))

(defn regexp "Create a regexp terminal out of regular expression r"
  [r]
  (let [s (str \^ r)]
    (if (= s "^") Epsilon
      {:tag :regexp :regexp (re-pattern s)})))

(defn nt "Refers to a non-terminal defined by the grammar map"
  [s] 
  {:tag :nt :keyword s})

(defn look "Lookahead, i.e., &parser" 
  [parser] 
  {:tag :look :parser parser}) 

(defn neg "Negative lookahead, i.e., !parser"
  [parser] 
  {:tag :neg :parser parser})

(defn hide "Hide the result of parser, i.e., <parser>"
  [parser] 
  (assoc parser :hide true))

(defn hide-tag "Hide the tag associated with this right-hand-side.  
  Wrap this combinator around the entire right-hand side."  
  [parser]
  (red parser raw-non-terminal-reduction))

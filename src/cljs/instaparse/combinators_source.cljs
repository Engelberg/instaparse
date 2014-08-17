(ns instaparse.combinators-source
  "This is the underlying implementation of the various combinators."
  (:require [instaparse.reduction :refer [singleton? red
                                          raw-non-terminal-reduction
                                          reduction-types]]))

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

(defn rep "Between m and n repetitions"
  [m n parser]
  {:pre [(<= m n)]}
  (if (= parser Epsilon) Epsilon
    {:tag :rep :parser parser :min m :max n}))

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

(defn string-ci "Create a case-insensitive string terminal out of s" 
  [s] 
  (if (= s "") Epsilon
    {:tag :string-ci :string s}))

(defn- regexp-to-str
  "(str regexp) in clojurescript puts slashes around the result, unlike
   in clojure. Work around that."
  [r]
  (if (regexp? r)
    (let [s (str r)]
      (subs s 1 (dec (count s))))
    r))

(defn regexp "Create a regexp terminal out of regular expression r"
  [r]
  (let [s (str \^ (regexp-to-str r))]
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

(defn hide-tag "Hide the tag associated with this rule.  
  Wrap this combinator around the entire right-hand side."  
  [parser]
  (red parser raw-non-terminal-reduction))

; Ways to alter a parser with hidden information, unhiding that information

(defn hidden-tag?
  "Tests whether parser was created with hide-tag combinator"
  [parser]
  (= (:red parser) raw-non-terminal-reduction))

(defn unhide-content
  "Recursively undoes the effect of hide on one parser"
  [parser]
  (let [parser (if (:hide parser) (dissoc parser :hide) parser)]
    (cond
      (:parser parser) (assoc parser :parser (unhide-content (:parser parser)))
      (:parsers parser) (assoc parser :parsers (map unhide-content (:parsers parser)))
      :else parser)))

(defn unhide-all-content
  "Recursively undoes the effect of hide on all parsers in the grammar"
  [grammar]
  (into {} (for [[k v] grammar]
             [k (unhide-content v)])))

(defn unhide-tags 
  "Recursively undoes the effect of hide-tag"
  [reduction-type grammar]
  (if-let [reduction (reduction-types reduction-type)]
    (into {} (for [[k v] grammar]
               [k (assoc v :red (reduction k))]))
    (throw (str "Invalid output format" reduction-type ". Use :enlive or :hiccup."))))

(defn unhide-all
  "Recursively undoes the effect of both hide and hide-tag"
  [reduction-type grammar]
  (if-let [reduction (reduction-types reduction-type)]
    (into {} (for [[k v] grammar]
               [k (assoc (unhide-content v) :red (reduction k))]))
    (throw (str "Invalid output format" reduction-type ". Use :enlive or :hiccup."))))

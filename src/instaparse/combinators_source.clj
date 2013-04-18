(ns instaparse.combinators-source
  "This is the underlying implementation of the various combinators."
  (:use instaparse.reduction))

;; Ways to build parsers

(def Epsilon {:tag :epsilon})

(defn opt [parser] 
  (if (= parser Epsilon) Epsilon
    {:tag :opt :parser parser}))

(defn plus [parser]
  (if (= parser Epsilon) Epsilon
    {:tag :plus :parser parser}))

(defn star [parser] 
  (if (= parser Epsilon) Epsilon
    {:tag :star :parser parser}))

(defn alt [& parsers] 
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

(defn ord [& parsers]
  (if (seq parsers)
    (ord2 (first parsers) (apply ord (rest parsers)))
    Epsilon))

(defn cat [& parsers]
  (if (every? (partial = Epsilon) parsers) Epsilon
    (let [parsers (remove #{Epsilon} parsers)]
      (if (singleton? parsers) (first parsers) ; apply vector reduction
        {:tag :cat :parsers parsers}))))

(defn string [s] 
  (if (= s "") Epsilon
    {:tag :string :string s}))

(defn regexp [r]
  (let [s (str \^ r)]
    (if (= s "^") Epsilon
      {:tag :regexp :regexp (re-pattern s)})))

(defn nt [s] {:tag :nt :keyword s})

(defn look [parser] {:tag :look :parser parser}) 

(defn neg [parser] {:tag :neg :parser parser})

(defn hide [parser] (assoc parser :hide true))

(defn hide-tag [parser]
  (red parser raw-non-terminal-reduction))

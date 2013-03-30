(ns instaparse.print
  "Facilities for taking parsers and grammars, and converting them to strings.
   Used for pretty-printing."
  (:require [clojure.string :as str]))

(declare parser->str) ; mutual recursion

(defn paren-for-tags [tag-set parser]
  (if (tag-set (parser :tag))
    (str "(" (parser->str parser) ")")
    (parser->str parser)))

(def paren-for-compound 
  (partial paren-for-tags #{:alt :ord :cat}))

(defn parser->str
  "Stringifies a parser built from combinators"
  [{:keys [parser parser1 parser2 parsers tag] :as p}]
  (case tag
    :epsilon "\u03b5"
    :opt (str (paren-for-compound parser) "?")
    :plus (str (paren-for-compound parser) "+")
    :star (str (paren-for-compound parser) "*")
    :alt (str/join " | " (map (partial paren-for-tags #{:ord}) parsers))
    :ord (str (paren-for-tags #{:alt} parser1)
              " / "
              (paren-for-tags #{:alt} parser2))
    :cat (str/join " " (map (partial paren-for-tags #{:alt :ord}) parsers))
    :string (with-out-str (pr (:string p)))
    ; Quirkily, Clojure doesn't pr regexps with escaped whitespace characters,
    ; so we have to go through some extra convolutions to get that behavior
    :regexp (str "#" (with-out-str (pr (subs (str (:regexp p)) 1))))
    :nt (subs (str (:keyword p)) 1)
    :look (str "&" (paren-for-compound parser))
    :neg (str "!" (paren-for-compound parser))
    :hide (str "<" (parser->str parser) ">")))
              
(defn rule->str
  "Takes a terminal symbol and a parser built from combinators,
   and returns a string for the rule."
  [terminal parser]
  (str (subs (str terminal) 1)
       " = " 
       (parser->str parser)))

(defn Parser->str
  "Takes a Parser object, i.e., something with a grammar map and a start 
   production keyword, and stringifies it." 
  [{grammar :grammar start :start-production}]
  (str/join \newline
            (cons
              ; Put starting production first
              (rule->str start (grammar start))
              ; Then the others
              (for [[terminal parser] grammar
                    :when (not= terminal start)]
                (rule->str terminal parser)))))
(ns instaparse.print
  "Facilities for taking parsers and grammars, and converting them to strings.
   Used for pretty-printing."
  (:require [clojure.string :as str]))

(declare combinators->str) ; mutual recursion

(defn paren-for-tags [tag-set parser]
  (if (tag-set (parser :tag))
    (str "(" (combinators->str parser) ")")
    (combinators->str parser)))

(def paren-for-compound 
  (partial paren-for-tags #{:alt :ord :cat}))

(defn regexp-replace
  "Replaces whitespace characters with escape sequences for better printing" 
  [s]
  (case s
    "\n" "\\n"
    "\b" "\\b"
    "\f" "\\f"
    "\r" "\\r"
    "\t" "\\t"
    s)) 

(defn regexp->str [r]
  (str/replace 
    (str "#\"" (subs (str r) 1) "\"")
    #"[\s]" regexp-replace))

(defn combinators->str
  "Stringifies a parser built from combinators"
  [{:keys [parser parser1 parser2 parsers tag] :as p}]
  (case tag
    :epsilon "\u03b5"
    :opt (str (paren-for-compound parser) "?")
    :plus (str (paren-for-compound parser) "+")
    :star (str (paren-for-compound parser) "*")
    :rep (if (not= (:min p) (:max p))
           (str (paren-for-compound parser) \{ 
                (:min p) \, (:max p) \})
           (str (paren-for-compound parser) \{ 
                (:min p)\}))
    :alt (str/join " | " (map (partial paren-for-tags #{:ord}) parsers))
    :ord (str (paren-for-tags #{:alt} parser1)
              " / "
              (paren-for-tags #{:alt} parser2))
    :cat (str/join " " (map (partial paren-for-tags #{:alt :ord}) parsers))
    :string (with-out-str (pr (:string p)))
    :string-ci (with-out-str (pr (:string p)))
    :regexp (regexp->str (:regexp p))
    :nt (subs (str (:keyword p)) 1)
    :look (str "&" (paren-for-compound parser))
    :neg (str "!" (paren-for-compound parser))
    :hide (str "<" (combinators->str parser) ">")))
              
(defn rule->str
  "Takes a terminal symbol and a parser built from combinators,
   and returns a string for the rule."
  [terminal parser]
  (str (subs (str terminal) 1)
       " = " 
       (combinators->str parser)))

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

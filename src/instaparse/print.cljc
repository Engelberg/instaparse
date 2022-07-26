(ns instaparse.print
  "Facilities for taking parsers and grammars, and converting them to strings.
   Used for pretty-printing."
  (:require [clojure.string :as str]))

(declare combinators->str) ; mutual recursion

(defn paren-for-tags [tag-set hidden? parser]
  (if (and (not hidden?) (tag-set (parser :tag)))
    (str "(" (combinators->str parser false) ")")
    (combinators->str parser false)))

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
    (str "#\""
         #?(:clj (str r)
            :cljs (subs (.-source r) 1))
         "\"")
    #"[\s]" regexp-replace))

#?(:clj
   (defn char-range->str [{:keys [lo hi]}]
     (if (= lo hi)
       (format "%%x%04x" lo)
       (format "%%x%04x-%04x" lo hi)))

   :cljs
   (do
     (defn number->hex-padded [n]
       (if (<= n 0xFFF)
         (.substr (str "0000" (.toString n 16)) -4)
         (.toString n 16)))

     (defn char-range->str [{:keys [lo hi]}]
       (if (= lo hi)
         (str "%x" (number->hex-padded lo))
         (str "%x" (number->hex-padded lo) "-" (number->hex-padded hi))))))

(defn combinators->str
  "Stringifies a parser built from combinators"
  ([p] (combinators->str p false))
  ([{:keys [parser parser1 parser2 parsers tag] :as p} hidden?]
    (if (and (not hidden?) (:hide p))
      (str \< (combinators->str p true) \>)
      (case tag
        :epsilon "\u03b5"
        :opt (str (paren-for-compound hidden? parser) "?")
        :plus (str (paren-for-compound hidden? parser) "+")
        :star (str (paren-for-compound hidden? parser) "*")
        :rep (if (not= (:min p) (:max p))
               (str (paren-for-compound hidden? parser) \{
                    (:min p) \, (:max p) \})
               (str (paren-for-compound hidden? parser) \{
                    (:min p) \}))
        :alt (str/join " | " (map (partial paren-for-tags #{:ord} hidden?) parsers))
        :ord (str (paren-for-tags #{:alt} hidden? parser1)
                  " / "
                  (paren-for-tags #{:alt} hidden? parser2))
        :cat (str/join " " (map (partial paren-for-tags #{:alt :ord} hidden?) parsers))
        :string (with-out-str (pr (:string p)))
        :string-ci (with-out-str (pr (:string p)))
        :char (char-range->str p)
        :regexp (regexp->str (:regexp p))
        :nt (subs (str (:keyword p)) 1)
        :look (str "&" (paren-for-compound hidden? parser))
        :neg (str "!" (paren-for-compound hidden? parser))))))

(defn non-terminal->str [non-terminal]
  (if-let  [ns (namespace non-terminal)]
    (str ns "/" (name non-terminal))
    (name non-terminal)))

(defn rule->str
  "Takes a non-terminal symbol and a parser built from combinators,
   and returns a string for the rule."
  [non-terminal parser]
  (if (= (-> parser :red :reduction-type) :raw)
    (str \< (name non-terminal) \>
         " = "
         (combinators->str parser))
    (str (non-terminal->str non-terminal)
         " = "
         (combinators->str parser))))

(defn Parser->str
  "Takes a Parser object, i.e., something with a grammar map and a start
   production keyword, and stringifies it."
  [{grammar :grammar start :start-production}]
  (str/join \newline
            (cons
              ; Put starting production first
              (rule->str start (grammar start))
              ; Then the others
              (for [[non-terminal parser] grammar
                    :when (not= non-terminal start)]
                (rule->str non-terminal parser)))))

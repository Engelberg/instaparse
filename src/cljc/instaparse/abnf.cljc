(ns instaparse.abnf
  "This is the context free grammar that recognizes ABNF notation."
  (:refer-clojure :exclude  [cat])
  (:require [instaparse.transform :as t]
            [instaparse.cfg :as cfg]
            [instaparse.gll :as gll]
            [instaparse.reduction :as red]
            [instaparse.combinators-source :refer
             [Epsilon opt plus star rep alt ord cat string-ci string
              string-ci regexp nt look neg hide hide-tag unicode-char]]
            [goog.string.format]))

(def ^:dynamic *case-insensitive*
  "This is normally set to false, in which case the non-terminals
are treated as case-sensitive, which is NOT the norm
for ABNF grammars. If you really want case-insensitivity,
bind this to true, in which case all non-terminals
will be converted to upper-case internally (which
you'll have to keep in mind when transforming)."
  false)

(def abnf-core
  {:ALPHA (regexp "[a-zA-Z]")
   :BIT (regexp "[01]")
   :CHAR (regexp "[\\u0001-\\u007F]")
   :CR (string "\u000D")
   :CRLF (string "\u000D\u000A")
   :CTL (regexp "[\\u0000-\\u001F|\\u007F]")
   :DIGIT (regexp "[0-9]")
   :DQUOTE (string "\u0022")
   :HEXDIG (regexp "[0-9a-fA-F]")
   :HTAB (string "\u0009")
   :LF (string "\u000A")
   :LWSP (alt (alt (string "\u0020") (string "\u0009")) ;WSP
              (star
                (cat (string "\u000D\u000A") ;CRLF
                     (alt (string "\u0020") (string "\u0009"))))) ;WSP
   :OCTET (regexp "[\\u0000-\\u00FF]")
   :SP (string "\u0020")
   :VCHAR (regexp "[\\u0021-\\u007E]")
   :WSP (alt (string "\u0020")     ;SP
             (string "\u0009"))})  ;HTAB

(def abnf-grammar
  "
<rulelist> = <opt-whitespace> (rule | hide-tag-rule)+;
rule = rulename-left <defined-as> alternation <opt-whitespace>;
hide-tag-rule = hide-tag <defined-as> alternation <opt-whitespace>;
rulename-left = rulename;
rulename-right = rulename;
<rulename> = #'[a-zA-Z][-a-zA-Z0-9]*';
<hide-tag> = <'<' opt-whitespace> rulename-left <opt-whitespace '>'>;
defined-as = <opt-whitespace> ('=' | '=/') <opt-whitespace>;
alternation = concatenation (<opt-whitespace '/' opt-whitespace> concatenation)*;
concatenation = repetition (<whitespace> repetition)*;
repetition = [repeat] <opt-whitespace> element;
repeat = NUM | (NUM? '*' NUM?);
<element> = rulename-right | group | hide | option | char-val | num-val
          | look | neg | regexp;
look = <'&' opt-whitespace> element;
neg = <'!' opt-whitespace> element;
<group> = <'(' opt-whitespace> alternation <opt-whitespace ')'>;
option = <'[' opt-whitespace> alternation <opt-whitespace ']'>;
hide = <'<' opt-whitespace> alternation <opt-whitespace '>'>;
char-val = <'\\u0022'> #'[\\u0020-\\u0021\\u0023-\\u007E]'* <'\\u0022'> (* double-quoted strings *)
         | <'\\u0027'> #'[\\u0020-\\u0026\u0028-\u007E]'* <'\\u0027'>;  (* single-quoted strings *)
<num-val> = <'%'> (bin-val | dec-val | hex-val);
bin-val = <'b'> bin-char
          [ (<'.'> bin-char)+ | ('-' bin-char) ];
bin-char = ('0' | '1')+;
dec-val = <'d'> dec-char
          [ (<'.'> dec-char)+ | ('-' dec-char) ];
dec-char = DIGIT+;
hex-val = <'x'> hex-char
          [ (<'.'> hex-char)+ | ('-' hex-char) ];
hex-char = HEXDIG+;
NUM = DIGIT+;
<DIGIT> = #'[0-9]';
<HEXDIG> = #'[0-9a-fA-F]';
opt-whitespace = #'\\s*(?:;.*?(?:\\u000D?\\u000A\\s*|$))*';
whitespace = #'\\s+(?:;.*?\\u000D?\\u000A\\s*)*';
regexp = #\"#'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'\"
       | #\"#\\\"[^\\\"\\\\]*(?:\\\\.[^\\\"\\\\]*)*\\\"\"
")

(defn format
  "Formats a string using goog.string.format."
  [fmt & args]
  (apply goog.string/format fmt args))

(defn get-char-combinator
  [& nums]
  (cond
    (= "-" (second nums)) (let [[lo _ hi] nums]
                            (unicode-char lo hi))
    :else (apply cat (for [n nums]
                       (unicode-char n)))))

(defn project
  "Restricts map to certain keys"
  [m ks]
  (into {}
        (for [k ks
              :when (contains? m k)]
          [k (m k)])))
          
(defn merge-core
  "Merges abnf-core map in with parsed grammar map"
  [grammar-map]
  (merge
    (project abnf-core (distinct (mapcat cfg/seq-nt (vals grammar-map))))
    grammar-map))

(defn hide-tag?
  "Tests whether parser was constructed with hide-tag"
  [p]
  (= (:red p) red/raw-non-terminal-reduction))

(defn alt-preserving-hide-tag [p1 p2]
  (let [hide-tag-p1? (hide-tag? p1)
        hide-tag-p2? (hide-tag? p2)]
    (cond
      (and hide-tag-p1? hide-tag-p2?)
      (hide-tag (alt (dissoc p1 :red) (dissoc p2 :red)))
      hide-tag-p1?
      (hide-tag (alt (dissoc p1 :red) p2))
      hide-tag-p2?
      (hide-tag (alt p1 (dissoc p2 :red)))
      :else
      (alt p1 p2))))
        
(def abnf-transformer
  {   
   :rule hash-map
   :hide-tag-rule (fn [tag rule] {tag (hide-tag rule)})
   :rulename-left #(if *case-insensitive*
                     (keyword (clojure.string/upper-case (apply str %&)))
                     (keyword (apply str %&)))
   :rulename-right #(if *case-insensitive*
                      (nt (keyword (clojure.string/upper-case (apply str %&))))
                      (nt (keyword (apply str %&))))
   ; since rulenames are case insensitive, convert it to upper case internally to be consistent
   :alternation alt
   :concatenation cat
   :repeat (fn [& items]
             (case (count items)
               1 (cond
                   (= (first items) "*") {}                         ; *
                   :else {:low (first items), :high (first items)}) ; x
               2 (cond
                   (= (first items) "*") {:high (second items)}     ; *x
                   :else {:low (first items)})                      ; x*
               3 {:low (first items), :high (nth items 2)}))        ; x*y
                 
   :repetition (fn 
                 ([repeat element]
                   (cond
                     (empty? repeat) (star element)
                     (= (count repeat) 2) (rep (:low repeat) (:high repeat) element)
                     (= (:low repeat) 1) (plus element)
                     (= (:high repeat) 1) (opt element)
                     :else (rep (or (:low repeat) 0)
                                (or (:high repeat) js/Infinity)
                                element)))
                 ([element]
                   element))
   :option opt
   :hide hide
   :look look
   :neg neg
   :regexp (comp regexp cfg/process-regexp)
   :char-val (fn [& cs]
               ; case insensitive string
               (string-ci (apply str cs)))
   :bin-char (fn [& cs]
               (js/parseInt (apply str cs) 2))
   :dec-char (fn [& cs]
               (js/parseInt (apply str cs)))
   :hex-char (fn [& cs]
               (js/parseInt (apply str cs) 16))
   :bin-val get-char-combinator
   :dec-val get-char-combinator
   :hex-val get-char-combinator
   :NUM #(js/parseInt (apply str %&))})

(def abnf-parser (red/apply-standard-reductions 
                   :hiccup (cfg/ebnf abnf-grammar)))

(defn rules->grammar-map
  [rules]
  (merge-core (apply merge-with alt-preserving-hide-tag rules)))

(defn abnf
  "Takes an ABNF grammar specification string and returns the combinator version.
  If you give it the right-hand side of a rule, it will return the combinator equivalent.
  If you give it a series of rules, it will give you back a grammar map.   
  Useful for combining with other combinators."
  [spec]
  (if (re-find #"=" spec)
    (let [rule-tree (gll/parse abnf-parser :rulelist spec false)]
      (if (instance? instaparse.gll.Failure rule-tree)
        (throw (str "Error parsing grammar specification:\n"
                    (with-out-str (println rule-tree))))
        (rules->grammar-map (t/transform abnf-transformer rule-tree))))      

    (let [rhs-tree (gll/parse abnf-parser :alternation spec false)]
      (if (instance? instaparse.gll.Failure rhs-tree)
        (throw (str "Error parsing grammar specification:\n"
                    (with-out-str (println rhs-tree))))        
        (t/transform abnf-transformer rhs-tree)))))

(defn build-parser [spec output-format]
  (let [rule-tree (gll/parse abnf-parser :rulelist spec false)]
    (if (instance? instaparse.gll.Failure rule-tree)
      (throw (str "Error parsing grammar specification:\n"
                  (with-out-str (println rule-tree))))
      (let [rules (t/transform abnf-transformer rule-tree)
            grammar-map (rules->grammar-map rules)
            start-production (first (first (first rules)))] 
        {:grammar (cfg/check-grammar (red/apply-standard-reductions output-format grammar-map))
         :start-production start-production
         :output-format output-format}))))


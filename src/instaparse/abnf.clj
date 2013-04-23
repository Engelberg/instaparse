(ns instaparse.abnf
  "This is the context free grammar that recognizes ABNF notation."
  (:use instaparse.core)
  (:use instaparse.combinators))

(def abnf-basic
  "
ALPHA = #'[a-zA-Z]';
BIT = '0' | '1';
CHAR = #'[\u0001-\u007F]';
CR = '\u000D';
CRLF = CR LF;
CTL = #'[\u0000-\u001F|\u007F]';
DIGIT = #'[0-9]';
DQUOTE = '\u0022';
HEXDIG = DIGIT | 'A' | 'B' | 'C' | 'D' | 'E' | 'F';
HTAB = '\u0009';
LF = '\u000A';
LWSP = (WSP | CRLF WSP)*;
OCTET = #'[\u0000-\u00FF]';
SP = '\u0020';
VCHAR = #'[\u0021-\u007E]';
WSP = SP | HTAB;
")

(def abnf
  "
rulelist = (rule | <(c-wsp* c-nl)>)+
rule = rulename-left <defined-as> elements <c-nl>
   (* continues if next line starts with white space *)
rulename-left = rulename
rulename-right = rulename
<rulename> = ALPHA (ALPHA | DIGIT | '-')*
defined-as = c-wsp* ('=' | '=/') c-wsp*
   (* basic rules definition and incremental alternatives *)
<elements> = alternation <c-wsp*>
c-wsp = WSP | (c-nl WSP)
c-nl = comment | CRLF
   (* comment or newline *)
comment = ';' (WSP | VCHAR)* CRLF
alternation = concatenation (<c-wsp* '/' c-wsp*> concatenation)*
concatenation = repetition (<c-wsp+> repetition)*
repetition = [repeat] element
repeat = NUM | (NUM? '*' NUM?)
<element> = rulename-right | group | option | char-val | num-val | <prose-val>
<group> = <'(' c-wsp*> alternation <c-wsp* ')'>
option = <'[' c-wsp*> alternation <c-wsp* ']'>
char-val = <DQUOTE> (SP | VCHAR)* <DQUOTE>
<num-val> = <'%'> (bin-val | dec-val | hex-val)
bin-val = <'b'> bin-char
          [ (<'.'> bin-char)+ | ('-' bin-char) ]
          (* series of contatenated bit values or single ONEOF range *)
bin-char = BIT+
dec-val = <'d'> dec-char
          [ (<'.'> dec-char)+ | ('-' dec-char) ]
dec-char = DIGIT+
hex-val = <'x'> hex-char
          [ (<'.'> hex-char)+ | ('-' hex-char) ]
hex-char = HEXDIG+
prose-val = <'<'> (SP | VCHAR)* <'>'>
          (* bracketed string of SP and VCHAR without angles
             prose description, to be used as last resort *)
NUM = DIGIT+
<ALPHA> = #'[a-zA-Z]'
<BIT> = '0' | '1'
<CHAR> = #'[\u0001-\u007F]'
    (* any 7-bit US-ASCII character, excluding NUL *)
<CR> = '\u000D'
    (* carriage return *)
<CRLF> = CR? LF
    (* newline (modified for instaparse) *)
<CTL> = #'[\u0000-\u001F\u007F]'
<DIGIT> = #'[0-9]'
<DQUOTE> = \"\\\"\"
<HEXDIG> = DIGIT | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
<HTAB> = '\u0009'
<LF> = '\u000A'
<SP> = '\u0020'
<VCHAR> = #'[\u0021-\u007E]'
   (* visible (printing) characters *)
<WSP> = SP | HTAB
   (* white space *)
")

(def abnf-parser (parser abnf))

(defn char-range
  "Takes two chars and returns a combinator representing a range of characters."
  [char1 char2]
  (regexp (str "[" char1 "-" char2 "]")))

(defn get-char-combinator
  ([num1]
    (string (str (char num1))))
  ([num1 num2 & nums]
    (let [v (concat [num1 num2] nums)]
      (if (= (v 1) "-")
        (char-range (char (v 0))
                    (char (v 2)))
        (apply alt (for [n v]
                     (string (str (char n)))))))))

(def abnf-transformer
  {:rulelist (fn [& rules]
               (-> (apply merge-with alt rules)
                 (into (ebnf abnf-basic))
                 (parser :start (key (first (first rules))))))
   :rule hash-map
   :rulename-left #(keyword (clojure.string/upper-case (apply str %&)))
   :rulename-right #(nt (keyword (clojure.string/upper-case (apply str %&))))
   ; since rulenames are case insensitive, convert it to upper case internally to be consistent
   :alternation alt
   :concatenation cat
   :repeat (fn ([num1 _ num2] {:low num1, :high num2})
             ([item1 item2] (if (= item1 "*")
                              {:high item2}
                              {:low item1}))
             ([_] {}))
                 
   :repetition (fn 
                 ([repeat element]
                   (cond
                     (empty? repeat) (star element)
                     (= (count repeat) 2) (rep (:low repeat) (:high repeat) element)
                     (= (:low repeat) 1) (plus element)
                     (= (:high repeat) 1) (opt element)
                     :else (rep (or (:low repeat) 0)
                                (or (:high repeat) Double/POSITIVE_INFINITY))))
                 ([element]
                   element))
   :option opt
   :char-val (fn [& cs]
               ; case insensitive string
               (regexp (apply str
                              (for [c cs]
                                (str "[" (clojure.string/upper-case c)
                                     "|" (clojure.string/lower-case c)
                                     "]")))))
   :bin-char (fn [& cs]
               (Integer/parseInt (apply str cs) 2))
   :dec-char (fn [& cs]
               (Integer/parseInt (apply str cs)))
   :hex-char (fn [& cs]
               (Integer/parseInt (apply str cs) 16))
   :bin-val get-char-combinator
   :dec-val get-char-combinator
   :hex-val get-char-combinator
   :NUM #(Integer/parseInt (apply str %&))})

(defn make-abnf-parser
  [s]
  (transform abnf-transformer (parse abnf-parser s)))
(ns instaparse.abnf
  "This is the context free grammar that recognizes ABNF notation."
  (:use instaparse.core)
  (:use instaparse.combinators))

(def abnf-core-ebnf
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

(def abnf-core-abnf
  "ALPHA          =  %x41-5A / %x61-7A   ; A-Z / a-z
BIT            =  \"0\" / \"1\"
CHAR           =  %x01-7F
                                ; any 7-bit US-ASCII character,
                                ;  excluding NUL
CR             =  %x0D
                                ; carriage return
CRLF           =  CR LF
                                ; Internet standard newline
CTL            =  %x00-1F / %x7F
                                ; controls
DIGIT          =  %x30-39
                                ; 0-9
DQUOTE         =  %x22
                                ; \" (Double Quote)
HEXDIG         =  DIGIT / \"A\" / \"B\" / \"C\" / \"D\" / \"E\" / \"F\"
HTAB           =  %x09
                                ; horizontal tab
LF             =  %x0A
                                ; linefeed
LWSP           =  *(WSP / CRLF WSP)
                                ; Use of this linear-white-space rule
                                ;  permits lines containing only white
                                ;  space that are no longer legal in
                                ;  mail headers and have caused
                                ;  interoperability problems in other
                                ;  contexts.
                                ; Do not use when defining mail
                                ;  headers and use with caution in
                                ;  other contexts.
OCTET          =  %x00-FF
                                ; 8 bits of data
SP             =  %x20
VCHAR          =  %x21-7E
                                ; visible (printing) characters
WSP            =  SP / HTAB
                                ; white space
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
char-val = <DQUOTE> #'[\u0020-\u0021\u0023-\u007E]'* <DQUOTE>
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
prose-val = <'<'> #'[\u0020-\u003D\u003F-\u007E]'* <'>'>
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

(defn re-escape
  [s]
  (let [bad-chars (set "[]{}-!@#$%^&*()")]
    (apply str
           (for [c s]
             (if (bad-chars c)
               (str "\\" c)
               c)))))

(def abnf-transformer
  {:rulelist (fn [& rules]
               (-> (apply merge-with alt rules)
                 ;(into (ebnf abnf-basic))
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
                                (let [low (clojure.string/lower-case c)
                                      high (clojure.string/upper-case c)]
                                  (if (= low high)
                                    (re-escape c)
                                    (str "["
                                         (re-escape low)
                                         "|"
                                         (re-escape high)
                                         "]")))))))
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

(def abnf-parser (parser abnf ;:transform abnf-transformer
                         ))


(defn make-abnf-parser
  [s]
  (->>
    (str s abnf-core-abnf)
    (parse abnf-parser)
    (transform abnf-transformer)))
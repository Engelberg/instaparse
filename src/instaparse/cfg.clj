(ns instaparse.cfg
  (:use instaparse.gll))

;syntax     ::=  { rule }
;rule       ::=  identifier  "::="  expression
;expression ::=  term { "|" term }
;term       ::=  factor { factor }
;factor     ::=  identifier |
;                quoted_symbol |
;                "("  expression  ")" |
;                "["  expression  "]" |
;                "{"  expression  "}"
;identifier ::=  letter { letter | digit }
;quoted_symbol ::= """ { any_character } """

(def single-quoted-string "'(\\'|[^\\'])*'")
(def single-quoted-regexp "#'(\\'|[^\\'])*'")

(def cfg {:rules (cat (nt :opt-whitespace)
                      (plus (nt :rule)))
          :whitespace (regexp "\\s+")
          :opt-whitespace (regexp "\\s*")
          :rule-separator (alt (string ":")
                               (string ":=")
                               (string "::=")
                               (string "=")
                               (string "->")
                               (string "=>"))
          :rule (cat (nt :nt) 
                     (nt :whitespace)
                     (nt :rule-separator)
                     (nt :whitespace)
                     (nt :alt)
                     (alt (nt :opt-whitespace)
                          (regexp "\\s*;\\s*")))                                                               
          :nt (regexp "[^ \\r\\t\\n(){}\\[\\]]+")
          :alt (cat (nt :cat)                           
                    (star
                      (cat
                        (nt :opt-whitespace)
                        (string "|")
                        (nt :opt-whitespace)
                        (nt :cat))))
          :paren-alt (cat (string "(")
                          (nt :opt-whitespace)
                          (nt :alt)
                          (nt :opt-whitespace)
                          (string ")"))
          :cat (plus (cat
                       (nt :opt-whitespace)
                       (nt :factor)
                       (nt :opt-whitespace)))
          :string (regexp single-quoted-string)
          :regexp (regexp single-quoted-regexp)
          :factor (alt (nt :nt)
                       (nt :string)
                       (nt :regexp)                       
                       (nt :paren-alt))})

(def cfg1 "S -> 'a'")
(def cfg2 
  "S -> X
   X -> Y
   Y -> Z")
(def cfg3
  "S -> X | Y
   Y -> A Z
   Z := 'a'")
(def cfg4
  "S -> A B | C
   C -> (A | B) C")

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

(def cfg {:rules (cat (hide (nt :opt-whitespace))
                      (plus (nt :rule)))
          :whitespace (regexp "\\s+")
          :opt-whitespace (regexp "\\s*")
          :rule-separator (alt (string ":")
                               (string ":=")
                               (string "::=")
                               (string "="))
          :rule (cat (nt :nt) 
                     (hide (nt :opt-whitespace))
                     (hide (nt :rule-separator))
                     (hide (nt :opt-whitespace))
                     (nt :alt)
                     (hide (alt (nt :opt-whitespace)
                                (regexp "\\s*;\\s*"))))          
          :nt (regexp "[^ \\r\\t\\n(){}\\[\\]+*?:=|'#]+")
          :alt (cat (nt :cat)                           
                    (star
                      (cat
                        (hide (nt :opt-whitespace))
                        (hide (string "|"))
                        (hide (nt :opt-whitespace))
                        (nt :cat))))
          :paren-alt (cat (hide (string "("))
                          (hide (nt :opt-whitespace))
                          (nt :alt)
                          (hide (nt :opt-whitespace))
                          (hide (string ")")))
          :cat (plus (cat
                       (hide (nt :opt-whitespace))
                       (nt :factor)
                       (hide (nt :opt-whitespace))))
          :string (regexp single-quoted-string)
          :regexp (regexp single-quoted-regexp)
          :factor (alt (nt :nt)
                       (nt :string)
                       (nt :regexp)                       
                       (nt :paren-alt))})

(def cfg1 "S = 'a'")
(def cfg2 
  "S = X
   X = Y
   Y = Z")
(def cfg3
  "S = X | Y
   Y = A Z
   Z = 'a'")
(def cfg4
  "S := A B | C
   C := (A | B) C")

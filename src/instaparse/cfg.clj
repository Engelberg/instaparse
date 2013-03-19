(ns instaparse.cfg
  (:use instaparse.gll)
  (:use clojure.pprint clojure.repl))
;
;(def single-quoted-string "'(\\'|[^'])*'")
;(def single-quoted-string "'([^']|(?<!\\\\)')*?'") 
;(def single-quoted-string #"'[^\\]*?'")
;(def single-quoted-string #"'([^\\']|\\\\|\\')*'")
;(def single-quoted-string #"'([^']|\\')*'")
(def single-quoted-string #"'(?:[^\\']|\\.)*'")
(def single-quoted-regexp #"#'(?:[^\\']|\\.)*'")
(def double-quoted-string #"\"(?:[^\\\"]|\\.)*\"")
(def double-quoted-regexp #"#\"(?:[^\\\"]|\\.)*\"")

(def opt-whitespace (hide (nt :opt-whitespace)))

(def cfg {:rules (cat opt-whitespace
                      (plus (nt :rule)))
          :whitespace (regexp "[,\\s]+")
          :opt-whitespace (regexp "[,\\s]*")
          :rule-separator (alt (string ":")
                               (string ":=")
                               (string "::=")
                               (string "="))
          :rule (cat (alt (nt :nt)
                          (nt :hide-nt))
                     opt-whitespace
                     (hide (nt :rule-separator))
                     opt-whitespace
                     (nt :alt)
                     (hide (alt (nt :opt-whitespace)
                                (regexp "\\s*[.;]\\s*"))))          
          :nt (cat
                (neg (nt :epsilon))
                (regexp "[^, \\r\\t\\n<>(){}\\[\\]+*?:=|'\"#&!;.]+"))
          :hide-nt (cat (hide (string "<"))
                        opt-whitespace
                        (nt :nt)
                        opt-whitespace
                        (hide (string ">")))                        
          :alt (cat (nt :cat)                           
                    (star
                      (cat
                        opt-whitespace
                        (hide (string "|"))
                        opt-whitespace
                        (nt :cat))))
          :paren (cat (hide (string "("))
                      opt-whitespace
                      (nt :alt)
                      opt-whitespace
                      (hide (string ")")))
          :hide (cat (hide (string "<"))
                     opt-whitespace	
                     (nt :alt)
                     opt-whitespace
                     (hide (string ">")))
          :cat (plus (cat
                       opt-whitespace
                       (alt (nt :factor) (nt :look) (nt :neg))
                       opt-whitespace))
          :string (alt
                    (regexp single-quoted-string)
                    (regexp double-quoted-string))
          :regexp (alt
                    (regexp single-quoted-regexp)
                    (regexp double-quoted-regexp))
          :opt (alt
                 (cat (hide (string "["))
                      opt-whitespace
                      (nt :alt)
                      opt-whitespace
                      (hide (string "]")))
                 (cat (nt :factor)
                      opt-whitespace
                      (hide (string "?"))))
          :star (alt
                  (cat (hide (string "{"))
                       opt-whitespace
                       (nt :alt)
                       opt-whitespace
                       (hide (string "}")))
                  (cat (nt :factor)
                       opt-whitespace
                       (hide (string "*"))))
          :plus (cat (nt :factor)
                     opt-whitespace
                     (hide (string "+")))
          :look (cat (hide (string "&"))
                     opt-whitespace
                     (nt :factor))
          :neg (cat (hide (string "!"))
                    opt-whitespace
                    (nt :factor))
          :epsilon (alt (string "Epsilon")
                        (string "epsilon")
                        (string "EPSILON")
                        (string "\u03b5"))
          :factor (hide-tag (alt (nt :nt)
                                 (nt :string)
                                 (nt :regexp)
                                 (nt :opt)     
                                 (nt :star)
                                 (nt :plus)
                                 (nt :paren)
                                 (nt :hide)
                                 (nt :epsilon)))})

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
(def cfg5
  "S=A?")
(def cfg6
  "S =(A | B)?")
(def cfg7
  "S = A, B?, (C C)*, D+, E")
(def cfg8
  "<S> = <A B?> (C | D)")
(def cfg9
  "S = A, &B")
(def cfg10
  "S = &B A")
(def cfg11
  "S = &B+ A")
(def cfg12
  "S = !B A")
(def cfg13
  "S = !&B A")
(def cfg15
  "S = 'a' S | Epsilon;
   C = 'b'.
   D = A")

(ns instaparse.core-test
  (:use clojure.test)
  (:require clojure.edn)
  (:require [instaparse.core :as insta])
  (:use instaparse.combinators))

(def as-and-bs
  (insta/parser
    "S = AB*
     AB = A B
     A = 'a'+
     B = 'b'+"))

(def as-and-bs-alternative
  (insta/parser
    "S:={AB}  ;
     AB ::= (A, B)
     A : \"a\" + ;
     B ='b' + ;"))


(def as-and-bs-enlive
  (insta/parser
    "S = AB*
     AB = A B
     A = 'a'+
     B = 'b'+"    
    :output-format :enlive))

(def as-and-bs-variation1
  (insta/parser
    "S = AB*
     AB = 'a'+ 'b'+"))

(def as-and-bs-variation2
  (insta/parser
    "S = ('a'+ 'b'+)*"))

(def paren-ab
  (insta/parser
    "paren-wrapped = '(' seq-of-A-or-B ')'
     seq-of-A-or-B = ('a' | 'b')*"))

(def paren-ab-hide-parens
  (insta/parser
    "paren-wrapped = <'('> seq-of-A-or-B <')'>
     seq-of-A-or-B = ('a' | 'b')*"))

(def paren-ab-manually-flattened
  (insta/parser
    "paren-wrapped = <'('> ('a' | 'b')* <')'>"))
  
(def paren-ab-hide-tag
  (insta/parser
    "paren-wrapped = <'('> seq-of-A-or-B <')'>
     <seq-of-A-or-B> = ('a' | 'b')*"))

(def paren-ab-hide-both-tags
  (insta/parser
    "<paren-wrapped> = <'('> seq-of-A-or-B <')'>
     <seq-of-A-or-B> = ('a' | 'b')*"))

(def addition
  (insta/parser
    "plus = plus <'+'> plus | num
     num = #'[0-9]'+"))

(def addition2
  (insta/parser
    "plus = num <'+'> plus | num
     num = #'[0-9]'+"))

(def addition3
  (insta/parser
    "plus = plus <'+'> num | num
     num = #'[0-9]'+"))

(def addition-e
  (insta/parser
    "plus = plus <'+'> plus | num
     num = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'"
    :output-format :enlive))

(def words-and-numbers
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = word | number
     whitespace = #'\\s+'
     word = #'[a-zA-Z]+'
     number = #'[0-9]+'"))

(def words-and-numbers-one-character-at-a-time
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = word | number
     whitespace = #'\\s+'
     word = letter+
     number = digit+ 
     <letter> = #'[a-zA-Z]'
     <digit> = #'[0-9]'"))

(def words-and-numbers-enlive
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = word | number
     whitespace = #'\\s+'
     word = letter+
     number = digit+ 
     <letter> = #'[a-zA-Z]'
     <digit> = #'[0-9]'"
    :output-format :enlive))

(insta/transform 
  {:word str, 
   :number (comp clojure.edn/read-string str)}
  (words-and-numbers-one-character-at-a-time "abc 123 def"))

(def ambiguous
  (insta/parser
    "S = A A
     A = 'a'*"))

(def not-ambiguous
  (insta/parser
    "S = A A
     A = #'a*'"))

(def repeated-a
  (insta/parser
    "S = 'a'+"))

(def lookahead-example
  (insta/parser
    "S = &'ab' ('a' | 'b')+"))

(def negative-lookahead-example
  (insta/parser
    "S = !'ab' ('a' | 'b')+"))

(def abc
  (insta/parser
    "S = &(A 'c') 'a'+ B
     A = 'a' A? 'b'
     <B> = 'b' B? 'c'"))

(def abc-grammar-map
  {:S (cat (look (cat (nt :A) (string "c")))
           (plus (string "a"))
           (nt :B))
   :A (cat (string "a") (opt (nt :A)) (string "b"))
   :B (hide-tag (cat (string "b") (opt (nt :B)) (string "c")))})

(def ambiguous-tokenizer
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = keyword | identifier
     whitespace = #'\\s+'
     identifier = #'[a-zA-Z]+'
     keyword = 'cond' | 'defn'"))

(def unambiguous-tokenizer
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = keyword | !keyword identifier
     whitespace = #'\\s+'
     identifier = #'[a-zA-Z]+'
     keyword = 'cond' | 'defn'"))

(def preferential-tokenizer
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = keyword / identifier
     whitespace = #'\\s+'
     identifier = #'[a-zA-Z]+'
     keyword = 'cond' | 'defn'"))

(def ord-test
  (insta/parser
    "S = Even / Odd
     Even = 'aa'*
     Odd = 'a'+"))

(def ord2-test
  (insta/parser
    "S = token (<ws> token)*
     ws = #'\\s+'
     keyword = 'hello' | 'bye'
     identifier = #'\\S+'
     token = keyword / identifier
     "))

(def even-odd
  (insta/parser
    "S = Even | Odd
     eos = !#'.'
     Even = 'aa'*
     Odd = !(Even eos) 'a'+"))

(def arithmetic
  (insta/parser
    "expr = add-sub
     <add-sub> = mul-div | add | sub
     add = add-sub <'+'> mul-div
     sub = add-sub <'-'> mul-div
     <mul-div> = term | mul | div
     mul = mul-div <'*'> term
     div = mul-div <'/'> term     
     <term> = number | <'('> add-sub <')'>
     number = #'[0-9]+'"))

(deftest parsing-tutorial
  (are [x y] (= x y)
    (as-and-bs "aaaaabbbaaaabb")
    [:S
     [:AB [:A "a" "a" "a" "a" "a"] [:B "b" "b" "b"]]
     [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]
    
    (as-and-bs-enlive "aaaaabbbaaaabb")
    '{:tag :S,
     :content
     ({:tag :AB,
       :content
       ({:tag :A, :content ("a" "a" "a" "a" "a")}
         {:tag :B, :content ("b" "b" "b")})}
       {:tag :AB,
      :content
      ({:tag :A, :content ("a" "a" "a" "a")}
        {:tag :B, :content ("b" "b")})})}
    
    (as-and-bs-variation1 "aaaaabbbaaaabb")
    [:S
     [:AB "a" "a" "a" "a" "a" "b" "b" "b"]
     [:AB "a" "a" "a" "a" "b" "b"]]
    
    (as-and-bs-variation2 "aaaaabbbaaaabb")
    [:S "a" "a" "a" "a" "a" "b" "b" "b" "a" "a" "a" "a" "b" "b"]
    
    (paren-ab "(aba)")
    [:paren-wrapped "(" [:seq-of-A-or-B "a" "b" "a"] ")"]
    
    (paren-ab-hide-parens "(aba)")
    [:paren-wrapped [:seq-of-A-or-B "a" "b" "a"]]
    
    (paren-ab-manually-flattened "(aba)")
    [:paren-wrapped "a" "b" "a"]
    
    (paren-ab-hide-tag "(aba)")
    [:paren-wrapped "a" "b" "a"]
    
    (insta/transform
      {:num read-string
       :plus +}
      (addition "1+2+3+4+5"))
    15
    
    (insta/transform
      {:num read-string
       :plus +}
      (insta/parses addition "1+2+3+4+5"))
    (repeat 14 15)

    (insta/transform
      {:num read-string
      :plus +}
      (addition-e "1+2+3+4+5"))
    15    
    
    ((insta/parser "S = 'a' S | '' ") "aaaa")
    [:S "a" [:S "a" [:S "a" [:S "a" [:S]]]]]
    
    ((insta/parser "S = S 'a' | Epsilon") "aaaa")
    [:S [:S [:S [:S [:S] "a"] "a"] "a"] "a"]
    
    (set (insta/parses ambiguous "aaaaaa"))
    (set '([:S [:A "a"] [:A "a" "a" "a" "a" "a"]]
           [:S [:A "a" "a" "a" "a" "a" "a"] [:A]]
           [:S [:A "a" "a"] [:A "a" "a" "a" "a"]]
           [:S [:A "a" "a" "a"] [:A "a" "a" "a"]]
           [:S [:A "a" "a" "a" "a"] [:A "a" "a"]]
           [:S [:A "a" "a" "a" "a" "a"] [:A "a"]]
           [:S [:A] [:A "a" "a" "a" "a" "a" "a"]]))
    
    (insta/parses not-ambiguous "aaaaaa")
    '([:S [:A "aaaaaa"] [:A ""]])
    
    (lookahead-example "abaaaab")
    [:S "a" "b" "a" "a" "a" "a" "b"]
    
    (insta/failure? (lookahead-example "bbaaaab"))
    true
    
    (insta/failure? (negative-lookahead-example "abaaaab"))
    true
    
    (negative-lookahead-example "bbaaaab")
    [:S "b" "b" "a" "a" "a" "a" "b"]

    (insta/parses ambiguous-tokenizer "defn my cond")
    '([:sentence
      [:identifier "defn"]
      [:identifier "my"]
      [:identifier "cond"]]
      [:sentence [:keyword "defn"] [:identifier "my"] [:identifier "cond"]]
      [:sentence [:identifier "defn"] [:identifier "my"] [:keyword "cond"]]
      [:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]])
    
    (insta/parses unambiguous-tokenizer "defn my cond")
    '([:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]])
    
    (insta/parses preferential-tokenizer "defn my cond")
    '([:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]] [:sentence [:identifier "defn"] [:identifier "my"] [:keyword "cond"]] [:sentence [:keyword "defn"] [:identifier "my"] [:identifier "cond"]] [:sentence [:identifier "defn"] [:identifier "my"] [:identifier "cond"]])
    
    (insta/parses repeated-a "aaaaaa")
    '([:S "a" "a" "a" "a" "a" "a"])

    (insta/parse repeated-a "aaaaaa" :partial true)
    [:S "a"]
    
    (insta/parses repeated-a "aaaaaa" :partial true)
    '([:S "a"] [:S "a" "a"] [:S "a" "a" "a"] [:S "a" "a" "a" "a"] [:S "a" "a" "a" "a" "a"] [:S "a" "a" "a" "a" "a" "a"])

    (words-and-numbers-one-character-at-a-time "abc 123 def")
    [:sentence [:word "a" "b" "c"] [:number "1" "2" "3"] [:word "d" "e" "f"]]

    (insta/transform 
     {:word str, 
      :number (comp clojure.edn/read-string str)}
     (words-and-numbers-one-character-at-a-time "abc 123 def"))
    [:sentence "abc" 123 "def"]
    
    (->> (words-and-numbers-enlive "abc 123 def")
      (insta/transform
        {:word str,
         :number (comp clojure.edn/read-string str)}))
    {:tag :sentence, :content ["abc" 123 "def"]}
    
    (arithmetic "1-2/(3-4)+5*6")
    [:expr
     [:add
      [:sub
       [:number "1"]
       [:div [:number "2"] [:sub [:number "3"] [:number "4"]]]]
      [:mul [:number "5"] [:number "6"]]]]
    
    (->> (arithmetic "1-2/(3-4)+5*6")
     (insta/transform
       {:add +, :sub -, :mul *, :div /, 
        :number clojure.edn/read-string :expr identity}))
    33
    
    (paren-ab-hide-both-tags "(aba)")
    '("a" "b" "a")
    
    ))
    
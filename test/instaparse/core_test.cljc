(ns instaparse.core-test
  #?(:clj (:refer-clojure :exclude [cat read-string]))
  (:require
    #?(:clj  [clojure.test :refer [deftest are is]]
       :cljs [cljs.test :as t])
    #?(:clj  [clojure.edn :refer [read-string]]
       :cljs [cljs.reader :refer [read-string]])
    [instaparse.core :as insta]
    [instaparse.cfg :refer [ebnf]]
    [instaparse.line-col :as lc]
    [instaparse.combinators-source :refer [Epsilon opt plus star rep 
                                           alt ord cat string-ci string
                                           string-ci regexp nt look neg 
                                           hide hide-tag]])
  #?(:cljs (:require-macros
             [cljs.test :refer [is are deftest run-tests]])))

(def as-and-bs
  (insta/parser
    "S = AB*
     AB = A B
     A = 'a'+
     B = 'b'+"))

(def as-and-bs-regex
  (insta/parser
    "S = AB*
     AB = A B
     A = #'a'+
     B = #'b'+"))

(def long-string
  (apply str (concat (repeat 20000 \a) (repeat 20000 \b))))

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
   :number (comp read-string str)}
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

(def unambiguous-tokenizer-improved
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = keyword | !keyword identifier
     whitespace = #'\\s+'
     end-of-string = !#'[\\s\\S]'
     identifier = #'[a-zA-Z]+'
     keyword = ('cond' | 'defn') &(whitespace | end-of-string)"))

(def unambiguous-tokenizer-improved2
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = keyword | !(keyword (whitespace | end-of-string)) identifier
     whitespace = #'\\s+'
     end-of-string = !#'[\\s\\S]'
     identifier = #'[a-zA-Z]+'
     keyword = 'cond' | 'defn'"))

(def unambiguous-tokenizer-improved3
  (insta/parser
    "sentence = token (<whitespace> token)*
     <token> = keyword | !keyword identifier
     whitespace = #'\\s+'
     identifier = #'[a-zA-Z]+'
     keyword = #'cond\\b' | #'defn\\b'"))

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

(def combo-build-example
  (insta/parser
    (merge
      {:S (alt (nt :A) (nt :B))}
      (ebnf "A = 'a'*")
      {:B (ebnf "'b'+")})
    :start :S))

(def tricky-ebnf-build
  "https://github.com/Engelberg/instaparse/issues/107"
  (insta/parser
    (merge
      {:S (alt (nt :A) (nt :B))}
      (ebnf "<A> = '='*")
      {:B (ebnf "'b' '='")})
    :start :S))

(defn spans [t]
  (if (sequential? t)
    (cons (insta/span t) (map spans (next t)))
    t))      

(defn spans-hiccup-tag [t]
  (if (sequential? t)
    (cons {:tag (first t) :span (insta/span t)} (map spans (next t)))
    t))      

(defn spans-enlive [t]
  (if (map? t)
    (assoc t :span (insta/span t) :content (map spans-enlive (:content t)))
    t))

(def whitespace 
  (insta/parser
    "whitespace = #'\\s+'"))

(def auto-whitespace-example
  (insta/parser 
    "S = A B 
     <A> = 'foo' 
     <B> = #'\\d+'" 
    :auto-whitespace whitespace))

(def words-and-numbers-auto-whitespace
  (insta/parser
    "sentence = token+
     <token> = word | number
     word = #'[a-zA-Z]+'
     number = #'[0-9]+'"
    :auto-whitespace whitespace))

(def auto-whitespace-example2
  (insta/parser 
    "S = A B 
     <A> = 'foo' 
     <B> = #'\\d+'" 
    :auto-whitespace :standard))

(def words-and-numbers-auto-whitespace2
  (insta/parser
    "sentence = token+
     <token> = word | number
     word = #'[a-zA-Z]+'
     number = #'[0-9]+'"
    :auto-whitespace :standard))

(def whitespace-or-comments-v1
  (insta/parser
    "ws-or-comment = #'\\s+' | comment
	   comment = '(*' inside-comment* '*)'
	   inside-comment =  ( !('*)' | '(*') #'.' ) | comment"))

(def whitespace-or-comments-v2
  (insta/parser
    "ws-or-comments = #'\\s+' | comments
     comments = comment+
     comment = '(*' inside-comment* '*)'
     inside-comment =  !( '*)' | '(*' ) #'.' | comment"))

(def whitespace-or-comments
  (insta/parser
    "ws-or-comments = #'\\s+' | comments
     comments = comment+
     comment = '(*' inside-comment* '*)'
     inside-comment =  !'*)' !'(*' #'.' | comment"
    :auto-whitespace whitespace))

(def words-and-numbers-auto-whitespace-and-comments
  (insta/parser
    "sentence = token+
     <token> = word | number
     word = #'[a-zA-Z]+'
     number = #'[0-9]+'"
    :auto-whitespace whitespace-or-comments))

(def eat-a (insta/parser "Aeater = #'[a]'+" :output-format :enlive))

(def int-or-double
  (insta/parser
    "ws = #'\\s+';
     Int = #'[0-9]+';
     Double = #'[0-9]+\\.[0-9]*|\\.[0-9]+';
     <ConstExpr> = Int | Double;
     Input = ConstExpr <ws> ConstExpr;"
     :start :Input))

(deftest parsing-tutorial
  (are [x y] (= x y)
    (as-and-bs "aaaaabbbaaaabb")
    [:S
     [:AB [:A "a" "a" "a" "a" "a"] [:B "b" "b" "b"]]
     [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]
    
#?@(:clj [(as-and-bs (StringBuilder. "aaaaabbbaaaabb"))
          [:S
           [:AB [:A "a" "a" "a" "a" "a"] [:B "b" "b" "b"]]
           [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]])
    
    (as-and-bs "aaaaabbbaaaabb")
    (as-and-bs "aaaaabbbaaaabb" :optimize :memory)
    
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
    
    (as-and-bs-enlive "aaaaabbbaaaabb")
    (as-and-bs-enlive "aaaaabbbaaaabb" :optimize :memory)
    
    (as-and-bs-variation1 "aaaaabbbaaaabb")
    [:S
     [:AB "a" "a" "a" "a" "a" "b" "b" "b"]
     [:AB "a" "a" "a" "a" "b" "b"]]
    
    (as-and-bs-variation1 "aaaaabbbaaaabb")
    (as-and-bs-variation1 "aaaaabbbaaaabb" :optimize :memory)
    
    (as-and-bs-variation2 "aaaaabbbaaaabb")
    [:S "a" "a" "a" "a" "a" "b" "b" "b" "a" "a" "a" "a" "b" "b"]
    
    (as-and-bs-variation2 "aaaaabbbaaaabb")
    (as-and-bs-variation2 "aaaaabbbaaaabb" :optimize :memory)
    
    (paren-ab "(aba)")
    [:paren-wrapped "(" [:seq-of-A-or-B "a" "b" "a"] ")"]
    
    (paren-ab "(aba)")
    (paren-ab "(aba)" :optimize :memory)        
    
    (paren-ab-hide-parens "(aba)")
    [:paren-wrapped [:seq-of-A-or-B "a" "b" "a"]]
    
    (paren-ab-hide-parens "(aba)")
    (paren-ab-hide-parens "(aba)" :optimize :memory)
    
    (paren-ab-manually-flattened "(aba)")
    [:paren-wrapped "a" "b" "a"]
    
    (paren-ab-manually-flattened "(aba)")
    (paren-ab-manually-flattened "(aba)" :optimize :memory)
    
    (paren-ab-hide-tag "(aba)")
    [:paren-wrapped "a" "b" "a"]
    
    (paren-ab-hide-tag "(aba)")
    (paren-ab-hide-tag "(aba)" :optimize :memory)
    
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
    
    ((insta/parser "S = 'a' S | '' ") "aaaa")
    ((insta/parser "S = 'a' S | '' ") "aaaa" :optimize :memory)
    
    ((insta/parser "S = S 'a' | Epsilon") "aaaa")
    [:S [:S [:S [:S [:S] "a"] "a"] "a"] "a"]
    
    ((insta/parser "S = S 'a' | Epsilon") "aaaa")
    ((insta/parser "S = S 'a' | Epsilon") "aaaa" :optimize :memory)
    
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
    
    (lookahead-example "abaaaab")
    (lookahead-example "abaaaab" :optimize :memory)       
    
    (insta/failure? (lookahead-example "bbaaaab"))
    true
    
    (lookahead-example "bbaaaab")
    (lookahead-example "bbaaaab" :optimize :memory)
    
    (insta/failure? (negative-lookahead-example "abaaaab"))
    true
    
    (insta/parses 
      (insta/parser 
         "Regex = (CharNonRange | Range) +
          Range = Char <'-'> Char
          CharNonRange = Char ! ('-' Char)
          Char = #'[-x]' | 'c' (! 'd') 'x'")
      "x-cx")
    '([:Regex [:Range [:Char "x"] [:Char "c" "x"]]])
    
    
    (negative-lookahead-example "abaaaab")
    (negative-lookahead-example "abaaaab" :optimize :memory)
    
    (negative-lookahead-example "bbaaaab")
    [:S "b" "b" "a" "a" "a" "a" "b"]
    
    (negative-lookahead-example "bbaaaab")
    (negative-lookahead-example "bbaaaab" :optimize :memory)

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
    
    (words-and-numbers-one-character-at-a-time "abc 123 def")
    (words-and-numbers-one-character-at-a-time "abc 123 def" :optimize :memory)   

    (insta/transform 
     {:word str, 
      :number (comp read-string str)}
     (words-and-numbers-one-character-at-a-time "abc 123 def"))
    [:sentence "abc" 123 "def"]
    
    (->> (words-and-numbers-enlive "abc 123 def")
      (insta/transform
        {:word str,
         :number (comp read-string str)}))
    {:tag :sentence, :content ["abc" 123 "def"]}
    
    (arithmetic "1-2/(3-4)+5*6")
    [:expr
     [:add
      [:sub
       [:number "1"]
       [:div [:number "2"] [:sub [:number "3"] [:number "4"]]]]
      [:mul [:number "5"] [:number "6"]]]]
    
    (arithmetic "1-2/(3-4)+5*6")
    (arithmetic "1-2/(3-4)+5*6" :optimize :memory)        
    
    (->> (arithmetic "1-2/(3-4)+5*6")
     (insta/transform
       {:add +, :sub -, :mul *, :div /, 
        :number read-string :expr identity}))
    33
    
    (paren-ab-hide-both-tags "(aba)")
    '("a" "b" "a")
    
    (paren-ab-hide-both-tags "(aba)")
    (paren-ab-hide-both-tags "(aba)" :optimize :memory)    
    
    (combo-build-example "aaaaa")
    [:S [:A "a" "a" "a" "a" "a"]]
    
    (combo-build-example "aaaaa")
    (combo-build-example "aaaaa" :optimize :memory)    
    
    (combo-build-example "bbbbb")    
    [:S [:B "b" "b" "b" "b" "b"]]
    
    (combo-build-example "bbbbb")
    (combo-build-example "bbbbb" :optimize :memory)

    (tricky-ebnf-build "===")
    [:S "=" "=" "="]

    (tricky-ebnf-build "b=")
    [:S [:B "b" "="]]
    
    ((insta/parser "S = ('a'?)+") "")
    [:S]
    
    ((insta/parser "S = ('a'?)+") "")
    ((insta/parser "S = ('a'?)+") "" :optimize :memory)    
    
    ((insta/parser
     "a = b c .
      b = 'b' .
      c = 'c' .") "bc")
    [:a [:b "b"] [:c "c"]]
    
    (paren-ab-hide-parens "(ababa)" :unhide :content)
    [:paren-wrapped "(" [:seq-of-A-or-B "a" "b" "a" "b" "a"] ")"]
    
    (paren-ab-hide-parens "(ababa)" :unhide :all)
    [:paren-wrapped "(" [:seq-of-A-or-B "a" "b" "a" "b" "a"] ")"]
    
    (paren-ab-hide-tag "(ababa)" :unhide :tags)
    [:paren-wrapped [:seq-of-A-or-B "a" "b" "a" "b" "a"]]
    
    (paren-ab-hide-tag "(ababa)" :unhide :all)
    [:paren-wrapped "(" [:seq-of-A-or-B "a" "b" "a" "b" "a"] ")"]
    
    (insta/parses words-and-numbers "ab 123 cd" :unhide :all)
    '([:sentence [:token [:word "ab"]] [:whitespace " "] [:token [:number "123"]] [:whitespace " "] [:token [:word "cd"]]])
    
    ((insta/parser "S = epsilon") "") [:S]
    
    (words-and-numbers-auto-whitespace " abc 123   45 de ")
    [:sentence [:word "abc"] [:number "123"] [:number "45"] [:word "de"]]
    
    (words-and-numbers-auto-whitespace2 " abc 123   45 de ")
    [:sentence [:word "abc"] [:number "123"] [:number "45"] [:word "de"]]
    
    (words-and-numbers-auto-whitespace-and-comments " abc 123 (* 456 *) (* (* 7*) 89 *)  def ")
    [:sentence [:word "abc"] [:number "123"] [:word "def"]]
    
    (insta/parses eat-a "aaaaaaaabbbbbb" :total true)
    '({:tag :Aeater, :content ("a" "a" "a" "a" "a" "a" "a" "a" {:tag :instaparse/failure, :content ("bbbbbb")})})
    
    (int-or-double "31 0.2")
    [:Input [:Int "31"] [:Double "0.2"]]
    
    ((insta/parser "S=#'\\s*'") "     ")
    [:S "     "]
    
    ((insta/parser "S = #'a+'") "aaaaaa")
    [:S "aaaaaa"]
    
    ((insta/parser "S = 'a' / eps") "a") [:S "a"]
    ((insta/parser "S = 'a' / eps") "") [:S]

    (insta/failure? ((insta/parser "S = 'a'+") "AaaAaa"))
    true

    ((insta/parser "S = 'a'+" :string-ci true) "AaaAaa")
    [:S "a" "a" "a" "a" "a" "a"]
    
    ((insta/parser "S = %x30.31" :input-format :abnf) "01")
    [:S "0" "1"]
    
    (auto-whitespace-example "foo 123")
    [:S "foo" "123"]

    (auto-whitespace-example2 "foo 123")
    [:S "foo" "123"]    
    
    (insta/failure? ((insta/parser "f = #'asdf'" ) ""))
    true
    
    (insta/transform {:ADD +} [:ADD 10 5])
    15

    (->> "a"
         ((insta/parser "<S> = 'a'"))
         (insta/transform {}))
    '("a")
    ))    

#?(:clj (do ;; CLOJURE ONLY TESTS

(defn spans [t]
  (if (sequential? t)
    (cons (insta/span t) (map spans (next t)))
    t))      

(defn spans-hiccup-tag [t]
  (if (sequential? t)
    (cons {:tag (first t) :span (insta/span t)} (map spans (next t)))
    t))

(defn spans-enlive [t]
  (if (map? t)
    (assoc t :span (insta/span t) :content (map spans-enlive (:content t)))
    t))

(deftest span-tests
  (are [x y] (= x y)
    (spans (as-and-bs "aaaabbbaabbab"))
    '([0 13] ([0 7] ([0 4] "a" "a" "a" "a") ([4 7] "b" "b" "b")) ([7 11] ([7 9] "a" "a") ([9 11] "b" "b")) ([11 13] ([11 12] "a") ([12 13] "b")))

    (spans (as-and-bs "aaaabbbaabbab"))
    (spans (as-and-bs "aaaabbbaabbab" :optimize :memory))    
    
    (spans ((insta/parser "S = 'a' S | '' ") "aaaa"))
    '([0 4] "a" ([1 4] "a" ([2 4] "a" ([3 4] "a" ([4 4])))))
    
    (spans ((insta/parser "S = 'a' S | '' ") "aaaa"))
    (spans ((insta/parser "S = 'a' S | '' ") "aaaa" :optimize :memory))    
    
    (spans (as-and-bs "aaaaabbbaacabb" :total true))
    '([0 14] ([0 8] ([0 5] "a" "a" "a" "a" "a") ([5 8] "b" "b" "b")) ([8 14] ([8 10] "a" "a") ([10 14] ([10 14] "cabb"))))
    
    (spans (as-and-bs "aaaaabbbaacabb" :total true))
    (spans (as-and-bs "aaaaabbbaacabb" :total true :optimize :memory))
    
    (spans-enlive (as-and-bs-enlive "aaaaabbbaacabb" :total true))
    '{:span [0 14], :tag :S, :content ({:span [0 8], :tag :AB, :content ({:span [0 5], :tag :A, :content ("a" "a" "a" "a" "a")} {:span [5 8], :tag :B, :content ("b" "b" "b")})} {:span [8 14], :tag :AB, :content ({:span [8 10], :tag :A, :content ("a" "a")} {:span [10 14], :tag :B, :content ({:span [10 14], :tag :instaparse/failure, :content ("cabb")})})})}
        
    (spans-enlive (as-and-bs-enlive "aaaabbbaabbab"))
    '{:span [0 13], :tag :S, :content ({:span [0 7], :tag :AB, :content ({:span [0 4], :tag :A, :content ("a" "a" "a" "a")} {:span [4 7], :tag :B, :content ("b" "b" "b")})} {:span [7 11], :tag :AB, :content ({:span [7 9], :tag :A, :content ("a" "a")} {:span [9 11], :tag :B, :content ("b" "b")})} {:span [11 13], :tag :AB, :content ({:span [11 12], :tag :A, :content ("a")} {:span [12 13], :tag :B, :content ("b")})})}
    
    (spans-enlive (as-and-bs-enlive "aaaabbbaabbab"))
    (spans-enlive (as-and-bs-enlive "aaaabbbaabbab" :optimize :memory))
        
    (->> (words-and-numbers-enlive "abc 123 def")
      (insta/transform
        {:word (comp (partial array-map :word) str),
         :number (comp (partial array-map :number) read-string str)}))
    {:tag :sentence, :content [{:word "abc"} {:number 123} {:word "def"}]}
    
    (->> (words-and-numbers-enlive "abc 123 def")
      (insta/transform
        {:word (comp (partial array-map :word) str),
         :number (comp (partial array-map :number) read-string str)})
      spans-enlive)
    '{:span [0 11], :tag :sentence, :content ({:content (), :span [0 3], :word "abc"} {:content (), :span [4 7], :number 123} {:content (), :span [8 11], :word "def"})}))

)) ;; END CLOJURE ONLY TESTS

(defn round-trip [parser]
  (insta/parser (prn-str parser)))

(deftest round-trip-test
  (are [p] (= (prn-str p) (prn-str (round-trip p)))
       as-and-bs
       as-and-bs-regex
       as-and-bs-variation1
       as-and-bs-variation2
       paren-ab
       paren-ab-hide-parens
       paren-ab-manually-flattened
       paren-ab-hide-tag
       paren-ab-hide-both-tags
       addition
       addition-e
       words-and-numbers
       words-and-numbers-one-character-at-a-time
       ambiguous
       not-ambiguous
       repeated-a
       lookahead-example
       negative-lookahead-example
       abc
       ambiguous-tokenizer
       unambiguous-tokenizer
       preferential-tokenizer
       ord-test
       ord2-test
       even-odd
       arithmetic
       whitespace
       words-and-numbers-auto-whitespace
       whitespace-or-comments-v1
       whitespace-or-comments-v2
       whitespace-or-comments
       words-and-numbers-auto-whitespace
       eat-a
       int-or-double))

(defn hiccup-line-col-spans [t]
  (if (sequential? t)
    (cons (meta t) (map hiccup-line-col-spans (next t)))
    t))

(defn enlive-line-col-spans [t]
  (if (map? t)
    (cons (meta t) (map enlive-line-col-spans (:content t)))
    t))

(deftest line-col-test
  (let [text1 "abc\ndef\ng\nh\ni",
        h (words-and-numbers text1)
        e (words-and-numbers-enlive text1)
        hlc (lc/add-line-col-spans text1 h)
        elc (lc/add-line-col-spans text1 e)]
    (is (= (enlive-line-col-spans elc)
           '({:instaparse.gll/end-column 2, :instaparse.gll/end-line 5, :instaparse.gll/start-column 1, :instaparse.gll/start-line 1, :instaparse.gll/start-index 0, :instaparse.gll/end-index 13} ({:instaparse.gll/end-column 4, :instaparse.gll/end-line 1, :instaparse.gll/start-column 1, :instaparse.gll/start-line 1, :instaparse.gll/start-index 0, :instaparse.gll/end-index 3} "a" "b" "c") ({:instaparse.gll/end-column 4, :instaparse.gll/end-line 2, :instaparse.gll/start-column 1, :instaparse.gll/start-line 2, :instaparse.gll/start-index 4, :instaparse.gll/end-index 7} "d" "e" "f") ({:instaparse.gll/end-column 2, :instaparse.gll/end-line 3, :instaparse.gll/start-column 1, :instaparse.gll/start-line 3, :instaparse.gll/start-index 8, :instaparse.gll/end-index 9} "g") ({:instaparse.gll/end-column 2, :instaparse.gll/end-line 4, :instaparse.gll/start-column 1, :instaparse.gll/start-line 4, :instaparse.gll/start-index 10, :instaparse.gll/end-index 11} "h") ({:instaparse.gll/end-column 2, :instaparse.gll/end-line 5, :instaparse.gll/start-column 1, :instaparse.gll/start-line 5, :instaparse.gll/start-index 12, :instaparse.gll/end-index 13} "i"))))           
    (is (= (hiccup-line-col-spans hlc)
           '({:instaparse.gll/end-column 2, :instaparse.gll/end-line 5, :instaparse.gll/start-column 1, :instaparse.gll/start-line 1, :instaparse.gll/start-index 0, :instaparse.gll/end-index 13} ({:instaparse.gll/end-column 4, :instaparse.gll/end-line 1, :instaparse.gll/start-column 1, :instaparse.gll/start-line 1, :instaparse.gll/start-index 0, :instaparse.gll/end-index 3} "abc") ({:instaparse.gll/end-column 4, :instaparse.gll/end-line 2, :instaparse.gll/start-column 1, :instaparse.gll/start-line 2, :instaparse.gll/start-index 4, :instaparse.gll/end-index 7} "def") ({:instaparse.gll/end-column 2, :instaparse.gll/end-line 3, :instaparse.gll/start-column 1, :instaparse.gll/start-line 3, :instaparse.gll/start-index 8, :instaparse.gll/end-index 9} "g") ({:instaparse.gll/end-column 2, :instaparse.gll/end-line 4, :instaparse.gll/start-column 1, :instaparse.gll/start-line 4, :instaparse.gll/start-index 10, :instaparse.gll/end-index 11} "h") ({:instaparse.gll/end-column 2, :instaparse.gll/end-line 5, :instaparse.gll/start-column 1, :instaparse.gll/start-line 5, :instaparse.gll/start-index 12, :instaparse.gll/end-index 13} "i"))))))

(deftest print-test
  ;; In scenarios when AutoFlattenSeq or FlattenOnDemandVector is
  ;; returned to the user, does the parse output print properly?
  (let [parser-str "<paren-wrapped> = <'('> seq-of-A-or-B <')'>
                    seq-of-A-or-B = ('a' | 'b')*"
        input "(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)"]
    ;; input is 33 "a"s to trigger FlattenOnDemandVector in hiccup
    ;; output format
    (doseq [[output-mode expected-output]
            [[:hiccup (list (into [:seq-of-A-or-B]
                                  (repeat 33 "a")))]
             [:enlive (list {:tag :seq-of-A-or-B
                             :content (repeat 33 "a")})]]

            :let [p (insta/parser parser-str :output-format output-mode)
                  actual-output (p input)]]
      (is (= expected-output actual-output))
      (is (= (with-out-str (prn expected-output))
             (with-out-str (prn actual-output))))
      (is (= (with-out-str (println expected-output))
             (with-out-str (println actual-output))))
      (is (= (str expected-output)
             (str actual-output))))))

(deftest invoke-test
  (let [parser (insta/parser "S = 'a'")
        text "a"]
    (are [x] (= [:S "a"] (parser text))
      (parser text 0 0)
      (parser text 0 0 1 1)
      (parser text 0 0 1 1 2 2)
      (parser text 0 0 1 1 2 2 3 3)
      (parser text 0 0 1 1 2 2 3 3 4 4)
      (parser text 0 0 1 1 2 2 3 3 4 4 5 5)
      (parser text 0 0 1 1 2 2 3 3 4 4 5 5 6 6)
      (parser text 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7)
      (parser text 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8)
      (parser text 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9)
      (parser text 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10))))

#?(:cljs
   (defn ^:export run []
         (run-tests)))

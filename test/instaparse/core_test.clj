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

(def combo-build-example
  (insta/parser
    (merge
      {:S (alt (nt :A) (nt :B))}
      (ebnf "A = 'a'*")
      {:B (ebnf "'b'+")})
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

(def words-and-numbers-auto-whitespace
  (insta/parser
    "sentence = token+
     <token> = word | number
     word = #'[a-zA-Z]+'
     number = #'[0-9]+'"
    :auto-whitespace whitespace))

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

(deftest parsing-tutorial
  (are [x y] (= x y)
    (as-and-bs "aaaaabbbaaaabb")
    [:S
     [:AB [:A "a" "a" "a" "a" "a"] [:B "b" "b" "b"]]
     [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]
    
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
    
    (arithmetic "1-2/(3-4)+5*6")
    (arithmetic "1-2/(3-4)+5*6" :optimize :memory)        
    
    (->> (arithmetic "1-2/(3-4)+5*6")
     (insta/transform
       {:add +, :sub -, :mul *, :div /, 
        :number clojure.edn/read-string :expr identity}))
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
    
    ((insta/parser "S = ('a'?)+") "")
    [:S]
    
    ((insta/parser "S = ('a'?)+") "")
    ((insta/parser "S = ('a'?)+") "" :optimize :memory)    
    
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
         :number (comp (partial array-map :number) clojure.edn/read-string str)}))
    {:tag :sentence, :content [{:word "abc"} {:number 123} {:word "def"}]}
    
    (->> (words-and-numbers-enlive "abc 123 def")
      (insta/transform
        {:word (comp (partial array-map :word) str),
         :number (comp (partial array-map :number) clojure.edn/read-string str)})
      spans-enlive)
    '{:span [0 11], :tag :sentence, :content ({:content (), :span [0 3], :word "abc"} {:content (), :span [4 7], :number 123} {:content (), :span [8 11], :word "def"})}

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
    
    (words-and-numbers-auto-whitespace-and-comments " abc 123 (* 456 *) (* (* 7*) 89 *)  def ")
    [:sentence [:word "abc"] [:number "123"] [:word "def"]]
    ))    


            
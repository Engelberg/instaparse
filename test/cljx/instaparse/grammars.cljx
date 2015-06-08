(ns instaparse.grammars
  #+clj (:refer-clojure :exclude [cat])
  (:require #+clj [clojure.test :refer [deftest are]]
            #+cljs [cljs.test :as t]
            [instaparse.reduction :refer [apply-standard-reductions]]
            [instaparse.combinators :refer [Epsilon opt plus 
                                            star rep alt ord 
                                            cat string string-ci 
                                            regexp nt look neg 
                                            hide hide-tag
                                            ebnf abnf]]
            [instaparse.gll :as gll]
            [instaparse.core :as insta])
  #+cljs (:require-macros [cljs.test :refer [is are deftest run-tests testing]]))

(defn- parse [grammar start text]
  (gll/parse (apply-standard-reductions grammar) start text false))

(defn- parses [grammar start text]
  (gll/parses (apply-standard-reductions grammar) start text false))

(defn- eparse [grammar start text]
  (gll/parse (apply-standard-reductions :enlive grammar) start text false))

(defn- eparses [grammar start text]
  (gll/parses (apply-standard-reductions :enlive grammar) start text false))

;; Grammars built with combinators

(def grammar1 {:s (alt (string "a") (string "aa") (string "aaa"))})
(def grammar2 {:s (alt (string "a") (string "b"))})
(def grammar3 {:s (alt (cat (string "a") (nt :s)) Epsilon)})
(def grammar4 {:y (string "b")
               :x (cat (string "a") (nt :y))})            
(def grammar5 {:s (cat (string "a") (string "b") (string "c"))})
(def grammar6 {:s (alt (cat (string "a") (nt :s)) (string "a"))})
(def grammar7 {:s (alt (cat (string "a") (nt :s)) Epsilon)})
(def grammar8 {:s (alt (cat (string "a") (nt :s) Epsilon) (string "a"))})
(def grammar9 {:s (alt (cat (string "a") (nt :s))
                       (cat (string "b") (nt :s))
                       Epsilon)})
(def grammar10 {:s (alt (cat (nt :s) (string "a") )
                       (cat (nt :s) (string "b") )
                       Epsilon)})
(def grammar11 {:s (alt (cat (nt :s) (string "a")) (string "a"))})
(def grammar12 {:s (alt (nt :a) (nt :a) (nt :a))
                :a (alt (cat (nt :s) (string "a")) (string "a"))})
(def grammar13 {:s (nt :a)
                :a (alt (cat (nt :s) (string "a")) (string "a"))})
(def amb-grammar {:s (alt (string "b") 
                          (cat (nt :s) (nt :s))
                          (cat (nt :s) (nt :s) (nt :s)))})
(def paren-grammar {:s (alt (cat (string "(") (string ")"))
                            (cat (string "(") (nt :s) (string ")"))
                            (cat (nt :s) (nt :s)))})
(def non-ll-grammar {:s (alt (nt :a) (nt :b))
                      :a (alt (cat (string "a") (nt :a) (string "b"))
                              Epsilon)
                      :b (alt (cat (string "a") (nt :b) (string "bb"))
                              Epsilon)})
(def grammar14 {:s (cat (opt (string "a")) (string "b"))})
(def grammar15 {:s (cat (opt (string "a")) (opt (string "b")))})
(def grammar16 {:s (plus (string "a"))})
(def grammar17 {:s (cat (plus (string "a")) (string "b"))})
(def grammar18 {:s (cat (plus (string "a")) (string "a"))})
(def grammar19 {:s (cat (string "a") (plus (alt (string "b")
                                                (string "c"))))})
(def grammar20 {:s (cat (string "a") (plus (cat (string "b")
                                                (string "c"))))})
(def grammar21 {:s (cat (string "a") (plus (alt (string "b")
                                                (string "c")))
                        (string "b"))})
(def grammar22 {:s (star (string "a"))})
(def grammar23 {:s (cat (star (string "a")) (string "b"))})
(def grammar24 {:s (cat (star (string "a")) (string "a"))})
(def grammar25 {:s (cat (string "a") (star (alt (string "b")
                                                (string "c"))))})
(def grammar26 {:s (cat (string "a") (star (cat (string "b")
                                                (string "c"))))})
(def grammar27 {:s (cat (string "a") (star (alt (string "b")
                                                (string "c")))
                        (string "b"))})
(def grammar28 {:s (regexp "a[0-9]b+c")})
(def grammar29 {:s (plus (opt (string "a")))})
(def grammar30 {:s (alt (nt :a) (nt :b))
                :a (plus (cat (string "a") (string "b")))
                :b (plus (cat (string "a") (string "b")))})
;equal: [zero one | one zero]   ;; equal number of "0"s and "1"s.
;
;zero: "0" equal | equal "0"    ;; has an extra "0" in it.
;
;one: "1" equal | equal "1"     ;; has an extra "1" in it.
(def equal-zeros-ones {:equal (opt (alt (cat (nt :zero) (nt :one))
                                        (cat (nt :one) (nt :zero))))
                       :zero (alt (cat (string "0") (nt :equal))
                                  (cat (nt :equal) (string "0")))
                       :one (alt (cat (string "1") (nt :equal))
                                 (cat (nt :equal) (string "1")))})
(def grammar31 {:equal (alt (cat (string "0") (nt :equal) (string "1"))
                            (cat (string "1") (nt :equal) (string "0"))
                            (cat (nt :equal) (nt :equal))
                            Epsilon)})
; Another slow one
(def grammar32 {:s (alt (string "0")
                        (cat (nt :s) (nt :s))
                        Epsilon)})

(def grammar33 {:s (alt (cat (nt :s) (nt :s))
                        Epsilon)})
(def grammar34 {:s (alt (nt :s) Epsilon)})
(def grammar35 {:s (opt (cat (nt :s) (nt :s)))})
(def grammar36 {:s (cat (opt (nt :s)) (nt :s))})
(def grammar37 {:s (cat (nt :s) (opt (nt :s)))})
(def grammar38 {:s (regexp "a[0-9](bc)+")})
(def grammar39 {:s (cat (string "0") (hide (string "1"))(string "2"))})
(def grammar40 {:s (nt :aa)
                :aa (hide-tag (alt Epsilon (cat (string "a") (nt :aa))))})
(def grammar41 {:s (cat (string "b") (plus (string "a")))})
(def grammar42 {:s (cat (string "b") (star (string "a")))})
(def grammar43 {:s (cat (star (string "a")) (string "b"))})
(def grammar44 {:s (cat (look (string "ab")) (nt :ab))
                :ab (plus (alt (string "a") (string "b")))})
(def grammar45 {:s (cat (nt :ab) (look (string "ab")))
                :ab (plus (alt (string "a") (string "b")))})

(def grammar46 {:s (cat (nt :ab) (look Epsilon))
                :ab (plus (alt (string "a") (string "b")))})
(def grammar47 {:s (cat (neg (string "ab")) (nt :ab))
                :ab (plus (alt (string "a") (string "b")))})
(def grammar48 {:s (cat (nt :ab) (neg (string "ab")))
                :ab (plus (alt (string "a") (string "b")))})
(def grammar49 {:s (cat (nt :ab) (neg Epsilon))
                :ab (plus (alt (string "a") (string "b")))})
; Grammar for odd number of a's.  
(def grammar50 {:s (alt (cat (string "a") (nt :s) (string "a"))
                        (string "a"))})
(def grammar51 {:s (hide-tag (alt (cat (string "a") (nt :s) (string "a"))
                                  (string "a")))})
(def grammar52 {:s (hide-tag (alt (cat (string "a") (nt :s) (string "b"))
                                  (string "a")))})
(def grammar53 {:s (hide-tag (alt (cat (string "a") (nt :s) (string "a"))
                                  (string "b")))})
(def grammar54 {:s (cat (string "a")
                        (star (string "aa")))})
(def grammar55 {:s (alt (cat (string "a") (nt :s) (opt (string "a")))
                        (string "a"))})
(def grammar56 {:s (alt (string "a")
                        (cat (string "a") (nt :s) (string "a"))
                        )})
;; PEG grammars
(def grammar57 {:s (ord (plus (string "aa"))
                        (plus (string "a")))})

(def grammar58 {:s (cat (ord (plus (string "aa"))
                             (plus (string "a")))
                        (string "b"))})

(def grammar59 {:S (cat (look (cat (nt :A) (string "c")))
                        (plus (string "a"))
                        (nt :B)
                        (neg (ord (string "a") (string "b") (string "c"))))
                :A (cat (string "a") (opt (nt :A)) (string "b"))
                :B (hide-tag (cat (string "b") (opt (nt :B)) (string "c")))}) 

(def grammar60 {:Expr (ord (nt :Product) (nt :Sum) (nt :Value))
                :Product (cat (nt :Expr) 
                              (star (cat (alt (string "*")
                                              (string "/"))
                                         (nt :Expr))))
                :Sum (cat (nt :Expr)
                          (star (cat (alt (string "+")
                                          (string "-"))
                                     (nt :Expr))))
                :Value (alt (regexp "[0-9]+")
                            (cat (string "(")
                                 (nt :Expr)
                                 (string ")")))})
                            

(def grammar61 {:Expr (alt (nt :Product) (nt :Value))
                :Product (cat (nt :Expr) 
                              (star (cat (alt (string "*")
                                              (string "/"))
                                         (nt :Expr))))                
                :Value (alt (string "[0-9]+")
                            (cat (string "(")
                                 (nt :Expr)
                                 (string ")")))})

(def grammar62 {:Expr (alt (nt :Product) (string "0"))
                :Product (plus (nt :Expr))}) 
                
(def grammar63 {:Expr (alt (nt :Expr) (string "0"))})
(def grammar64 {:Expr (hide-tag (alt (nt :Product) 
                                     (cat (neg (nt :Product)) (nt :Sum))
                                     (cat (neg (nt :Product))
                                          (neg (nt :Sum))
                                          (nt :Value))))
                :Product (cat (nt :Expr) 
                              (star (cat (alt (string "*")
                                              (string "/"))
                                         (nt :Expr))))
                :Sum (cat (nt :Expr)
                          (star (cat (alt (string "+")
                                          (string "-"))
                                     (nt :Expr))))
                :Value (alt (regexp "[0-9]+")
                            (cat (string "(")
                                 (nt :Expr)
                                 (string ")")))})

(def grammar65 {:s (cat (alt (plus (string "aa"))
                             (cat 
                               (neg (plus (string "aa")))
                               (plus (string "a"))))
                        (string "b"))})

(def grammar66 {:s (neg (nt :s))})
(def grammar67 {:s (cat (neg (nt :s)) (string "0"))})
(def grammar68 {:s (cat (neg (nt :a)) (string "0"))
                :a (neg (nt :s))})
(def grammar69 {:s (cat (neg (nt :a)) (string "abc"))
                :a (cat (neg (string "b")) (string "c"))})
(def grammar70 {:s (cat (neg (nt :a)) (string "abc"))
                :a (cat (neg (string "b")) (string "a"))})

(deftest testing-grammars
  (are [x y] (= x y)
       (parse grammar1 :s "a") [:s "a"]
       (parse grammar1 :s "aa") [:s "aa"]
       (parse grammar1 :s "aaa") [:s "aaa"]
       (insta/failure? (parse grammar1 :s "b")) true
       (parse grammar2 :s "b") [:s "b"]
       
       (parse grammar3 :s "aaaaa")
       [:s "a" [:s "a" [:s "a" [:s "a" [:s "a" [:s]]]]]]
       
       (eparse grammar3 :s "aaa")
       '{:tag :s, :content ("a" {:tag :s, :content ("a" {:tag :s, :content ("a" {:tag :s, :content nil})})})}
       
       (parse grammar4 :x "ab")
       [:x "a" [:y "b"]]
       
       (parse grammar5 :s "abc")
       [:s "a" "b" "c"]
       
       (eparse grammar5 :s "abc")
       '{:tag :s, :content ("a" "b" "c")}
       
       (parse grammar6 :s "aaaa")
       [:s "a" [:s "a" [:s "a" [:s "a"]]]]
       
       (parse grammar7 :s "aaaa")
       [:s "a" [:s "a" [:s "a" [:s "a" [:s]]]]]
       
       (parses grammar8 :s "aaaaa")
       '([:s "a" [:s "a" [:s "a" [:s "a" [:s "a"]]]]])
       
       (eparse grammar9 :s "aaa")
       '{:tag :s, :content ("a" {:tag :s, :content ("a" {:tag :s, :content ("a" {:tag :s, :content nil})})})}
       
       (parse grammar9 :s "bbb")
       [:s "b" [:s "b" [:s "b" [:s]]]]

       (parses grammar10 :s "aaaa")
       '([:s [:s [:s [:s [:s] "a"] "a"] "a"] "a"])
       
       (eparses grammar10 :s "bb")
       '({:tag :s, :content ({:tag :s, :content ({:tag :s, :content nil} "b")} "b")})
       
       (parses grammar11 :s "aaaa")
       '([:s [:s [:s [:s "a"] "a"] "a"] "a"])
       
       (parses grammar12 :s "aaa")
       '([:s [:a [:s [:a [:s [:a "a"]] "a"]] "a"]])
       
       (parses grammar13 :s "aaa")
       '([:s [:a [:s [:a [:s [:a "a"]] "a"]] "a"]])

       (parses amb-grammar :s "b")
       '([:s "b"])

       (parses amb-grammar :s "bb")
       '([:s [:s "b"] [:s "b"]])
       
       (parses amb-grammar :s "bbb")
       '([:s [:s "b"] [:s [:s "b"] [:s "b"]]] [:s [:s "b"] [:s "b"] [:s "b"]] [:s [:s [:s "b"] [:s "b"]] [:s "b"]])

       (set (parses amb-grammar :s "bbbb"))
       (set '([:s [:s "b"] [:s [:s "b"] [:s [:s "b"] [:s "b"]]]] [:s [:s [:s "b"] [:s "b"]] [:s "b"] [:s "b"]] [:s [:s [:s "b"] [:s [:s "b"] [:s "b"]]] [:s "b"]] [:s [:s "b"] [:s [:s "b"] [:s "b"]] [:s "b"]] [:s [:s "b"] [:s "b"] [:s [:s "b"] [:s "b"]]] [:s [:s [:s "b"] [:s "b"]] [:s [:s "b"] [:s "b"]]] [:s [:s "b"] [:s [:s "b"] [:s "b"] [:s "b"]]] [:s [:s [:s "b"] [:s "b"] [:s "b"]] [:s "b"]] [:s [:s [:s [:s "b"] [:s "b"]] [:s "b"]] [:s "b"]] [:s [:s "b"] [:s [:s [:s "b"] [:s "b"]] [:s "b"]]]))
       
       (parses paren-grammar :s "(()())()")
       '([:s [:s "(" [:s [:s "(" ")"] [:s "(" ")"]] ")"] [:s "(" ")"]])
       
       (parse non-ll-grammar :s "aabb")
       [:s [:a "a" [:a "a" [:a] "b"] "b"]]
       
       (insta/failure? (parse non-ll-grammar :s "aabbb"))
       true
       
       (parse non-ll-grammar :s "aabbbb")
       [:s [:b "a" [:b "a" [:b] "bb"] "bb"]]
       
       (parse grammar14 :s "b")
       [:s "b"]
       (parse grammar14 :s "ab")
       [:s "a" "b"]
       
       (parse grammar15 :s "ab")
       [:s "a" "b"]
       (parse grammar15 :s "b")
       [:s "b"]
       (parse grammar15 :s "")
       [:s]
       
       (parse grammar16 :s "aaaa")
       [:s "a" "a" "a" "a"]

       (parses grammar17 :s "aaaab")
       '([:s "a" "a" "a" "a" "b"])
       
       (parses grammar18 :s "aaaa")
       '([:s "a" "a" "a" "a"])

       (parse grammar19 :s "abbcbc")
       [:s "a" "b" "b" "c" "b" "c"]
       
       (parse grammar20 :s "abcbcbc")
       [:s "a" "b" "c" "b" "c" "b" "c"]
       (insta/failure? (parse grammar20 :s "a"))
       true
       
       (parse grammar22 :s "")
       [:s]
       (parse grammar22 :s "aaa")
       [:s "a" "a" "a"]
       
       (parse grammar23 :s "b")
       [:s "b"]
       (parse grammar23 :s "aab")
       [:s "a" "a" "b"]
       
       (parse grammar24 :s "a")
       [:s "a"]
       (parse grammar24 :s "aaa")
       [:s "a" "a" "a"]
       
       (parse grammar25 :s "a")
       [:s "a"]
       (parse grammar25 :s "abbc")
       [:s "a" "b" "b" "c"]
       (parse grammar26 :s "a")
       [:s "a"]
       (parse grammar26 :s "abc")
       [:s "a" "b" "c"]
       
       (parse grammar28 :s "a4bbbc")
       [:s "a4bbbc"]
       
       (parses grammar29 :s "aaaaa")
       '([:s "a" "a" "a" "a" "a"])
       
       (parses grammar30 :s "ababab")
       '([:s [:b "a" "b" "a" "b" "a" "b"]] [:s [:a "a" "b" "a" "b" "a" "b"]])
       
       (count (parses equal-zeros-ones :equal "00110110"))
       448
       
       (parse grammar31 :equal "00110110")
       [:equal [:equal "0" [:equal [:equal "0" [:equal] "1"] [:equal "1" [:equal] "0"]] "1"] [:equal "1" [:equal] "0"]]

       (parse grammar32 :s "0000")
       [:s [:s "0"] [:s [:s "0"] [:s [:s "0"] [:s "0"]]]]
       
       (insta/failure? (parse grammar33 :s "0000"))
       true
       (insta/failure? (parse grammar34 :s "0000"))
       true
       (insta/failure? (parse grammar35 :s "0000"))
       true       
       (insta/failure? (parse grammar36 :s "0000"))
       true
       (insta/failure? (parse grammar37 :s "0000"))
       true       
       (parse grammar33 :s "")
       [:s]       
       (parse grammar34 :s "")
       [:s]
       (parse grammar35 :s "")
       [:s]
       (insta/failure? (parse grammar36 :s ""))
       true
       (insta/failure? (parse grammar37 :s ""))
       true
       (parse grammar38 :s "a2bcbc")
       [:s "a2bcbc"]
       (parse grammar39 :s "012")
       [:s "0" "2"]
       (eparse grammar39 :s "012")
       '{:tag :s, :content ("0" "2")}
       
       (parse grammar40 :s "aaa")
       [:s "a" "a" "a"]
       (eparse grammar40 :s "aaa")
       '{:tag :s, :content ("a" "a" "a")}
       (parse grammar41 :s "baaaa")
       [:s "b" "a" "a" "a" "a"]
       (parse grammar42 :s "baaaa")
       [:s "b" "a" "a" "a" "a"]
       (insta/failure? (parse grammar41 :s "b"))
       true
       (parse grammar42 :s "b")
       [:s "b"]
       (parse grammar43 :s "b")
       [:s "b"]
       (parse grammar43 :s "ab")
       [:s "a" "b"]
       (parse grammar44 :s "abbab")
       [:s [:ab "a" "b" "b" "a" "b"]]
       (insta/failure? (parse grammar44 :s "bbab"))
       true
       (parse grammar46 :s "babaab")
       [:s [:ab "b" "a" "b" "a" "a" "b"]]
       (insta/failure? (parse grammar45 :s "babaab"))
       true
       (parse grammar47 :s "babaab")
       [:s [:ab "b" "a" "b" "a" "a" "b"]]
       (insta/failure? (parse grammar47 :s "abbab"))
       true
       (parse grammar48 :s "abab")
       [:s [:ab "a" "b" "a" "b"]]
       (insta/failure? (parse grammar49 :s "ababa"))
       true
       (parse grammar50 :s "aaa")
       [:s "a" [:s "a"] "a"]
       (insta/failure? (parse grammar50 :s "aa"))
       true
       (parse grammar51 :s "aaa")
       '("a" "a" "a")
       (eparse grammar51 :s "aaa")
       '("a" "a" "a")       
       (eparse grammar52 :s "aab")
       '("a" "a" "b")
       (eparse grammar53 :s "aba")
       '("a" "b" "a")
       (parse grammar54 :s "aaa")
       [:s "a" "aa"]
       
       (parses grammar55 :s "aaa")
       '([:s "a" [:s "a"] "a"] [:s "a" [:s "a" [:s "a"]]])
       (parses grammar56 :s "aaa")
       '([:s "a" [:s "a"] "a"])
       
       (parses grammar57 :s "aaaa")
       '([:s "aa" "aa"] [:s "a" "a" "a" "a"])
       (parses grammar57 :s "aaaaa")
       '([:s "a" "a" "a" "a" "a"])
       
       (parses grammar58 :s "aaaab")
       '([:s "aa" "aa" "b"] [:s "a" "a" "a" "a" "b"])
       (parses grammar58 :s "aaaaab")
       '([:s "a" "a" "a" "a" "a" "b"])
       (parses grammar59 :S "aaabbbccc")
       '([:S "a" "a" "a" "b" "b" "b" "c" "c" "c"])
       
       (parses grammar65 :s "aaaab")
       '([:s "aa" "aa" "b"])
       (parses grammar65 :s "aaaaab")
       '()
       (parses grammar67 :s "0")
       '([:s "0"])
       (parses grammar68 :s "0")
       '()
       
       (parses grammar69 :s "abc")
       '([:s "abc"])
       (parses grammar70 :s "abc")
       ()
       
       ))
       

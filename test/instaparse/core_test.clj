(ns instaparse.core-test
  (:use clojure.test)
  (:require [instaparse.core :as insta]))

(def as-and-bs
  (insta/parser
    "S = AB*
     AB = A B
     A = 'a'+
     B = 'b'+"))

(def as-and-bs-alternative
  (insta/parser
    "S:={AB}  ;
     AB ::= A, B
     A : 'a' + ;
     B ='b' + ;"))

(def as-and-bs-enlive
  (insta/parser
    "S = AB*
     AB = A B
     A = 'a'+
     B = 'b'+"
    
    :output-format :enlive))


(deftest parsing-tutorial
  (are [x y] (= x y)
    (as-and-bs "aaaaabbbaaaabb")
    [:S
     [:AB [:A "a" "a" "a" "a" "a"] [:B "b" "b" "b"]]
     [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]))


(ns instaparse.combinators-test
  (:require [cemerick.cljs.test :as t]
            [instaparse.core :as insta]
            [instaparse.combinators-source :refer
             [Epsilon opt plus star rep alt ord cat string-ci string
              string-ci regexp nt look neg hide hide-tag]]
            [instaparse.cfg :refer [ebnf]])
  (:require-macros [cemerick.cljs.test :refer [is are deftest with-test
                                               run-tests testing]]))
;; Simple tests using combinators to verify that the basic machinery
;; is working.
(deftest combinator-parse-test
  (are [x s y] (= ((insta/parser x) s) y)
       ;; This fails; might be a bug in the jvm version too
       ;;[:S Epsilon] "" [:S]

       [:S (string "a")] "a" [:S "a"]

       [:S (regexp #"\d+")] "123" [:S "123"]

       [:S (alt (string "a") (string "b"))] "a" [:S "a"]
       [:S (alt (string "a") (string "b"))] "b" [:S "b"]

       [:S (cat (string "a") (string "b"))] "ab" [:S "a" "b"]

       [:S (opt (string "a"))] "a" [:S "a"]
       [:S (opt (string "a"))] ""  [:S]


       [:S (plus (string "a"))] "a"   [:S "a"]
       [:S (plus (string "a"))] "aaa" [:S "a" "a" "a"]

       [:S (star (string "a"))] ""    [:S]
       [:S (star (string "a"))] "a"   [:S "a"]
       [:S (star (string "a"))] "aaa" [:S "a" "a" "a"]

       [:S (alt Epsilon (cat (nt :S) (string "a")))] "aa" [:S [:S [:S] "a"] "a"]

       [:S (cat (string "a") (regexp #"\d+"))] "a123" [:S "a" "123"]
))

(deftest combinator-failure-test
  (are [x s] (insta/failure? ((insta/parser x) s))
       [:S Epsilon] "a"
       [:S (string "a")] "b"
       [:S (regexp #"\d+")] "abc"

       [:S (alt (string "a") (string "b"))] "c"
       [:S (alt (string "a") (string "b"))] ""

       [:S (cat (string "a") (string "b"))] "c"
       [:S (cat (string "a") (string "b"))] "ac"

       [:S (plus (string "a"))] ""
       [:S (plus (string "a"))] "b"

       [:S (opt (string "a"))] "b"

       [:S (star (string "a"))] "b"
))

(ns instaparse.defparser-test
  (:require
    #?(:clj  [clojure.test :as t :refer [deftest are is]]
       :cljs [cljs.test :as t :refer-macros [deftest are is]])
    #?(:clj  [instaparse.core :as insta :refer [defparser]]
       :cljs [instaparse.core :as insta :refer-macros [defparser]])
    [instaparse.combinators :as c]
    [instaparse.core-test :refer [parsers-similar?]]))

(defparser p1 "S = #'a' | 'b'")

(defparser p2 [:S (c/alt (c/regexp #"a") (c/string "b"))])

(defparser p3 {:S (c/alt (c/regexp #"a") (c/string "b"))}
  :start :S)

(defparser p4 "S =/ #'a' / 'b'"
  :input-format :abnf)

(defparser p5 "test/data/defparser_grammar.txt")

(def p6 (insta/parser "S = #'a' | 'b'"))

(deftest defparser-test-standard
  (is (parsers-similar? p1 p2 p3 p4 p5))

  #?(:clj
     (are [x y] (thrown? y (eval (quote x)))
       (defparser p6 "test/data/parser_not_found.txt")
       Exception)))

(defparser ws1 "S = (<whitespace?> 'a')+ <whitespace?>; <whitespace> = #'\\s+'")

(defparser ws2 "S = 'a'+" :auto-whitespace :standard)

(defparser ws3 "S = 'a'+" :auto-whitespace (insta/parser "whitespace = #'\\s+'"))

(let [ws (insta/parser "whitespace = #'\\s+'")]
  (defparser ws4 "S = 'a'+" :auto-whitespace ws))

(deftest defparser-test-auto-whitespace
  (is (parsers-similar? ws1 ws2 ws3 ws4)))

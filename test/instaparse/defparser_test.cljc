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

(defparser p4 "test/data/defparser_grammar.txt")

(def p5 (insta/parser "S = #'a' | 'b'"))

(deftest defparser-test-standard
  (is (parsers-similar? p1 p2 p3 p4 p5))

  #?(:clj
     (are [x y] (thrown? y (eval (quote x)))
       (defparser p6 "test/data/parser_not_found.txt")
       Exception)))

(defparser a1 "S = #'a' / 'b'"
  :input-format :abnf)

(def a2 (insta/parser "S = #'a' / 'b'" :input-format :abnf))

(def a3 (insta/parser "S = #'a' | 'b'" :input-format :ebnf, :string-ci true))

(deftest defparser-test-abnf
  (is (parsers-similar? a1 a2 a3)))

(defparser ws1 "S = (<whitespace?> 'a')+ <whitespace?>; <whitespace> = #'\\s+'")

(defparser ws2 "S = 'a'+" :auto-whitespace :standard)

(defparser ws3 "S = 'a'+" :auto-whitespace (insta/parser "whitespace = #'\\s+'"))

(let [ws (insta/parser "whitespace = #'\\s+'")]
  (defparser ws4 "S = 'a'+" :auto-whitespace ws))

(def ws5 (insta/parser "S = 'a'+" :auto-whitespace :standard))

(defparser ws6 "<whitespace> = #'\\s+'; S = (<whitespace?> 'a')+ <whitespace?>"
  :start :S)

(deftest defparser-test-auto-whitespace
  (is (parsers-similar? ws1 ws2 ws3 ws4 ws5 ws6)))

(defparser e1 "S = 'a'+" :output-format :enlive)

(def e2 (insta/parser "S = 'a'+" :output-format :enlive))

(deftest defparser-test-enlive
  (is (parsers-similar? e1 e2))
  (is (= (e2 "a") (e1 "a"))))

(ns instaparse.path-log
  (:require
    #?(:clj  [clojure.test :refer [deftest is]]
       :cljs [cljs.test :as t])
    #?(:clj  [instaparse.core :as insta]
       :cljs [instaparse.core :as insta]))
  #?(:cljs (:require-macros
             [cljs.test :refer [is deftest]])))

(def simple-parser
  (insta/parser
    "TOP = R1+ \"\\n\"
    R1 = ( 'foo' | 'bar' | R3 )+
       | 'baz'
       | R2
    R2 = 'qux'
       | 'quux'
       | 'quuux'
    R3 = 'z'"))

(def text1
  "foo\n")

(def text2
  "barquuxz\n")

(def text3
  "quxquxquuxquuxquuuxquuux\n")

(deftest path-log-tests
  (let [res1 (simple-parser text1)
        path-freqs1 (->> res1 meta :path-log frequencies)
        res2 (simple-parser text2)
        path-freqs2 (->> res2 meta :path-log frequencies)
        res3 (simple-parser text3)
        path-freqs3 (->> res3 meta :path-log frequencies)]

    (is (= res1
           [:TOP
            [:R1 "foo"]
            "\n"]))
    (is (= path-freqs1
           {[:TOP] 1,
            [:TOP :cat 0] 1,
            [:TOP :cat 0 :plus] 1,
            [:TOP :cat 1] 1,
            [:R1] 1,
            [:R1 :alt 0] 1,
            [:R1 :alt 0 :plus] 1,
            [:R1 :alt 0 :plus :alt 0] 1}))

    (is (= res2
           [:TOP
            [:R1 "bar"]
            [:R1 [:R2 "quux"]]
            [:R1 [:R3 "z"]]
            "\n"]))
    (is (= path-freqs2
           {[:TOP] 1,
            [:TOP :cat 0] 3,
            [:TOP :cat 0 :plus] 3,
            [:TOP :cat 1] 1,
            [:R1] 3,
            [:R1 :alt 0] 2,
            [:R1 :alt 0 :plus] 2,
            [:R1 :alt 0 :plus :alt 1] 1,
            [:R1 :alt 0 :plus :alt 2] 1,
            [:R1 :alt 2] 1,
            [:R2] 1,
            [:R2 :alt 1] 1,
            [:R3] 1}))

    (is (= res3
           [:TOP
            [:R1 [:R2 "qux"]]
            [:R1 [:R2 "qux"]]
            [:R1 [:R2 "quux"]]
            [:R1 [:R2 "quux"]]
            [:R1 [:R2 "quuux"]]
            [:R1 [:R2 "quuux"]]
            "\n"]))
    (is (= path-freqs3
           {[:TOP] 1,
            [:TOP :cat 0] 6,
            [:TOP :cat 0 :plus] 6,
            [:TOP :cat 1] 1,
            [:R1] 6,
            [:R1 :alt 2] 6,
            [:R2] 6,
            [:R2 :alt 0] 2,
            [:R2 :alt 1] 2,
            [:R2 :alt 2] 2}))))


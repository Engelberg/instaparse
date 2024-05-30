(ns instaparse.namespaced-nts-test
  (:require
   #?(:clj [clojure.test :refer [deftest is]]
      :cljs [cljs.test :as t])
   #?(:clj [instaparse.core :as insta]
      :cljs [instaparse.core :as insta]))
  #?(:cljs (:require-macros
            [cljs.test :refer [is deftest]])))

(def namespaced-nts-parser
  (insta/parser
   "S = token (<ws> token)*
    ws = #'\\s+'
    keyword/hello = 'hello'
    keyword.namespaced/bye = 'bye'
    <keyword> = keyword/hello | keyword.namespaced/bye
    identifier = #'\\S+'
    token = keyword / identifier"
   :allow-namespaced-nts true))

(deftest parser
  (is (= (namespaced-nts-parser "bye") [:S [:token [:keyword.namespaced/bye "bye"]]])))

(deftest round-trip
  (let [grammar (prn-str namespaced-nts-parser)]
    (is (= grammar
           (prn-str (insta/parser
                     grammar
                     :allow-namespaced-nts true))))))


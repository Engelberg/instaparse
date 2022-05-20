(ns instaparse.retain-comments
  (:require
    #?(:clj  [clojure.test :refer [deftest is]]
       :cljs [cljs.test :as t])
    #?(:clj  [instaparse.core :as insta]
       :cljs [instaparse.core :as insta]))
  #?(:cljs (:require-macros
             [cljs.test :refer [is deftest]])))

(def retain1
  (insta/parser
    "foo = 'bar' (* a comment for 'bar' *)
    | 'baz'"))

(def retain2
  (insta/parser
    "(* this is a top-level comment *)
    foo = 'bar' (* a comment for 'bar' *)
        | 'baz' (* a comment for 'baz' *)"))

(def retain3
  (insta/parser
    "(* foo comment 1 *)
    (* foo comment 2 *)
    foo = 'bar' (* 'bar' comment 1 *) (* 'bar' comment 2 *)
        | 'baz' (* 'baz' comment 1 *) (* 'baz' comment 2 *) ;
    (* qux comment 1 *)
    (* qux comment 2 *)
    qux = 'quux' (* 'quux' comment 1 *) (* 'quux' comment 2 *)
        | 'quuux' (* 'quuux' comment 1 *) (* 'quuux' comment 2 *) ;
    (* blah comment 1 *)
    (* blah comment 2 *)
    blah = 'blahblah' (* 'blahblah' comment 1 *) (* 'blahblah' comment 2 *)"))

(def retain4
  (insta/parser
    "(* foo comment *)
    foo = 'bar' (* 'bar' comment *)
        | 'baz' (* 'baz' comment 1 *)
    (* 'baz' comment 2 *)
    qux = 'quux' (* 'quux' comment *)
        | 'quuux' (* 'quuux' comment *)"))


(deftest retain-tests
  (is (= (-> retain1 :grammar :foo :parsers first :comments)
         '(" a comment for 'bar' ")))

  (is (= (-> retain2 :grammar :foo :parsers first :comments)
         '(" a comment for 'bar' ")))
  (is (= (-> retain2 :grammar :foo :parsers second :comments)
         '(" a comment for 'baz' ")))

  (is (= (-> retain3 :grammar :foo :comments)
         '(" foo comment 1 " " foo comment 2 ")))
  (is (= (-> retain3 :grammar :foo :parsers first :comments)
         '(" 'bar' comment 1 " " 'bar' comment 2 ")))
  (is (= (-> retain3 :grammar :foo :parsers second :comments)
         '(" 'baz' comment 1 " " 'baz' comment 2 ")))
  (is (= (-> retain3 :grammar :qux :comments)
         '(" qux comment 1 " " qux comment 2 ")))
  (is (= (-> retain3 :grammar :qux :parsers first :comments)
         '(" 'quux' comment 1 " " 'quux' comment 2 ")))
  (is (= (-> retain3 :grammar :qux :parsers second :comments)
         '(" 'quuux' comment 1 " " 'quuux' comment 2 ")))

  (is (= (-> retain4 :grammar :foo :comments)
         '(" foo comment ")))
  (is (= (-> retain4 :grammar :foo :parsers first :comments)
         '(" 'bar' comment ")))
  (is (= (-> retain4 :grammar :foo :parsers second :comments)
         '(" 'baz' comment 1 " " 'baz' comment 2 ")))
  (is (= (-> retain4 :grammar :qux :parsers first :comments)
         '(" 'quux' comment ")))
  (is (= (-> retain4 :grammar :qux :parsers second :comments)
         '(" 'quuux' comment "))))


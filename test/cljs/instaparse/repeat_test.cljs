(ns instaparse.repeat-test
  (:require [instaparse.core :as insta]
            [instaparse.repeat :as repeat])
  (:require-macros [instaparse.repeat-test :refer [text-slurp]]
                   [cemerick.cljs.test :refer [is are deftest with-test 
                                               run-tests testing]]))

(def text (text-slurp))

(def user-parser
"content = user-block*
user-block = (user before-section after-section < blank-line* >)
user = prefix separator number separator name newline
before-section = < before > lines error-line*
after-section = < after > lines
<before> = < 'BEFORE' newline >
<after> = < 'AFTER' newline >
<prefix> = < 'User' >
<lines> = line*
<line> = <#'\\s+'> subscription newline
<error-line> = ( '(no dates!)' | 'FIXUP!' ) newline
blank-line = #'\\s*\n'
name = #'.*'
(*WIP why infinite loop?*)
subscription = !prefix #'.*?(?=\\s+-)' < separator > date
date = #'.*'
<newline> = <'\n'>
<separator> = <#'[ -]+'>
number = #'[0-9]+'
 
")

(deftest memory-optimize-test 
  (are [grammar text optimize?]
       (let [parser (insta/parser grammar)
             parser-enlive (insta/parser grammar :output-format :enlive)
             tree1 (parser text)
             tree2 (parser text :optimize :memory)
             tree3 (parser-enlive text)
             tree4 (parser-enlive text :optimize :memory)]
         (and (= tree1 tree2) (= tree3 tree4)
            (= optimize? (repeat/used-memory-optimization? tree2))
            (= optimize? (repeat/used-memory-optimization? tree4))))
       
       ;user-parser text true
       "S = 'ab'*" "ababab" true
       "S = 'ab'*" "abababd" false
       "S = 'ab'*" "" false
       "<S> = 'ab'*" "ababab" true
       "<S> = 'ab'*" "abababd" false
       "<S> = 'ab'*" "" false
       "S = <'ab'>*" "ababab" false
       "S = <'ab'*>" "ababab" false
       
       "S = A*; A = 'a'" "aaaa" true
       "S = A*; A = 'a'" "aaaad" false
       "S = A*; A = 'a'" "" false
       "<S> = A*; A = 'a'" "aaaa" true
       "<S> = A*; A = 'a'" "aaaad" false
       "<S> = A*; A = 'a'" "" false
       "S = <A>*; A = 'a'" "aaaa" false
       "S = <A*>; A = 'a'" "aaaa" false
       
       "S = 'ab'+" "ababab" true
       "S = 'ab'+" "abababd" false
       "S = 'ab'+" "" false
       "<S> = 'ab'+" "ababab" true
       "<S> = 'ab'+" "abababd" false
       "<S> = 'ab'+" "" false
       "S = <'ab'>+" "ababab" false
       "S = <'ab'+>" "ababab" false
       
       "S = A+; A = 'a'" "aaaa" true
       "S = A+; A = 'a'" "aaaad" false
       "S = A+; A = 'a'" "" false
       "<S> = A+; A = 'a'" "aaaa" true
       "<S> = A+; A = 'a'" "aaaad" false
       "<S> = A+; A = 'a'" "" false
       "S = <A>+; A = 'a'" "aaaa" false
       "S = <A+>; A = 'a'" "aaaa" false

       "S = 'c' 'ab'*" "cababab" true
       "S = 'c' 'ab'*" "cabababd" false
       "S = 'c' 'ab'*" "dababab" false
       "S = 'c' 'ab'*" "c" false
       "S = 'c' 'ab'*" "" false
       "<S> = 'c' 'ab'*" "cababab" true
       "<S> = 'c' 'ab'*" "cabababd" false
       "<S> = 'c' 'ab'*" "dcababab" false
       "<S> = 'c' 'ab'*" "c" false
       "<S> = 'c' 'ab'*" "" false       
       "S = 'c' <'ab'>*" "cababab" false
       "S = 'c' <'ab'*>" "cababab" false
       "S = <'c'> <'ab'>*" "cababab" false
       "S = <'c'> 'ab'*" "cababab" false
       
       "S = 'c' A*; A = 'a'" "caaaa" true
       "S = 'c' A*; A = 'a'" "caaaad" false
       "S = 'c' A*; A = 'a'" "dcaaaad" false
       "S = 'c' A*; A = 'a'" "c" false
       "<S> = 'c' A*; A = 'a'" "caaaa" true
       "<S> = 'c' A*; A = 'a'" "caaaad" false
       "<S> = 'c' A*; A = 'a'" "daaaad" false
       "<S> = 'c' A*; A = 'a'" "c" false
       "S = 'c' <A>*; A = 'a'" "caaaa" false
       "S = 'c' <A*>; A = 'a'" "caaaa" false
       
       "S = 'c' 'ab'+" "cababab" true
       "S = 'c' 'ab'+" "dababab" false
       "S = 'c' 'ab'+" "abababd" false
       "S = 'c' 'ab'+" "c" false
       "S = 'c' 'ab'+" "" false
       "<S> = 'c' 'ab'+" "cababab" true
       "<S> = 'c' 'ab'+" "cabababd" false
       "<S> = 'c' 'ab'+" "dcababab" false
       "<S> = 'c' 'ab'+" "c" false
       "<S> = 'c' 'ab'+" "" false       
       "S = 'c' <'ab'>+" "cababab" false
       "S = 'c' <'ab'+>" "cababab" false
       "S = <'c'> <'ab'>+" "cababab" false
       "S = <'c'> 'ab'+" "cababab" false
       
       "S = 'c' A+; A = 'a'" "caaaa" true
       "S = 'c' A+; A = 'a'" "caaaad" false
       "S = 'c' A+; A = 'a'" "dcaaaa" false
       "S = 'c' A+; A = 'a'" "c" false
       "<S> = 'c' A+; A = 'a'" "caaaa" true
       "<S> = 'c' A+; A = 'a'" "caaaad" false
       "<S> = 'c' A+; A = 'a'" "dcaaaa" false
       "<S> = 'c' A+; A = 'a'" "c" false
       "S = 'c' <A>+; A = 'a'" "caaaa" false
       "S = 'c' <A+>; A = 'a'" "caaaa" false
       
       "S = C A+; C = 'c'; A = 'a'" "caaaa" true
       "S = C A+; C = 'c'; <A> = 'a'" "caaaa" true
       "S = C A+; <C> = 'c'; A = 'a'" "caaaa" true
       "S = C A+; <C> = 'c'; <A> = 'a'" "caaaa" true
       "S = <C> A+; C = 'c'; A = 'a'" "caaaa" false       
       "S = C A+; C = 'c'; A = 'a'" "caaaad" false
       "S = C A+; C = 'c'; A = 'a'" "dcaaaa" false
       "S = C A+; C = 'c'; A = 'a'" "c" false
       "<S> = C A+; C = 'c'; A = 'a'" "caaaa" true
       "<S> = <C> A+; C = 'c'; A = 'a'" "caaaa" false
       "<S> = C A+; C = 'c'; A = 'a'" "caaaad" false
       "<S> = C A+; C = 'c'; A = 'a'" "dcaaaa" false
       "<S> = C A+; C = 'c'; A = 'a'" "c" false
       "S = C <A>+; C = 'c'; A = 'a'" "caaaa" false
       "S = C <A+>; C = 'c'; A = 'a'" "caaaa" false
       ))

(ns instaparse.re-parse
    (:require [instaparse.core :as insta]))
                  
(defn- e-tree-seq 
  "tree-seqs enlive trees/graphs, at least instaparse ones"
  [e-tree]
  (if (map? (first e-tree))
      (tree-seq (comp seq :content) :content (first e-tree))
      (tree-seq (comp seq :content) :content e-tree)))
                                      
(defn- flatten-enlive
  "flattens an enlive tree (instaparse dialect)"
  [tree]
  (apply str (filter string? (e-tree-seq tree))))
                                         
(defn- flatten-hiccup
  "flattens a hiccup tree (instparse dialect)"
  [tree]
  (apply str (filter string? (flatten tree))))
                    
(defn re-parse
  "[parser tree (:rule)]
  Re-parse an instaparse tree with a parser
  If :rule is given, re-parse only those nodes matching
  :rule."
  ([parser tree]
  (if (vector? tree)
         (insta/parse parser (flatten-hiccup tree))
         (insta/parse parser (flatten-enlive tree))))
  ([parser tree rule]
  (if (vector? tree)
         (insta/transform {rule (fn [& node] (re-parse parser [rule node]))} tree)
         (insta/transform {rule (fn [& node] (re-parse parser {:tag rule, :content node}))} tree))))
                                             
                                             
;;;;;;;;;;;;;;;;;
; Demonstration ;
;;;;;;;;;;;;;;;;;
                         
                         
(def  ^:private m-enl
     "simple tree parser"
     (insta/parser "tree: node* 
              node: leaf | '(' node (<'('> node <')'>)* node* ')' 
              leaf: #'a+'
              " :output-format :enlive))

(def ^:private edn-enl ;
     "toy edn parser"
     (insta/parser (slurp "edn.grammar") :output-format :enlive))

(def ^:private m-hic
    (insta/parser "tree: node* 
              node: leaf | '(' node (<'('> node <')'>)* node* ')' 
              leaf: #'a+' ; "))

(= (flatten-hiccup (m-hic "a(a(a)a)a")) 
   "a(a(a)a)a")                              ; true
(= (flatten-enlive (m-enl "a(a)a"))          ; <-this is a map
   "a(a)a")                                  ; true 
(= (flatten-enlive (edn-enl "{:foo bar}"))   ; <-this is a list w. a map in it
   "{:foo bar}")                             ; true 


                                         
(= (re-parse m-hic (m-hic "a(a)a"))
   (m-hic "a(a)a"))                        ; true
(= (re-parse m-enl (m-enl "a(a)a"))
   (m-enl "a(a)a"))                        ; true
(= (re-parse m-enl (m-hic "a(a)a"))
   (m-enl "a(a)a"))                        ; true
(= (re-parse m-hic (m-hic "a(a)a") :tree)  
   (m-hic "a(a)a"))                        ; true
(= (re-parse m-enl (m-enl "a(a)a") :tree) 
   (m-enl "a(a)a"))                        ; true 
          
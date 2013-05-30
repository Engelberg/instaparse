(ns instaparse.viz-test
  (:use instaparse.core instaparse.viz))

(def make-tree-e
     "simple tree parser"
     (instaparse.core/parser "tree: node* 
              node: leaf | <'('> node (<'('> node <')'>)* node* <')'> 
              leaf: #'a+'
              " :output-format :enlive))

(def make-tree-h
     "simple tree parser"
     (instaparse.core/parser "tree: node* 
              node: leaf | <'('> node (<'('> node <')'>)* node* <')'> 
              leaf: #'a+'
              " :output-format :hiccup))

(def make-tree-se
     "simple tree parser"
     (instaparse.core/parser "<tree>: node* 
              node: leaf | <'('> node (<'('> node <')'>)* node* <')'> 
              leaf: #'a+'
              " :output-format :enlive))

(def make-tree-sh
     "simple tree parser"
     (instaparse.core/parser "<tree>: node* 
              node: leaf | <'('> node (<'('> node <')'>)* node* <')'> 
              leaf: #'a+'
              " :output-format :hiccup))

(defn view-test-trees [t]
  (tree-viz (make-tree-e "((a)((a)))(a)"))
  (Thread/sleep t)
  (tree-viz (make-tree-h "((a)((a)))(a)"))
  (Thread/sleep t)
  (tree-viz (make-tree-sh "((a)((a)))(a)"))
  (Thread/sleep t)
  (tree-viz (make-tree-se "((a)((a)))(a)"))
  (Thread/sleep t)
  (tree-viz (make-tree-e ""))
  (Thread/sleep t)
  (tree-viz (make-tree-se "")))
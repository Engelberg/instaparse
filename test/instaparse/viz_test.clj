(ns instaparse.viz-test
  (:use clojure.test)
  (:require instaparse.core)
  (:use instaparse.viz))

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

(def make-tree-special ;; for testing special chars in node description
     "simple tree parser"
     (instaparse.core/parser "tree: node*
              node: leaf | <'('> node (<'('> node <')'>)* node* <')'>
              leaf: #'[^()]+'
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

(deftest create_buffered_images
  (testing "works — returns a java.awt.image.BufferedImage"
    (is (#'instaparse.viz/hiccup-tree-viz (make-tree-special "((no)(escaping)(needed)(here))") {})))
  (testing "should be fixed — returns nil, but needs be a java.awt.image.BufferedImage"
    (is (#'instaparse.viz/hiccup-tree-viz (make-tree-special "((a)(\"string\")(requiring)(escaping))") {}))))

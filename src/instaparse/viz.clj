(ns instaparse.viz
    (:require [rhizome.viz :refer [view-tree]]
              [instaparse.core :refer [parser parse parses]]))
                                       
              
(def make-tree
     "simple tree parser"
     (parser "tree: node* 
              node: leaf | <'('> node (<'('> node <')'>)* node* <')'> 
              leaf: #'a+'
              "))
              
(def a-tree (make-tree "aaaa(aaaa(aaa(aa)aa)aaa)aaaaa"))  
              
(defn- acceptable? [node]
       (not (keyword? node)))
              
(defn- seq-for-labels [tree]
            (filter acceptable? tree))
              
(defn tree-viz 
    "visualize instaparse hiccup output as a rhizome graph" 
    [tree]
    (view-tree sequential? seq-for-labels tree 
               :node->descriptor (fn [n] {:label (if (vector? n) 
                                                     (first n) 
                                                     (when (string? n) n ))})))

(tree-viz a-tree)
                                   
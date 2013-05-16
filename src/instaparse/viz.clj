(ns instaparse.viz)   

(try 
  (require '[rhizome.viz :as r])
  (catch Exception e
    (require '[instaparse.viz-not-found :as r])))

(defn tree-viz 
    "visualize instaparse hiccup output as a rhizome graph. Requires rhizome: https://github.com/ztellman/rhizome"
    [mytree]           
      (r/view-tree sequential? rest mytree 
                   :node->descriptor (fn [n] {:label (if (vector? n) 
                                                       (first n) 
                                                       (when (string? n) n ))})))

(def make-tree
     "simple tree parser"
     (instaparse.core/parser "tree: node* 
              node: leaf | <'('> node (<'('> node <')'>)* node* <')'> 
              leaf: #'a+'
              " :output-format :enlive))
              
(defn- enlive-seq
  [tree]
  (if (map? tree)          
      (:content tree)
      (first tree)))

(defn- kids? [node]
  (not (= nil (:content node))))

(defn e-tree-viz
  "visualize enlive trees"
  [mytree]
  (r/view-tree kids? enlive-seq mytree 
             :node->descriptor (fn [n] 
                                 {:label (if (string? n)
                                             n
                                             (:tag n))})))
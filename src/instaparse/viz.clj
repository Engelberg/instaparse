(ns instaparse.viz
    (:require [rhizome.viz :refer [view-tree]]
              #_[instaparse.core :refer [parser parse parses]]))
 
              
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
                                   
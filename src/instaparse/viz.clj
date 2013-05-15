(ns instaparse.viz
    (:require [rhizome.viz :refer [view-tree]]))
            
              
(defn tree-viz 
    "visualize instaparse hiccup output as a rhizome graph"
    [mytree]
    (view-tree sequential? rest mytree 
               :node->descriptor (fn [n] {:label (if (vector? n) 
                                                     (first n) 
                                                     (when (string? n) n ))})))
                                   
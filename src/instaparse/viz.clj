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
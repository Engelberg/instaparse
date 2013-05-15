(ns instaparse.viz)
            
              
(defn tree-viz 
    "visualize instaparse hiccup output as a rhizome graph. Requires rhizome: https://github.com/ztellman/rhizome"
    [mytree]
    (try (use 'rhizome.viz) (catch Exception e (println (str (.getMessage e) " to install rhizome, "
                                                             "visit https://github.com/ztellman/rhizome "
                                                             "and follow the instructions for your platform."))))
    (rhizome.viz/view-tree sequential? rest mytree 
               :node->descriptor (fn [n] {:label (if (vector? n) 
                                                     (first n) 
                                                     (when (string? n) n ))})))
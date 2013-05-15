(ns instaparse.viz)
            
              
(defn tree-viz 
    "visualize instaparse hiccup output as a rhizome graph. Requires rhizome: https://github.com/ztellman/rhizome"
    [mytree]
    (try (use 'rhizome.viz) (catch Exception e (println (str "caught exception: " (.getMessage e)))))
    (rhizome.viz/view-tree sequential? rest mytree 
               :node->descriptor (fn [n] {:label (if (vector? n) 
                                                     (first n) 
                                                     (when (string? n) n ))})))
                                   
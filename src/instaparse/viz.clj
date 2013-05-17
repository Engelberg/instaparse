(ns instaparse.viz
  (:import java.io.IOException))   

(try 
  (require '[rhizome.viz :as r])
  (catch Exception e
    (require '[instaparse.viz-not-found :as r])))

(defn- hiccup-tree-viz
    "visualize instaparse hiccup output as a rhizome graph. Requires rhizome: https://github.com/ztellman/rhizome"
    [mytree]         
    (r/view-tree sequential? rest mytree 
                 :node->descriptor (fn [n] {:label (if (coll? n) 
                                                     (first n) 
                                                     (when (string? n) n ))})))
      
(defn- enlive-tree-viz
  "visualize enlive trees"
  [mytree]
  (r/view-tree (comp seq :content) :content mytree 
             :node->descriptor (fn [n] 
                                 {:label (if (string? n)
                                             n
                                             (:tag n))})))

(defn tree-type
  [tree]
  (cond
    (and (map? tree) (:tag tree)) :enlive
    (vector? tree) :hiccup
    (empty? tree) :nil
    (seq? tree) :rootless
    :else :invalid))

(defn fake-root
  "Create a root for a rootless tree"
  [children]
  (case (tree-type (first children))
    :enlive {:tag :hidden-root-tag
             :content children}
    :hiccup (into [:hidden-root-tag]
                  children)
    :nil nil
    :invalid))
    
(defn tree-viz
  [tree]
  {:pre [(not= (tree-type tree) :invalid)]}
  (try
    (case (tree-type tree)
      :enlive (enlive-tree-viz tree)
      (:hiccup :nil) (hiccup-tree-viz tree)
      :rootless (tree-viz (fake-root tree)))
    (catch IOException e
      (throw (UnsupportedOperationException. 
               "\n\nYou appear to have rhizome in your dependencies, but have not installed GraphViz on your system.
\nSee https://github.com/ztellman/rhizome for more information.\n")))))
    

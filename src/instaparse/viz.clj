(ns instaparse.viz
  (:import java.io.IOException))

(defn span
  "Takes a subtree of the parse tree and returns a [start-index end-index] pair
   indicating the span of text parsed by this subtree.
   start-index is inclusive and end-index is exclusive, as is customary
   with substrings.
   Returns nil if no span metadata is attached."
  [tree]
  (let [m (meta tree)
        s (:instaparse.gll/start-index m)
        e (:instaparse.gll/end-index m)]
    (when (and s e)
      [s e])))

(def rhizome-newline
  ;; Prior to Rhizome 0.2.5., \ was not an escape character so \n needed extra escaping.
  (when-let [escape-chars (try (ns-resolve (find-ns 'rhizome.dot) 'escapable-characters)
                               (catch Exception e nil))]
    (if (= escape-chars "|{}\"")
      "\\n"
      "\n")))


(defn- hiccup-tree-viz
  "visualize instaparse hiccup output as a rhizome graph. Requires rhizome: https://github.com/ztellman/rhizome"
  [mytree options]
  (let [tree->image (resolve 'rhizome.viz/tree->image)]
    (tree->image sequential? rest mytree
                 :node->descriptor (fn [n] {:label (if (sequential? n)
                                                     (apply str (first n)
                                                            (when (span n)
                                                              [rhizome-newline (span n)]))
                                                     (with-out-str (pr n)))})
                 :options options)))

(defn- enlive-tree-viz
  "visualize enlive trees"
  [mytree options]
  (let [tree->image (resolve 'rhizome.viz/tree->image)]
    (tree->image (comp seq :content) :content mytree
                 :node->descriptor (fn [n]
                                     {:label (if (and (map? n) (:tag n))
                                               (apply str (:tag n)
                                                      (when (span n)
                                                        [rhizome-newline (span n)]))
                                               (with-out-str (pr n)))})
                 :options options)))

(defn tree-type
  [tree]
  (cond
    (and (map? tree) (:tag tree)) :enlive
    (and (vector? tree) (keyword? (first tree))) :hiccup
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
  "Creates a graphviz visualization of the parse tree.
   Optional keyword arguments:
   :output-file :buffered-image (return a java.awt.image.BufferedImage object)
   or
   :output-file output-file (will save the tree image to output-file)

   :options options (options passed along to rhizome)

Important: This function will only work if you have added rhizome
to your dependencies, and installed graphviz on your system.
See https://github.com/ztellman/rhizome for more information."
  [tree & {output-file :output-file options :options}]
  {:pre [(not= (tree-type tree) :invalid)]}
  (let [ttype (tree-type tree)]
    (if (= ttype :rootless)
      (tree-viz (fake-root tree) :output-file output-file :options options)
      (do
        (try
          (require 'rhizome.viz)
          (catch Exception e
            (throw (UnsupportedOperationException.
                     "\n\nVisualization of parse trees is only supported if you have rhizome among your project dependencies and graphviz installed on your computer.\n
          Visit https://github.com/ztellman/rhizome to find out the version info to put in your project.clj file and for links to the graphviz installer."))))
        (let [image
              (try
                (case (tree-type tree)
                  :enlive (enlive-tree-viz tree options)
                  (:hiccup :nil) (hiccup-tree-viz tree options))
                (catch IOException e
                  (throw (UnsupportedOperationException.
                           "\n\nYou appear to have rhizome in your dependencies, but have not installed GraphViz on your system.
  \nSee https://github.com/ztellman/rhizome for more information.\n"))))
              save-image (resolve 'rhizome.viz/save-image)
              view-image (resolve 'rhizome.viz/view-image)]
          (cond
            (= output-file :buffered-image) image
            output-file (save-image image output-file)
            :else (view-image image)))))))
(ns instaparse.viz-not-found
    "This file is a stub, so that a meaningful error will be returned if rhizome is not in your project's dependencies")

(defn view-tree [& args]
  (throw (UnsupportedOperationException. 
           "\n\nVisualization of parse trees is only supported if you have rhizome among your project dependencies and graphviz installed on your computer.\n
Visit https://github.com/ztellman/rhizome to find out the version info to put in your project.clj file and for links to the graphviz installer.")))

(defn tree->image [& args]
  (throw (UnsupportedOperationException. 
           "\n\nVisualization of parse trees is only supported if you have rhizome among your project dependencies and graphviz installed on your computer.\n
Visit https://github.com/ztellman/rhizome to find out the version info to put in your project.clj file and for links to the graphviz installer.")))

(defn view-image [& args]
  (throw (UnsupportedOperationException. 
           "\n\nVisualization of parse trees is only supported if you have rhizome among your project dependencies and graphviz installed on your computer.\n
Visit https://github.com/ztellman/rhizome to find out the version info to put in your project.clj file and for links to the graphviz installer.")))

(defn save-image [& args]
  (throw (UnsupportedOperationException. 
           "\n\nVisualization of parse trees is only supported if you have rhizome among your project dependencies and graphviz installed on your computer.\n
Visit https://github.com/ztellman/rhizome to find out the version info to put in your project.clj file and for links to the graphviz installer.")))
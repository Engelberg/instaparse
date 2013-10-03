(ns instaparse.gll-macros)

(def DEBUG false)
(def PRINT false)
(defmacro debug [& body]
  (when DEBUG
    `(do ~@body)))
(defmacro dprintln [& body]  
  (when PRINT `(println ~@body)))
(defmacro dpprint [& body]  
  (when PRINT `(println ~@body)))

(defmacro success [tramp node-key result end]
  `(instaparse.gll/push-result ~tramp ~node-key
                               (instaparse.gll/make-success ~result ~end)))

(ns instaparse.gll-macros)

(def PROFILE false)
(defmacro profile [& body]
  (when PROFILE
    `(do ~@body)))

(def PRINT false)
(defmacro dprintln [& body]  
  (when PRINT `(println ~@body)))
(defmacro dpprint [& body]  
  (when PRINT `(println ~@body)))

(defmacro success [tramp node-key result end]
  `(instaparse.gll/push-result ~tramp ~node-key
                               (instaparse.gll/make-success ~result ~end)))

(defmacro swap-field! [field-form f & args]
  `(set! ~field-form (~f ~field-form ~@args)))

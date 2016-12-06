(ns instaparse.macros)

(defmacro defclone [here there]
  (if (contains? &env :locals)
    ;; cljs
    `(def ~here ~there)
    ;; clj
    `(do 
       (def ~here ~there)
       (alter-meta! (var ~here) assoc
                    :doc (:doc (meta (var ~there)))
                    :arglists (:arglists (meta (var ~there)))
                    :file (:file (meta (var ~there)))
                    :line (:line (meta (var ~there)))
                    :column (:column (meta (var ~there))))
       (var ~here))))

(defmacro set-global-var!
  [v value]
  (if (contains? &env :locals)
    ;; cljs
    `(set! ~v ~value)
    ;; clj
    `(alter-var-root (var ~v) (constantly ~value))))

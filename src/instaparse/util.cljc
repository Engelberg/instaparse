(ns instaparse.util)

;; Both appear to be called with several strings as separate
;; arguments:
(defn throw-runtime-exception
  [& message]
  (let [^String text (apply str message)]
    (-> text
        #?(:clj RuntimeException.)
        throw)))

(defn throw-illegal-argument-exception
  [& message]
  (let [^String text (apply str message)]
    (-> text
        #?(:clj IllegalArgumentException.)
        throw)))

#?(:cljs
    (defn regexp-flags [re]
      (cond-> ""
        (.-ignoreCase re) (str "i")
        (.-multiline re) (str "m")
        (.-unicode re) (str "u"))))

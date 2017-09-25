(ns instaparse.util)

(defn throw-runtime-exception
  [& message]
  (-> (apply str message)
      #?(:clj RuntimeException.)
      throw))

(defn throw-illegal-argument-exception
  [& message]
  (-> (apply str message)
      #?(:clj IllegalArgumentException.)
      throw))

(defn regexp-flags [re]
  (cond-> ""
    (.-ignoreCase re) (str "i")
    (.-multiline re) (str "m")
    (.-unicode re) (str "u")))

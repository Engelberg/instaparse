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

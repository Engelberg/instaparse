(ns instaparse.incremental-vector-test
  (:use clojure.test instaparse.incremental-vector))

(defn rand-mutation [v iv]
  (case (rand-int 3)
    0 (let [n (rand-int 50000)] [(conj v n) (conj iv n)])
    1 [(pop v) (pop iv)]
    2 (let [i (rand-int (count v)), n (rand-int 50000)]
        [(assoc v i n) (assoc iv i n)])))

(deftest rand-incremental-vector-test
  (loop [v (vec (range 1000)) iv (ivec (range 1000)) n 1000]
    (let [[v iv] (rand-mutation v iv)]
      (when (pos? n)
        (is (= v iv))
        (is (= iv v))
        (is (= (hash v) (hash iv)))
        (is (= (seq v) (seq iv)))
        (recur v iv (dec n))))))
      
    
  
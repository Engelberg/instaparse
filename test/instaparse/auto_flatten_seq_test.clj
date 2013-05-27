(ns instaparse.auto-flatten-seq-test
  (:use clojure.test instaparse.auto-flatten-seq))

(defn rand-mutation [v iv]
  (let [rnd (int (rand-int 3))]
    (case rnd
      0 (let [n (rand-int 50000)] [(conj v n) (append-flat iv n) rnd])
      2 (let [i (rand-int 64), r (ivec (repeat i (rand-int 50000)))]
          [(into v r) (append-flat iv r) rnd])
      1 (let [i (rand-int 64), r (ivec (repeat i (rand-int 50000)))]
          [(into (vec (seq r)) v) (append-flat r iv) rnd]))))
        
(deftest rand-incremental-vector-test
  (is (= (append-flat (ivec [:s]) nil) [:s]))
  (loop [v (vec (range 100)) iv (ivec (range 100)) n 50 loops 20]
    (let [[v iv rnd] (rand-mutation v iv)]
      (cond
        (zero? loops) nil
        (zero? n) (recur (vec (range 100)) (ivec (range 100)) 50 (dec loops))
        :else
        (do
          (is (= (count v) (count iv)))
          (is (= v iv))
          (is (= iv v))        
          (is (= (hash v) (hash iv)))
          (is (= (seq v) (seq iv)))        
          (recur v iv (dec n) loops))))))

(defn depth [v]
  (cond
    (empty? v) 0
    (sequential? (first v)) (max (inc (depth (first v))) (depth (rest v)))
    :else (depth (rest v))))

(ns instaparse.auto-flatten-seq-test
  (:require [cemerick.cljs.test :as t]
            [instaparse.auto-flatten-seq :refer [auto-flatten-seq conj-flat
                                                 hash-conj hash-cat]])
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test 
                                                  run-tests testing)]))

(defn rand-mutation [v iv]
  (let [rnd (int (rand-int 3))]
    (case rnd
      0 (let [n (rand-int 50000)] [(conj v n) (conj-flat iv n) rnd])
      2 (let [i (rand-int 64), r (auto-flatten-seq (repeat i (rand-int 50000)))]
          [(into v r) (conj-flat iv r) rnd])
      1 (let [i (rand-int 64), r (auto-flatten-seq (repeat i (rand-int 50000)))]
          [(into (vec (seq r)) v) (conj-flat r iv) rnd]))))
        
(deftest rand-incremental-vector-test
  (is (= (conj-flat (auto-flatten-seq [:s]) nil) [:s]))
  (loop [v (vec (range 100)) iv (auto-flatten-seq (range 100)) n 50 loops 1]
    (let [[v iv rnd] (rand-mutation v iv)]
      (cond
        (zero? loops) nil
        (zero? n) (recur (vec (range 100)) (auto-flatten-seq (range 100)) 50 (dec loops))
        :else
        (do
          (is (= (count v) (count iv)))
          (is (= v iv))
          (is (= iv v))        
          (is (= (hash v) (hash iv)))
          (is (= (seq v) (seq iv)))        
          (recur v iv (dec n) loops))))))

(deftest hash-test
  (is (= (hash [1 2])
         (hash-conj (hash [1]) 2)))
  (is (= (hash [1 2])
         (hash-cat [1] [2])))
  (is (= (hash [1 2 3])
         (hash-cat [1] [2 3]))))

(defn check-equiv [v iv]
  (is (= (count v) (count iv)))
  (is (= (hash v) (hash iv)))
  (is (= v iv))
  (is (= iv v))
  (is (= (seq v) (seq iv))))

(deftest simple-afs-test
  (let [v (vec (range 10))
        iv (auto-flatten-seq (range 10))]
    (check-equiv v iv)
    (check-equiv (conj v 42)
                 (conj-flat iv 42))
    (check-equiv (conj v 42)
                 (conj-flat iv (auto-flatten-seq [42])))
    (check-equiv (concat v [1 2])
                 (conj-flat iv (auto-flatten-seq [1 2]))))) 

(defn depth [v]
  (cond
    (empty? v) 0
    (sequential? (first v)) (max (inc (depth (first v))) (depth (rest v)))
    :else (depth (rest v))))

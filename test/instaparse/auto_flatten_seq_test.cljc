(ns instaparse.auto-flatten-seq-test
  (:require
    [instaparse.auto-flatten-seq :refer [auto-flatten-seq conj-flat convert-afs-to-vec]]
    #?(:clj [clojure.test :refer [deftest are is]]
       :cljs [cljs.test]))
  #?(:cljs (:require-macros [cljs.test :refer [deftest are is]])))

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
  (loop [v (vec (range 100)) iv (auto-flatten-seq (range 100)) n 50 loops 20]
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
          (is (= v (convert-afs-to-vec iv)))
          (is (= (convert-afs-to-vec iv) v))
          (is (= (type (empty (convert-afs-to-vec iv))) (type v)))
          (is (= (hash v) (hash (convert-afs-to-vec iv))))
          (recur v iv (dec n) loops))))))

(defn depth [v]
  (cond
    (empty? v) 0
    (sequential? (first v)) (max (inc (depth (first v))) (depth (rest v)))
    :else (depth (rest v))))

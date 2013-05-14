(ns instaparse.auto-flatten-vector)

(def ^:const threshold 32)

(declare EMPTY)

(defn- expt [base pow]
  (if (zero? pow) 1
    (loop [n (int pow), y (int 1), z (int base)]
      (let [t (even? n), n (quot n 2)]
        (cond
          t (recur n y (unchecked-multiply-int z z))
          (zero? n) (unchecked-multiply-int z y)
          :else (recur n (unchecked-multiply-int z y) (unchecked-multiply-int z z)))))))

(def ^:const inverse-thirty-one -1108378657)

(defn- hash-conj [hash-v item]
  (unchecked-add-int (unchecked-multiply-int 31 hash-v) (hash item)))  

(defn- hash-pop [v top]
  (unchecked-multiply-int inverse-thirty-one
                          (unchecked-subtract-int (hash v) (hash top))))

(defn- hash-cat [v1 v2]
  (let [c (count v2)
        e (int (expt 31 c))]
    (unchecked-add-int
      (unchecked-multiply-int e (hash v1))
      (unchecked-subtract-int (hash v2) e))))

(deftype IncrementalVector [^clojure.lang.PersistentVector v ^int hashcode ^int cnt ^boolean dirty]
  Object
  (toString [self] (.toString v))
  (hashCode [self] hashcode)
  (equals [self other]
    (and (instance? IncrementalVector other)
         (== hashcode (.hashcode ^IncrementalVector other))
         (== (count v) (count (.v ^IncrementalVector other)))
         (= v (.v ^IncrementalVector other))))
  clojure.lang.IHashEq
  (hasheq [self] hashcode)
  java.util.Collection
  (iterator [self]
    (.iterator (seq self)))
  (size [self]
    cnt)
  clojure.lang.IPersistentCollection
  (equiv [self other]
    (and (== hashcode (hash other))
         (== cnt (count other))
         (= (seq self) other)))
  (empty [self] (with-meta EMPTY (meta self))) 
  clojure.lang.Counted
  (count [self] cnt)
  clojure.lang.IPersistentVector
  (assoc [self i val] 
    (throw (UnsupportedOperationException.)))
  (cons [self obj]
    (cond
      (empty? obj) self
      (vector? obj)
      (if (<= (count obj) threshold)
        (IncrementalVector. (into v obj) (hash-cat self obj) (+ (count obj) cnt)
                            (or dirty (:dirty obj)))
        (IncrementalVector. (conj v obj) (hash-cat self obj) (+ (count obj) cnt)
                            true))
      :else (IncrementalVector. (conj v obj) (hash-conj hashcode obj) (inc cnt) dirty)))
  clojure.lang.ILookup
  (valAt [self key]
    (when (= key :dirty) dirty))
  (valAt [self key not-found]
    (if (= key :dirty) dirty not-found))
  clojure.lang.IObj
  (withMeta [self metamap]
    (IncrementalVector. (with-meta v metamap) hashcode cnt dirty))
  clojure.lang.IMeta
  (meta [self]
    (meta v))
  clojure.lang.Seqable
  (seq [self]
    (flatten v))
  clojure.lang.IPersistentStack
  (peek [self] (peek v))
  (pop [self] 
    (let [top (peek v)]
      (cond
        (vector? top)
        (if (> (count top) 1)
          (let [new-top (pop top)]
            (IncrementalVector. (conj (pop v) new-top) (hash-pop self (peek top)) (dec cnt) dirty))
          (let [new-v (pop v)]
            (IncrementalVector. new-v (hash-pop self (peek top)) (dec cnt) dirty)))
        :else
        (let [new-v (pop v)]
          (IncrementalVector. new-v (hash-pop self top) (dec cnt) dirty))))))
     
(defn ivec [v]
  (let [v (vec v)]
    (IncrementalVector. v (hash v) (count v) false)))

(def EMPTY (ivec []))
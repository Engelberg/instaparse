(ns instaparse.incremental-vector
  (:require [clojure.core.protocols :refer [IKVReduce]]))

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

(defn- hash-pop [v]
  (let [top (peek v)]
    (unchecked-multiply-int inverse-thirty-one
                            (unchecked-subtract-int (hash v) (hash top)))))

(defn- hash-assoc [v i new]
  (let [old (get v i)
        c (count v)]
    (unchecked-add-int (hash v)
                       (unchecked-multiply-int 
                         (unchecked-subtract-int (hash new) (hash old))
                         (expt 31 (- c i 1))))))

(deftype IncrementalVector [^clojure.lang.PersistentVector v ^int hashcode]
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
    (.iterator v))
  (size [self]
    (count v))
  (toArray [self]
    (.toArray v))
  clojure.lang.IPersistentCollection
  (equiv [self other]
    (or 
      (and (instance? IncrementalVector other)
           (== hashcode (.hashcode ^IncrementalVector other))
           (== (count v) (count (.v ^IncrementalVector other)))
           (= v (.v ^IncrementalVector other)))
      (= v other)))
  (empty [self] (with-meta EMPTY (meta self))) 
  clojure.lang.Counted
  (count [self] (count v))
  clojure.lang.IPersistentVector
  (assoc [self i val] 
    (let [new-v (assoc v i val)]
      (IncrementalVector. new-v (hash-assoc self i val))))
  (assocN [self i val] 
    (let [new-v (assoc v i val)]
      (IncrementalVector. new-v (hash-assoc self i val))))
  (length [self] (count v))
  (cons [self obj]
    (IncrementalVector. (conj v obj) (hash-conj hashcode obj)))
  clojure.lang.IObj
  (withMeta [self metamap]
    (IncrementalVector. (with-meta v metamap) hashcode))
  clojure.lang.IMeta
  (meta [self]
    (meta v))
  clojure.lang.Seqable
  (seq [self]
    (seq v))
  clojure.lang.Sequential
  clojure.lang.ILookup
  (valAt [self key]
    (.valAt v key))
  (valAt [self key not-found]
    (.valAt v key not-found))
  clojure.lang.Indexed
  (nth [self i]
    (.nth v i))
  (nth [self i not-found]
    (.nth v i not-found))
  clojure.lang.IFn
  (invoke [self arg]
    (.invoke v arg))
  (applyTo [self arglist]
    (.applyTo v arglist))
  clojure.lang.Reversible
  (rseq [self]
    (rseq v))
  clojure.lang.IPersistentStack
  (peek [self] (peek v))
  (pop [self] 
    (let [new-v (pop v)]
      (IncrementalVector. new-v (hash-pop self))))
  clojure.lang.Associative
  (containsKey [self k]
    (.containsKey v k))
  (entryAt [self k]
    (.entryAt v k))
  IKVReduce
  (kv-reduce [self f init]
    (reduce-kv f init v))
  java.lang.Comparable
  (compareTo [this that]
    (.compareTo v that))
   java.util.List
  (get [this i] (.nth v i))
  (indexOf [this o] (.indexOf v o))
  (lastIndexOf [this o] (.lastIndexOf v o))
  (listIterator [this]
    (.listIterator v 0))
  (listIterator [this i]
    (.listIterator v i))
  (subList [this a z]
    (.subList v a z))
  ) 
  
  
(defn ivec [v]
  (let [v (vec v)]
    (IncrementalVector. v (hash v))))

(def EMPTY (ivec []))
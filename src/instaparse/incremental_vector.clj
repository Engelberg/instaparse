(ns instaparse.incremental-vector)

(declare EMPTY)

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
  (assocN [self i val] 
    (let [new-v (assoc v i val)]
      (IncrementalVector. new-v (hash new-v))))
  (cons [self obj]
    (IncrementalVector. (conj v obj) (hash-combine hashcode obj)))
  clojure.lang.IObj
  (withMeta [self metamap]
    (IncrementalVector. (with-meta v metamap) hashcode))
  clojure.lang.IMeta
  (meta [self]
    (meta v))
  clojure.lang.Seqable
  (seq [self]
    (seq v))
  clojure.lang.ILookup
  (valAt [self key]
    (.valAt v key))
  (valAt [self key not-found]
    (.valAt v key not-found))
  clojure.lang.IFn
  (invoke [self arg]
    (.invoke v arg))
  clojure.lang.IPersistentStack
  (peek [self] (peek v))
  (pop [self] 
    (let [new-v (pop v)]
      (IncrementalVector. new-v (hash new-v))))) 
  
  
(defn ivec [v]
  (let [v (vec v)]
    (IncrementalVector. v (hash v))))

(def EMPTY (ivec []))
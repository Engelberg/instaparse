(ns instaparse.incremental-vector)

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

(deftype AutoFlattenSeq [^clojure.lang.PersistentVector v ^int hashcode]
  Object
  (toString [self] (.toString v))
  (hashCode [self] hashcode)
  (equals [self other]
    (and (instance? AutoFlattenSeq other)
         (== hashcode (.hashcode ^AutoFlattenSeq other))
         (== (count v) (count (.v ^AutoFlattenSeq other)))
         (= v (.v ^AutoFlattenSeq other))))
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
      (and (instance? AutoFlattenSeq other)
           (== hashcode (.hashcode ^AutoFlattenSeq other))
           (== (count v) (count (.v ^AutoFlattenSeq other)))
           (= v (.v ^AutoFlattenSeq other)))
      (= v other)))
  (empty [self] (with-meta EMPTY (meta self))) 
  clojure.lang.Counted
  (count [self] (count v))
  clojure.lang.IPersistentVector
  (assoc [self i val] 
    (let [new-v (assoc v i val)]
      (AutoFlattenSeq. new-v (hash-assoc self i val))))
  (cons [self obj]
    (AutoFlattenSeq. (conj v obj) (hash-conj hashcode obj)))
  clojure.lang.IObj
  (withMeta [self metamap]
    (AutoFlattenSeq. (with-meta v metamap) hashcode))
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
  clojure.lang.IPersistentStack
  (peek [self] (peek v))
  (pop [self] 
    (let [new-v (pop v)]
      (AutoFlattenSeq. new-v (hash-pop self))))) 
  
  
(defn ivec [v]
  (let [v (vec v)]
    (AutoFlattenSeq. v (hash v))))

(def EMPTY (ivec []))
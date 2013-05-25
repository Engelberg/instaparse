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

(declare iv?)

(defn delve [v index]
  (if (vector? (get-in v index))
    (recur v (conj index 0))
    index))

(declare true-count)

(defn advance [v index]
  (cond
    (= (true-count index) 1)
    (when (< (peek index) (dec (true-count v)))
      (delve v [(inc (peek index))]))
    
    (< (peek index) (dec (true-count (get-in v (pop index)))))
    (delve v (conj (pop index) (inc (peek index))))
    
    :else
    (recur v (pop index))))
      
(defn flat-seq
  ([v] (if (pos? (count v)) 
         (flat-seq v (delve v [0]))
         nil))
  ([v index]
    (lazy-seq
      (cons (get-in v index) 
            (when-let [next-index (advance v index)] 
              (flat-seq v next-index))))))  

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
    (if-let [s (seq self)]
      (.iterator (seq self))
      (.iterator [])))
  (size [self]
    cnt)
  (toArray [self]
    (.toArray v))
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
      (and (sequential? obj) (empty? obj)) self
      (vector? obj)
      (if (<= (count obj) threshold)
        (IncrementalVector. (into v obj) (hash-cat self obj) (+ (count obj) cnt)
                            (or dirty (and (instance? IncrementalVector obj)
                                           (.dirty ^IncrementalVector obj))))
        (IncrementalVector. (conj v obj) (hash-cat self obj) (+ (count obj) cnt)
                            true))
      :else (IncrementalVector. (conj v obj) (hash-conj hashcode obj) (inc cnt) dirty)))  
  clojure.lang.Indexed
  (nth [self i]
    (.nth v i))
  (nth [self i not-found]
    (.nth v i not-found))
  clojure.lang.ILookup
  (valAt [self key]    
    (.valAt v key))
  (valAt [self key not-found]
    (.valAt v key not-found))
  clojure.lang.IObj
  (withMeta [self metamap]
    (IncrementalVector. (with-meta v metamap) hashcode cnt dirty))
  clojure.lang.IMeta
  (meta [self]
    (meta v))
  clojure.lang.Seqable
  (seq [self]
    (if dirty (flat-seq v) (seq v)))
  clojure.lang.IFn
  (invoke [self arg]
    (get self arg))
  (applyTo [self arglist]
    (apply get self arglist))
  clojure.lang.IPersistentStack
  (peek [self]
    (let [top (peek v)]
      (if (vector? top) (peek top) top)))
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

(defn iv? [s]
  (instance? IncrementalVector s))

(defn true-count [v]
  (if (iv? v)
    (count (.v ^IncrementalVector v))
    (count v)))


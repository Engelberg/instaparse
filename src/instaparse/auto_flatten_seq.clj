(ns instaparse.auto-flatten-seq
  (:import clojure.lang.PersistentVector)
  (:require [clojure.core.protocols :refer [IKVReduce]]))

(def ^:const threshold 32)
(def ^:const inverse-thirty-one -1108378657)

(defprotocol ConjFlat
  (conj-flat [self obj])
  (cached? [self]))

; For incremental hashing, need to store the hash prior to mixing step
(defprotocol PremixHash
  (premix-hash [self]))

; Need a backwards compatible version of mix-collection-hash
(defmacro compile-if [test then else]
  (if (eval test)
    then
    else))

(defmacro mix-collection-hash-bc [x y] ;backwards-compatible
  `(compile-if (resolve 'clojure.core/mix-collection-hash)
               (mix-collection-hash ~x ~y)
               ~x))

(declare EMPTY)

(defn- expt [base pow]
  (if (zero? pow) 1
    (loop [n (int pow), y (int 1), z (int base)]
      (let [t (even? n), n (quot n 2)]
        (cond
          t (recur n y (unchecked-multiply-int z z))
          (zero? n) (unchecked-multiply-int z y)
          :else (recur n (unchecked-multiply-int z y) (unchecked-multiply-int z z)))))))

(defmacro hash-conj [premix-hash-v item]
  `(unchecked-add-int (unchecked-multiply-int 31 ~premix-hash-v) (hash ~item)))  

(defmacro hash-pop [v top]
  `(unchecked-multiply-int inverse-thirty-one
                           (unchecked-subtract-int (premix-hash ~v) (hash ~top))))

(defn- hash-cat ^long [v1 v2]
  (let [c (count v2)
        e (int (expt 31 c))]
    (unchecked-add-int
      (unchecked-multiply-int e (premix-hash v1))
      (unchecked-subtract-int (premix-hash v2) e))))

(declare afs?)

(defn delve [v index]
  (loop [v (get-in v index)
         index index]
    (if (afs? v)
      (recur (get v 0) (conj index 0))
      index)))

(declare true-count)

(defn advance [v index]
  (cond
    (= (count index) 1)
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

(deftype AutoFlattenSeq [^PersistentVector v ^int premix-hashcode ^int hashcode
                         ^int cnt ^boolean dirty
                         ^:unsynchronized-mutable ^clojure.lang.ISeq cached-seq]
  Object
  (toString [self] (.toString (seq self)))
  (hashCode [self] hashcode)
  (equals [self other]
    (and (instance? AutoFlattenSeq other)
         (== hashcode (.hashcode ^AutoFlattenSeq other))
         (== cnt (.cnt ^AutoFlattenSeq other))
         (= dirty (.dirty ^AutoFlattenSeq other))
         (= v (.v ^AutoFlattenSeq other))))
  clojure.lang.IHashEq
  (hasheq [self] hashcode)
  PremixHash
  (premix-hash [self] premix-hashcode)
  java.util.Collection
  (iterator [self]
    (if-let [^java.util.Collection s (seq self)]
      (.iterator s)
      (let [^java.util.Collection e ()]
        (.iterator e))))
  (size [self]
    cnt)
  (toArray [self]
    (let [^java.util.Collection s (seq self)]
      (.toArray s)))
  clojure.lang.Sequential
  clojure.lang.ISeq
  (equiv [self other]
    (and (== hashcode (hash other))
         (== cnt (count other))
         (or (== cnt 0)
             (= (seq self) other))))
  (empty [self] (with-meta EMPTY (meta self))) 
  (first [self] (first (seq self)))
  (next [self] (next (seq self)))
  (more [self] (rest (seq self)))
  (cons [self obj]
    (cons obj self))
  ConjFlat
  (conj-flat [self obj]
    (cond
      (nil? obj) self
      (afs? obj)
      (cond
        (zero? cnt) obj
        (<= (count obj) threshold)
        (let [phc (hash-cat self obj)
              new-cnt (+ cnt (count obj))]
          (AutoFlattenSeq. (into v obj) phc (mix-collection-hash-bc phc new-cnt) new-cnt
                           (or dirty (.dirty ^AutoFlattenSeq obj)) nil))
        :else
        (let [phc (hash-cat self obj)
              new-cnt (+ cnt (count obj))]
          (AutoFlattenSeq. (conj v obj) phc (mix-collection-hash-bc phc new-cnt) new-cnt
                           true nil)))
      :else 
      (let [phc (hash-conj premix-hashcode obj)
            new-cnt (inc cnt)]
        (AutoFlattenSeq. (conj v obj) phc (mix-collection-hash-bc phc new-cnt) new-cnt dirty nil))))
  (cached? [self] cached-seq)
  clojure.lang.Counted
  (count [self] cnt)
  clojure.lang.ILookup
  (valAt [self key]    
    (.valAt v key))
  (valAt [self key not-found]
    (.valAt v key not-found))
  clojure.lang.IObj
  (withMeta [self metamap]
    (AutoFlattenSeq. (with-meta v metamap) premix-hashcode hashcode cnt dirty nil))
  clojure.lang.IMeta
  (meta [self]
    (meta v))
  clojure.lang.Seqable
  (seq [self]
    (if cached-seq cached-seq
      (do
        (set! cached-seq (if dirty (flat-seq v) (seq v)))
        cached-seq))))
     
(defn hash-ordered-coll-without-mix [v]
  (compile-if (resolve 'clojure.core/mix-collection-hash)
              (reduce (fn [acc e] (unchecked-add-int
                                    (unchecked-multiply-int 31 acc)
                                    (hash e)))
                      1
                      v)
              (hash v)))

(defn auto-flatten-seq [v]
  (let [v (vec v)]
    (AutoFlattenSeq. v (hash-ordered-coll-without-mix v) (hash v) (count v) false nil)))

(def EMPTY (auto-flatten-seq []))

(defn afs? [s]
  (instance? AutoFlattenSeq s))

(defn true-count [v]
  (if (afs? v)
    (count (.v ^AutoFlattenSeq v))
    (count v)))

;; For hiccup format, we need to be able to convert the seq to a vector.

(defn flat-vec-helper [acc v]
  (if-let [s (seq v)]
    (let [fst (first v)]
      (if (afs? fst) 
        (recur (flat-vec-helper acc fst) (next v))
        (recur (conj! acc fst) (next v))))
    acc))

(defn flat-vec
  "Turns deep vector (like the vector inside of FlattenOnDemandVector) into a flat vec"
  [v]
  (persistent! (flat-vec-helper (transient []) v)))

(defprotocol GetVec
  (^PersistentVector get-vec [self]))

(deftype FlattenOnDemandVector [v   ; ref containing PersistentVector or nil 
                                ^int hashcode
                                ^int cnt
                                flat] ; ref containing PersistentVector or nil                                
  GetVec
  (get-vec [self] 
           (when (not @flat)             
             (dosync
               (when (not @flat)
                 (ref-set flat (with-meta (flat-vec @v) (meta @v))) 
                 (ref-set v nil)))) ; clear out v so it can be garbage collected
           @flat)
                    
  Object
  (toString [self] (.toString (get-vec self)))
  (hashCode [self] hashcode)
  (equals [self other]
    (and (instance? FlattenOnDemandVector other)
         (== hashcode (.hashcode ^FlattenOnDemandVector other))
         (== cnt (.cnt ^FlattenOnDemandVector other))
         (= v (.v ^FlattenOnDemandVector other))
         (= flat (.flat ^FlattenOnDemandVector other))))
  clojure.lang.IHashEq
  (hasheq [self] hashcode)
  java.util.Collection
  (iterator [self]
    (.iterator (get-vec self)))
  (size [self]
    cnt)
  (toArray [self]
    (.toArray (get-vec self)))
  clojure.lang.IPersistentCollection
  (equiv [self other]
    (or 
      (and (== hashcode (hash other))
           (== cnt (count other))
           (= (get-vec self) other))))
  (empty [self] (with-meta EMPTY (meta self))) 
  clojure.lang.Counted
  (count [self] cnt)
  clojure.lang.IPersistentVector
  (assoc [self i val]
    (assoc (get-vec self) i val))
  (assocN [self i val]
    (.assocN (get-vec self) i val))
  (length [self]
    cnt)
  (cons [self obj]
    (conj (get-vec self) obj))
  clojure.lang.IObj
  (withMeta [self metamap]    
    (if @flat
      (FlattenOnDemandVector. (ref @v) hashcode cnt (ref (with-meta @flat metamap)))
      (FlattenOnDemandVector. (ref (with-meta @v metamap)) hashcode cnt (ref @flat))))
  clojure.lang.IMeta
  (meta [self]
    (if @flat (meta @flat) (meta @v)))
  clojure.lang.Seqable
  (seq [self]
    (seq (get-vec self)))
  clojure.lang.ILookup
  (valAt [self key]
    (.valAt (get-vec self) key))
  (valAt [self key not-found]
    (.valAt (get-vec self) key not-found))
  clojure.lang.Indexed
  (nth [self i]
    (.nth (get-vec self) i))
  (nth [self i not-found]
    (.nth (get-vec self) i not-found))
  clojure.lang.IFn
  (invoke [self arg]
    (.invoke (get-vec self) arg))
  (applyTo [self arglist]
    (.applyTo (get-vec self) arglist))
  clojure.lang.Reversible
  (rseq [self]
    (if (pos? cnt)
      (rseq (get-vec self))
      nil))
  clojure.lang.IPersistentStack
  (peek [self] 
    (peek (get-vec self)))
  (pop [self] 
    (pop (get-vec self)))
  clojure.lang.Associative
  (containsKey [self k]
    (.containsKey (get-vec self) k))
  (entryAt [self k]
    (.entryAt (get-vec self) k))
  IKVReduce
  (kv-reduce [self f init]
    (.kvreduce (get-vec self) f init))
  java.lang.Comparable
  (compareTo [self that]
    (.compareTo (get-vec self) that))
  java.util.List
  (get [self i] (nth (get-vec self) i))
  (indexOf [self o] (.indexOf (get-vec self) o))
  (lastIndexOf [self o] (.lastIndexOf (get-vec self) o))
  (listIterator [self]
    (.listIterator (get-vec self) 0))
  (listIterator [self i]
    (.listIterator (get-vec self) i))
  (subList [self a z]
    (.subList (get-vec self) a z))
  )

(defn convert-afs-to-vec [^AutoFlattenSeq afs]
  (cond
    (.dirty afs) 
    (if (cached? afs)
      (vec (seq afs))
      (FlattenOnDemandVector. (ref (.v afs))
                              (.hashcode afs)
                              (.cnt afs)
                              (ref nil)))
    :else
    (.v afs)))
    


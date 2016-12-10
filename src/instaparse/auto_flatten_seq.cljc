(ns instaparse.auto-flatten-seq
  #?(:clj (:import clojure.lang.PersistentVector))
  #?(:clj (:require [clojure.core.protocols :refer [IKVReduce]])))

(def ^:const threshold 32)

(defprotocol ConjFlat
  (conj-flat [self obj])
  (cached? [self]))

; Need a backwards compatible version of mix-collection-hash
#?(:clj (defmacro compile-if [test then else]
          (if (eval test)
            then
            else)))

#?(:clj (defmacro mix-collection-hash-bc [x y]
          ;; backwards-compatible
          `(compile-if (resolve 'clojure.core/mix-collection-hash)
                       (mix-collection-hash ~x ~y)
                       ~x)))

(declare EMPTY hash-cat afs? true-count)

#?(:clj
   (defmacro hash-conj [premix-hash-v item]
     `(unchecked-add-int (unchecked-multiply-int 31 ~premix-hash-v) (hash ~item)))
   :cljs
   (defn ^number hash-conj
     "Returns the hash code, consistent with =, for an external ordered
  collection implementing Iterable.
  See http://clojure.org/data_structures#hash for full algorithms."
     [unmixed-hash item]
     (+ (imul 31 unmixed-hash) (hash item))))

#?(:clj
   (defn- expt [base pow]
     (if (zero? pow)
       1
       (loop [n (int pow), y (int 1), z (int base)]
         (let [t (even? n), n (quot n 2)]
           (cond
             t (recur n y (unchecked-multiply-int z z))
             (zero? n) (unchecked-multiply-int z y)
             :else (recur n (unchecked-multiply-int z y) (unchecked-multiply-int z z)))))))
   :cljs
   (defn- expt [base pow]
     (if (zero? pow)
       1
       (loop [n (int pow), y (int 1), z (int base)]
         (let [t (even? n), n (quot n 2)]
           (cond
             t (recur n y (imul z z))
             (zero? n) (imul z y)
             :else (recur n (imul z y) (imul z z))))))))

(defn delve [v index]
  (loop [v (get-in v index)
         index index]
    (if (afs? v)
      (recur (get v 0) (conj index 0))
      index)))

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

#?(:clj
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
:cljs
(deftype AutoFlattenSeq [^PersistentVector v ^number premix-hashcode ^number hashcode ^number cnt ^boolean dirty
                         ^:unsynchronized-mutable ^ISeq cached-seq]
  Object
  (toString [self] (pr-str* (seq self)))
  IHash
  (-hash [self] hashcode)
  ISequential
  ISeq
  (-first [self] (first (seq self)))
  (-rest [self] (rest (seq self)))
  IEquiv
  (-equiv [self other]
    (and ;(instance? AutoFlattenSeq other)
         (= hashcode (hash other))
         (= cnt (count other))
         (or (= cnt 0)
             (= (seq self) other))))
  ICollection
  (-conj [self o] (cons o self))
  IEmptyableCollection
  (-empty [self] (with-meta EMPTY (meta self))) 
  INext
  (-next [self] (next (seq self)))
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
          (AutoFlattenSeq. (into v obj) phc (mix-collection-hash phc new-cnt) new-cnt
                           (or dirty (.-dirty ^AutoFlattenSeq obj)) nil))
        :else
        (let [phc (hash-cat self obj)
              new-cnt (+ cnt (count obj))]
          (AutoFlattenSeq. (conj v obj) phc (mix-collection-hash phc new-cnt) new-cnt
                           true nil)))
      :else
      (let [phc (hash-conj premix-hashcode obj)
            new-cnt (inc cnt)]
        (AutoFlattenSeq. (conj v obj) phc (mix-collection-hash phc new-cnt) new-cnt dirty nil))))
  (cached? [self] cached-seq)
  ICounted
  (-count [self] cnt)
  ILookup
  (-lookup [self key]
    (-lookup v key))
  (-lookup [self key not-found]
    (-lookup v key not-found))
  IWithMeta
  (-with-meta [self metamap]
    (AutoFlattenSeq. (with-meta v metamap) premix-hashcode hashcode cnt dirty nil))
  IMeta
  (-meta [self]
    (meta v))
  ISeqable
  (-seq [self]
    (if cached-seq cached-seq
      (do
        (set! cached-seq (if dirty (flat-seq v) (seq v)))
        cached-seq)))))

#?(:clj
   (defn- hash-cat ^long [^AutoFlattenSeq v1 ^AutoFlattenSeq v2]
     (let [c (count v2)
           e (int (expt 31 c))]
       (unchecked-add-int
        (unchecked-multiply-int e (.premix-hashcode v1))
        (unchecked-subtract-int (.premix-hashcode v2) e))))
   :cljs
   (defn- hash-cat ^number [^AutoFlattenSeq v1 ^AutoFlattenSeq v2]
     (let [c (count v2)
           e (int (expt 31 c))]
       (+ (imul e (.-premix-hashcode v1))
          (- (.-premix-hashcode v2) e)))))

#?(:clj
   (defn hash-ordered-coll-without-mix ^long [v]
     (compile-if (resolve 'clojure.core/mix-collection-hash)
       (let [thirty-one (int 31)
             cnt (count v)]
         (loop [acc (int 1) i (int 0)]
           (if (< i cnt)
             (recur (unchecked-add-int
                     (unchecked-multiply-int thirty-one acc)
                     (hash (v i)))
                    (inc i))
             acc)))
       (hash v)))
   :cljs
   (defn ^number hash-ordered-coll-without-mix
     "Returns the partially calculated hash code, still requires a call to mix-collection-hash"
     ([coll]
      (hash-ordered-coll-without-mix 1 coll))
     ([existing-unmixed-hash coll]
      (loop [unmixed-hash existing-unmixed-hash
             coll (seq coll)]
        (if-not (nil? coll)
          (recur (bit-or (+ (imul 31 unmixed-hash) (hash (first coll))) 0) 
                 (next coll))
          unmixed-hash)))))

#?(:cljs
   (extend-protocol IPrintWithWriter
     instaparse.auto-flatten-seq/AutoFlattenSeq
     (-pr-writer [afs writer opts]
       (-pr-writer (seq afs) writer opts))))

(defn auto-flatten-seq [v]
  (let [v (vec v)]
    (AutoFlattenSeq. v
                     (hash-ordered-coll-without-mix v)
                     (hash v) (count v)
                     false nil)))

(def EMPTY (auto-flatten-seq []))

(defn afs? [s]
  (instance? AutoFlattenSeq s))

(defn true-count [v]
  (if (afs? v)
    (count (.-v ^AutoFlattenSeq v))
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

#?(:clj
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
  (empty [self] (with-meta [] (meta self))) 
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
:cljs
(deftype FlattenOnDemandVector [v   ; atom containing PersistentVector or nil 
                                ^number hashcode
                                ^number cnt
                                flat] ; atom containing PersistentVector or nil
  GetVec
  (get-vec [self] 
    (when (not @flat)             
      (swap! flat (fn [_] (with-meta (flat-vec @v) (meta @v))))
      (swap! v (fn [_] nil))) ; clear out v so it can be garbage collected 
    @flat)
  
  Object
  (toString [self]
    (pr-str* (get-vec self)))
  IHash
  (-hash [self] hashcode)
  IEquiv
  (-equiv [self other]
    (or 
     (and (= hashcode (hash other))
          (= cnt (count other))
          (= (get-vec self) other))))
  IEmptyableCollection
  (-empty [self] (with-meta [] (meta self))) 
  ICounted
  (-count [self] cnt)
  IVector
  (-assoc-n [self i val]
    (-assoc-n (get-vec self) i val))
  ICollection
  (-conj [self obj]
    (conj (get-vec self) obj))
  IWithMeta
  (-with-meta [self metamap]    
    (if @flat
      (FlattenOnDemandVector. (atom @v) hashcode cnt (atom (with-meta @flat metamap)))
      (FlattenOnDemandVector. (atom (with-meta @v metamap)) hashcode cnt (atom @flat))))
  IMeta
  (-meta [self]
    (if @flat (meta @flat) (meta @v)))
  ISequential
  ISeqable
  (-seq [self]
    (seq (get-vec self)))
  ILookup
  (-lookup [self key]
    (-lookup (get-vec self) key))
  (-lookup [self key not-found]
    (-lookup (get-vec self) key not-found))
  IIndexed
  (-nth [self i]
    (-nth (get-vec self) i))
  (-nth [self i not-found]
    (-nth (get-vec self) i not-found))
  IFn
  (-invoke [self arg]
    (-invoke (get-vec self) arg))
  (-invoke [self arg not-found]
    (-invoke (get-vec self) arg not-found))
  IReversible
  (-rseq [self]
    (if (pos? cnt)
      (rseq (get-vec self))
      nil))
  IStack
  (-peek [self] 
    (-peek (get-vec self)))
  (-pop [self] 
    (-pop (get-vec self)))
  IAssociative
  (-assoc [self i val]
    (assoc (get-vec self) i val))
  (-contains-key? [self k]
    (-contains-key? (get-vec self) k))
  IKVReduce
  (-kv-reduce [self f init]
    (-kv-reduce (get-vec self) f init))
  IComparable
  (-compare [self that]
    (-compare (get-vec self) that))
  ))

#?(:cljs
   (extend-protocol IPrintWithWriter
     instaparse.auto-flatten-seq/FlattenOnDemandVector
     (-pr-writer [v writer opts]
       (-pr-writer (get-vec v) writer opts))))

(defn convert-afs-to-vec [^AutoFlattenSeq afs]
  (cond
    (.-dirty afs)
    (if (cached? afs)
      (vec (seq afs))
      #?(:clj
         (FlattenOnDemandVector.
          (ref (.-v afs))
          (.-hashcode afs)
          (.-cnt afs)
          (ref nil))
         :cljs
         (FlattenOnDemandVector.
          (atom (.-v afs))
          (.-hashcode afs)
          (.-cnt afs)
          (atom nil))))
    :else
    (.-v afs)))

(ns instaparse.auto-flatten-seq
  (:import cljs.core.PersistentVector))

(def ^:const threshold 32)

(declare EMPTY)

(defn- hash-conj [hash-v item]
  (hash-combine hash-v (hash item)))  

;; The clojurscript vector hash algorithm is different from the clojure one, 
;; with no obvious way to optimize this operation. 
(defn- hash-cat [v1 v2]
  (hash (concat v1 v2)))

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

(defprotocol ConjFlat
  (conj-flat [self obj])
  (cached? [self]))

(deftype AutoFlattenSeq [^PersistentVector v ^int hashcode ^int cnt ^boolean dirty
                         ^:unsynchronized-mutable ^ISeq cached-seq]
  IHash
  (-hash [self] hashcode)
  ISequential
  ISeq
  (-first [self] (first (seq self)))
  (-rest [self] (rest (seq self)))
  ;; (cons [self obj]
  ;;   (cons obj self))
  IEquiv
  (-equiv [self other]
    (and (== hashcode (hash other))
         (== cnt (count other))
         (or (== cnt 0)
             (= (seq self) other))))
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
        (AutoFlattenSeq. (into v obj) (hash-cat self obj) (+ (count obj) cnt)
                            (or dirty (.-dirty ^AutoFlattenSeq obj)) nil)
        :else
        (AutoFlattenSeq. (conj v obj) (hash-cat self obj) (+ (count obj) cnt)
                            true nil))
      :else (AutoFlattenSeq. (conj v obj) (hash-conj hashcode obj) (inc cnt) dirty nil)))
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
    (AutoFlattenSeq. (with-meta v metamap) hashcode cnt dirty nil))
  IMeta
  (-meta [self]
    (meta v))
  ISeqable
  (-seq [self]
    (if cached-seq cached-seq
      (do
        (set! cached-seq (if dirty (flat-seq v) (seq v)))
        cached-seq))))
     
(defn auto-flatten-seq [v]
  (let [v (vec v)]
    (AutoFlattenSeq. v (hash v) (count v) false nil)))

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

(deftype FlattenOnDemandVector [v   ; atom containing PersistentVector or nil 
                                ^int hashcode
                                ^int cnt
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
  ;; (hashCode [self] hashcode)
  ;; (equals [self other]
  ;;   (and (instance? FlattenOnDemandVector other)
  ;;        (== hashcode (.hashcode ^FlattenOnDemandVector other))
  ;;        (== cnt (.cnt ^FlattenOnDemandVector other))
  ;;        (= v (.v ^FlattenOnDemandVector other))
  ;;        (= flat (.flat ^FlattenOnDemandVector other))))
  IHash
  (-hash [self] hashcode)
  IEquiv
  (-equiv [self other]
    (and (instance? FlattenOnDemandVector other)
         (== hashcode (.-hashcode ^FlattenOnDemandVector other))
         (== cnt (.-cnt ^FlattenOnDemandVector other))
         (= v (.-v ^FlattenOnDemandVector other))
         (= flat (.-flat ^FlattenOnDemandVector other))))
  IEmptyableCollection
  (-empty [self] (with-meta EMPTY (meta self))) 
  ICounted
  (-count [self] cnt)
  IAssociative
  (-assoc [self i val]
    (assoc (get-vec self) i val))
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
  (-contains-key? [self k]
    (-contains-key? (get-vec self) k))
  IKVReduce
  (-kv-reduce [self f init]
    (-kv-reduce (get-vec self) f init))
  IComparable
  (-compare [self that]
    (-compare (get-vec self) that))
  )

(defn convert-afs-to-vec [^AutoFlattenSeq afs]
  (cond
    (.-dirty afs) 
    (if (cached? afs)
      (vec (seq afs))
      (FlattenOnDemandVector. (atom (.-v afs))
                              (.-hashcode afs)
                              (.-cnt afs)
                              (atom nil)))
    :else
    (.-v afs)))

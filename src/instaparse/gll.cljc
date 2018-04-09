(ns instaparse.gll
  "The heart of the parsing mechanism.  Contains the trampoline structure,
   the parsing dispatch function, the nodes where listeners are stored,
   the different types of listeners, and the loop for executing the various
   listeners and parse commands that are on the stack."

  (:require
    ;; Incremental vector provides a more performant hashing strategy 
    ;; for this use-case for vectors
    ;; We use the auto flatten version
    [instaparse.auto-flatten-seq :as afs]

    ;; failure contains the augment-failure function, which is called to
    ;; add enough information to the failure object for pretty printing 
    [instaparse.failure :as fail]

    ;; reduction contains code relating to reductions and flattening.
    [instaparse.reduction :as red]

    ;; Two of the public combinators are needed.
    [instaparse.combinators-source :refer [Epsilon nt]]

    ;; Need a way to convert parsers into strings for printing and error messages.
    [instaparse.print :as print]

    ;; Utility to preserve RegExp flags
    #?(:cljs
       [instaparse.util :refer [regexp-flags]])

    ;; Unicode utilities for char-range
    #?(:cljs
       [goog.i18n.uChar :as u]))

  #?(:cljs
     (:use-macros
       [instaparse.gll :only
        [log profile dprintln dpprint success
         attach-diagnostic-meta trace-or-false]])))

;; As of Java 7, strings no longer have fast substring operation,
;; so we use Segments instead, which implement the CharSequence
;; interface with a fast subSequence operation.  Fortunately,
;; Java regular expressions work on anything that adheres
;; to the CharSequence interface.  There is a built-in class
;; javax.swing.text.Segment which does the trick, but
;; this class is not available on Google App Engine.  So
;; to support the use of instaparse on Google App Engine,
;; we simply create our own Segment type.

#?(:clj
   (deftype Segment [^CharSequence s ^int offset ^int count]
     CharSequence
     (length [this] count)
     (subSequence [this start end]
       (Segment. s (+ offset start) (- end start)))
     (charAt [this index]
       (.charAt s (+ offset index)))
     (toString [this]
       (.toString (doto (StringBuilder. count)
                    (.append s offset (+ offset count)))))))

;;;;; SETUP DIAGNOSTIC MACROS AND VARS
#?(:clj (do

(defonce PRINT false)
(defmacro dprintln [& body]  
  (when PRINT `(println ~@body)))
(defmacro dpprint [& body]  
  (when PRINT `(clojure.pprint/pprint ~@body)))

(defonce PROFILE false)
(defmacro profile [& body]
  (when PROFILE
    `(do ~@body)))

;; By default TRACE is set to false, and all these macros are used
;; throughout the code to ensure there is absolutely no performance
;; penalty from the tracing code.  Everything related to tracing
;; is compiled away.
;;
;; We recompile this file with TRACE set to true to activate the
;; tracing code.
;;
;; bind-trace is the one exception where we can't completely compile
;; the new code away, because it is used in instaparse.core, which won't be
;; recompiled.  Still, binding is a relatively slow operation, so by testing
;; whether TRACE is true inside the expansion, we can at least avoid 
;; the performance hit of binding every time.

(defonce TRACE false)
(def ^:dynamic *trace* false)
(defmacro log [tramp & body]
  (when TRACE
    `(when (:trace? ~tramp) (println ~@body))))
(defmacro attach-diagnostic-meta [f metadata]
  (if TRACE
    `(with-meta ~f ~metadata)
    f))
(defmacro bind-trace [trace? body]
  `(if TRACE
     (binding [*trace* ~trace?] ~body)
          ~body))
(defmacro trace-or-false []
  (if TRACE '*trace* false))

))

; In diagnostic messages, how many characters ahead do we want to show.
(def ^:dynamic *diagnostic-char-lookahead* 10)

(declare sub-sequence string-context)

#?(:clj
   (defn string-context [^CharSequence text index]
     (let [end (+ index *diagnostic-char-lookahead*),
           length (.length text)]
       (if (< length end)
         (str (sub-sequence text index))
         (str (sub-sequence text index end) "...")))))

(profile (def stats (atom {})))
(profile (defn add! [call] (swap! stats update-in [call] (fnil inc 0))))
(profile (defn clear! [] (reset! stats {})))

;; Now we can get down to parsing

(defn get-parser [grammar p]
  (get grammar p p))

(declare alt-parse cat-parse string-parse epsilon-parse non-terminal-parse
         opt-parse plus-parse star-parse regexp-parse lookahead-parse
         rep-parse negative-lookahead-parse ordered-alt-parse
         string-case-insensitive-parse char-range-parse)
(defn -parse [parser index tramp]
  (log tramp (format "Initiating parse: %s at index %d (%s)"
                     (print/combinators->str parser) index
                     (string-context (:text tramp) index)))
  (case (:tag parser)
    :nt (non-terminal-parse parser index tramp)
    :alt (alt-parse parser index tramp)
    :cat (cat-parse parser index tramp)
    :string (string-parse parser index tramp)
    :string-ci (string-case-insensitive-parse parser index tramp)
    :char (char-range-parse parser index tramp)
    :epsilon (epsilon-parse parser index tramp)
    :opt (opt-parse parser index tramp)
    :plus (plus-parse parser index tramp)
    :rep (rep-parse parser index tramp)
    :star (star-parse parser index tramp)
    :regexp (regexp-parse parser index tramp)
    :look (lookahead-parse parser index tramp)
    :neg (negative-lookahead-parse parser index tramp)
    :ord (ordered-alt-parse parser index tramp)))

(declare alt-full-parse cat-full-parse string-full-parse epsilon-full-parse 
         non-terminal-full-parse opt-full-parse plus-full-parse star-full-parse
         rep-full-parse regexp-full-parse lookahead-full-parse ordered-alt-full-parse
         string-case-insensitive-full-parse char-range-full-parse)
(defn -full-parse [parser index tramp]
  (log tramp (format "Initiating full parse: %s at index %d (%s)"
               (print/combinators->str parser) index
               (string-context (:text tramp) index)))
  (case (:tag parser)
    :nt (non-terminal-full-parse parser index tramp)
    :alt (alt-full-parse parser index tramp)
    :cat (cat-full-parse parser index tramp)
    :string (string-full-parse parser index tramp)
    :string-ci (string-case-insensitive-full-parse parser index tramp)
    :char (char-range-full-parse parser index tramp)
    :epsilon (epsilon-full-parse parser index tramp)
    :opt (opt-full-parse parser index tramp)
    :plus (plus-full-parse parser index tramp)
    :rep (rep-full-parse parser index tramp)
    :star (star-full-parse parser index tramp)
    :regexp (regexp-full-parse parser index tramp)
    :look (lookahead-full-parse parser index tramp)
    :neg (negative-lookahead-parse parser index tramp)
    :ord (ordered-alt-full-parse parser index tramp)))

(defrecord Failure [index reason])

#?(:clj
   (defmethod clojure.core/print-method Failure [x writer]
     (binding [*out* writer]
       (fail/pprint-failure x)))
   :cljs
   (extend-protocol IPrintWithWriter
     instaparse.gll/Failure
     (-pr-writer [fail writer _]
       (-write writer (with-out-str
                        (fail/pprint-failure fail))))))

; This is a trick to make sure we can recognize the type of
; a Failure record after this namespace is recompiled,
; but the core namespace is not recompiled
; which is what happens when tracing is enabled.
(def failure-type (type (Failure. nil nil)))

#?(:clj
   (defn text->segment
     "Converts text to a Segment, which has fast subsequencing"
     [^CharSequence text]
     (Segment. text 0 (count text)))

   :cljs
   (defn text->segment
     [text]
     text))

#?(:clj
   (defn sub-sequence
     "Like clojure.core/subs but consumes and returns a CharSequence"
     (^CharSequence [^CharSequence text start]
      (.subSequence text start (.length text)))
     (^CharSequence [^CharSequence text start end]
      (.subSequence text start end)))

   :cljs
   (def sub-sequence subs))

; The trampoline structure contains the grammar, text to parse, a stack and a nodes
; Also contains an atom to hold successes and one to hold index of failure point.
; grammar is a map from non-terminals to parsers
; text is a CharSequence
; stack is an atom of a vector containing items implementing the Execute protocol.
; nodes is an atom containing a map from [index parser] pairs to Nodes
; success contains a successful parse
; failure contains the index of the furthest-along failure

(defrecord Tramp [grammar text segment fail-index node-builder
                  stack next-stack generation negative-listeners 
                  msg-cache nodes success failure trace?])
(defn make-tramp 
  ([grammar text] (make-tramp grammar text (text->segment text) -1 nil))
  ([grammar text segment] (make-tramp grammar text segment -1 nil))
  ([grammar text fail-index node-builder] (make-tramp grammar text (text->segment text) fail-index node-builder))
  ([grammar text segment fail-index node-builder]
    (Tramp. grammar text segment
            fail-index node-builder
            (atom []) (atom []) (atom 0) (atom (sorted-map-by >)) 
            (atom {}) (atom {}) (atom nil) (atom (Failure. 0 [])) (trace-or-false))))
  
; A Success record contains the result and the index to continue from
(defn make-success [result index] {:result result :index index})
(defn total-success? [tramp s]
  (= (count (:text tramp)) (:index s)))

; The trampoline's nodes field is map from [index parser] pairs to Nodes
; Nodes track the results of a given parser at a given index, and the listeners
; who care about the result.
; results are expected to be refs of sets.
; listeners are refs of vectors.

(defrecord Node [listeners full-listeners results full-results])
(defn make-node [] (Node. (atom []) (atom []) (atom #{}) (atom #{})))
; Currently using records for Node.  Seems to run marginally faster.
; Here's the way without records:
;(defn make-node [] {:listeners (atom []) :full-listeners (atom []) 
;                    :results (atom #{}) :full-results (atom #{})})

;; Trampoline helper functions

(defn push-stack
  "Pushes an item onto the trampoline's stack"
  [tramp item]
  (profile (add! :push-stack))
  (swap! (:stack tramp) conj item))

(defn push-message
  "Pushes onto stack a message to a given listener about a result"
  [tramp listener result]
  (let [cache (:msg-cache tramp)
        i (:index result)
        k [listener i]
        c (get @cache k 0)
        f #(listener result)]
    (profile (add! :push-message))    
    #_(dprintln "push-message" i c @(:generation tramp) (count @(:stack tramp))
             (count @(:next-stack tramp)))
    #_(dprintln "push-message: listener result" listener result)
    (if (> c @(:generation tramp))
      (swap! (:next-stack tramp) conj f)
      (swap! (:stack tramp) conj f))
    (swap! cache assoc k (inc c))))
    
(defn listener-exists?
  "Tests whether node already has a listener"
  [tramp node-key]
  (let [nodes (:nodes tramp)]
    (when-let [node (@nodes node-key)]
      (pos? (count @(:listeners node))))))

(defn full-listener-exists?
  "Tests whether node already has a listener or full-listener"
  [tramp node-key]
  (let [nodes (:nodes tramp)]
    (when-let [node (@nodes node-key)]
      (or (pos? (count @(:full-listeners node)))
          (pos? (count @(:listeners node)))))))

(defn result-exists?
  "Tests whether node has a result or full-result"
  [tramp node-key]
  (let [nodes (:nodes tramp)]
    (when-let [node (@nodes node-key)]
      (or (pos? (count @(:full-results node)))
          (pos? (count @(:results node)))))))

(defn full-result-exists?
  "Tests whether node has a full-result"
  [tramp node-key]
  (let [nodes (:nodes tramp)]
    (when-let [node (@nodes node-key)]
      (pos? (count @(:full-results node))))))      

(defn node-get
  "Gets node if already exists, otherwise creates one"
  [tramp node-key]
  (let [nodes (:nodes tramp)]
    (if-let [node (@nodes node-key)]
      node 
      (let [node (make-node)]
        (profile (add! :create-node))
        (swap! nodes assoc node-key node)
        node))))

(defn safe-with-meta [obj metamap]
  (if #?(:clj (instance? clojure.lang.IObj obj)
         :cljs (satisfies? cljs.core/IWithMeta obj))
    (with-meta obj metamap)
    obj))

(defn push-result
  "Pushes a result into the trampoline's node.
   Categorizes as either result or full-result.
   Schedules notification to all existing listeners of result
   (Full listeners only get notified about full results)"
  [tramp node-key result]
  (log tramp (if (= (:tag (node-key 1)) :neg)
               (format "Negation satisfied: %s at index %d (%s)"
                       (print/combinators->str (node-key 1)) (node-key 0)
                       (string-context (:text tramp) (node-key 0)))
               (format "Result for %s at index %d (%s) => %s"
                       (print/combinators->str (node-key 1)) (node-key 0)
                       (string-context (:text tramp) (node-key 0))
                       (with-out-str (pr (:result result))))))
  (let [node (node-get tramp node-key)
        parser (node-key 1)
        ;; reduce result with reduction function if it exists
        result (if (:hide parser)
                 (assoc result :result nil)
                 result)
        result (if-let [reduction-function (:red parser)]
                 (make-success  
                   (safe-with-meta 
                     (red/apply-reduction reduction-function (:result result))
                     {::start-index (node-key 0) ::end-index (:index result)})
                   (:index result))                 
                 result)              
        total? (total-success? tramp result)
        results (if total? (:full-results node) (:results node))]
    (when (not (@results result))  ; when result is not already in @results
      (profile (add! :push-result))
      (swap! results conj result)
      (doseq [listener @(:listeners node)]
        (push-message tramp listener result))
      (when total?
        (doseq [listener @(:full-listeners node)]
          (push-message tramp listener result)))))) 

(defn push-listener
  "Pushes a listener into the trampoline's node.
   Schedules notification to listener of all existing results.
   Initiates parse if necessary"
  [tramp node-key listener]
  #_(dprintln "push-listener" [(node-key 1) (node-key 0)] (type listener))
  (let [listener-already-exists? (listener-exists? tramp node-key)
        node (node-get tramp node-key)
        listeners (:listeners node)]
    (profile (add! :push-listener))
    (swap! listeners conj listener)
    (doseq [result @(:results node)]
      (push-message tramp listener result))
    (doseq [result @(:full-results node)]
      (push-message tramp listener result))
    (when (not listener-already-exists?)
      (push-stack tramp #(-parse (node-key 1) (node-key 0) tramp))))) 

(defn push-full-listener
  "Pushes a listener into the trampoline's node.
   Schedules notification to listener of all existing full results."
  [tramp node-key listener]
  (let [full-listener-already-exists? (full-listener-exists? tramp node-key)
        node (node-get tramp node-key)
        listeners (:full-listeners node)]
    (profile (add! :push-full-listener))
    (swap! listeners conj listener)
    (doseq [result @(:full-results node)]
      (push-message tramp listener result))
    (when (not full-listener-already-exists?)
      (push-stack tramp #(-full-parse (node-key 1) (node-key 0) tramp)))))

(def merge-negative-listeners (partial merge-with into))

(defn push-negative-listener
  "Pushes a thunk onto the trampoline's negative-listener stack."
  [tramp creator negative-listener]
  #_(dprintln "push-negative-listener" (type negative-listener))
  ; creator is a node-key, i.e., a [index parser] pair
  (swap! (:negative-listeners tramp) merge-negative-listeners 
         {(creator 0) [(attach-diagnostic-meta negative-listener {:creator creator})]}))  

;(defn success [tramp node-key result end]
;  (push-result tramp node-key (make-success result end)))

#?(:clj
   (defmacro success [tramp node-key result end]
     `(push-result ~tramp ~node-key (make-success ~result ~end))))

(declare build-node-with-meta)
(defn fail [tramp node-key index reason]
  (log tramp (format "No result for %s at index %d (%s)"
                     (print/combinators->str (node-key 1)) (node-key 0)
                     (string-context (:text tramp) (node-key 0))))
  (swap! (:failure tramp) 
         (fn [failure] 
           (let [current-index (:index failure)]
             (case (compare index current-index)
               1 (Failure. index [reason])
               0 (Failure. index (conj (:reason failure) reason))
               -1  failure))))
  #_(dprintln "Fail index" (:fail-index tramp))
  (when (= index (:fail-index tramp))
    (success tramp node-key 
             (build-node-with-meta
               (:node-builder tramp) :instaparse/failure
               (sub-sequence (:text tramp) index)
               index (count (:text tramp)))
             (count (:text tramp)))))

;; Stack helper functions

(defn step
  "Executes one thing on the stack (not threadsafe)"
  [stack]
  (let [top (peek @stack)]
    (swap! stack pop)
    #_(dprintln "Top" top (meta top))
    (top)))

(defn run
  "Executes the stack until exhausted"
  ([tramp] (run tramp nil))
  ([tramp found-result?] 
    (let [stack (:stack tramp)]
          ;_ (dprintln "run" found-result? (count @(:stack tramp)) (count @(:next-stack tramp)))]
      (cond
        @(:success tramp)
        (do (log tramp "Successful parse.\nProfile: " @stats)
          (cons (:result @(:success tramp))
                (lazy-seq
                  (do (reset! (:success tramp) nil)
                    (run tramp true)))))
        
        (pos? (count @stack))
        (do ;(dprintln "stacks" (count @stack) (count @(:next-stack tramp)))
          (step stack) (recur tramp found-result?))

        (pos? (count @(:negative-listeners tramp)))
        (let [[index listeners] (first @(:negative-listeners tramp))
              listener (peek listeners)]
          (log tramp (format "Exhausted results for %s at index %d (%s)"
                             (print/combinators->str (((meta listener) :creator) 1))
                             (((meta listener) :creator) 0)
                             (string-context (:text tramp) 
                                             (((meta listener) :creator) 0)))) 
          (listener)
          (if (= (count listeners) 1)
            (swap! (:negative-listeners tramp) dissoc index)
            (swap! (:negative-listeners tramp) update-in [index] pop))
          (recur tramp found-result?))        
        
        found-result?
        (let [next-stack (:next-stack tramp)]
          #_(dprintln "Swapping stacks" (count @(:stack tramp)) 
                   (count @(:next-stack tramp)))
          (reset! stack @next-stack) 
          (reset! next-stack [])
          (swap! (:generation tramp) inc)  
          #_(dprintln "Swapped stacks" (count @(:stack tramp)) 
                   (count @(:next-stack tramp)))          
          (recur tramp nil))
        
        :else nil))))

;; Listeners

; There are six kinds of listeners that receive notifications
; The first kind is a NodeListener which simply listens for a completed parse result
; Takes the node-key of the parser which is awaiting this result.

(defn NodeListener [node-key tramp]  
  (fn [result]
    ;(dprintln "Node Listener received" [(node-key 0) (:tag (node-key 1))] "result" result)
    (push-result tramp node-key result)))

; The second kind of listener handles lookahead.
(defn LookListener [node-key tramp]
  (fn [result]
    (success tramp node-key nil (node-key 0))))     

; The third kind of listener is a CatListener which listens at each stage of the
; concatenation parser to carry on the next step.  Think of it as a parse continuation.
; A CatListener needs to know the sequence of results for the parsers that have come
; before, and a list of parsers that remain.  Also, the node-key of the final node
; that needs to know the overall result of the cat parser.

(defn CatListener [results-so-far parser-sequence node-key tramp]
  (dpprint {:tag :CatListener
           :results-so-far results-so-far
           :parser-sequence (map :tag parser-sequence)
           :node-key [(node-key 0) (:tag (node-key 1))]})
  (fn [result] 
    (let [{parsed-result :result continue-index :index} result
          new-results-so-far (afs/conj-flat results-so-far parsed-result)]
      (if (seq parser-sequence)
        (push-listener tramp [continue-index (first parser-sequence)]
                       (CatListener new-results-so-far (next parser-sequence) node-key tramp))          
        (success tramp node-key new-results-so-far continue-index)))))

(defn CatFullListener [results-so-far parser-sequence node-key tramp]
;  (dpprint {:tag :CatFullListener
;           :results-so-far results-so-far
;           :parser-sequence (map :tag parser-sequence)
;           :node-key [(node-key 0) (:tag (node-key 1))]})
  (fn [result] 
    (let [{parsed-result :result continue-index :index} result
          new-results-so-far (afs/conj-flat results-so-far parsed-result)]
      (cond
        (red/singleton? parser-sequence)
        (push-full-listener tramp [continue-index (first parser-sequence)]
                            (CatFullListener new-results-so-far (next parser-sequence) node-key tramp))        
        
        (seq parser-sequence)
        (push-listener tramp [continue-index (first parser-sequence)]
                       (CatFullListener new-results-so-far (next parser-sequence) node-key tramp))          
        
        :else
        (success tramp node-key new-results-so-far continue-index)))))

; The fourth kind of listener is a PlusListener, which is a variation of
; the CatListener but optimized for "one or more" parsers.

(defn PlusListener [results-so-far parser prev-index node-key tramp]
  (fn [result]
    (let [{parsed-result :result continue-index :index} result]
      (if (= continue-index prev-index)
        (when (zero? (count results-so-far)) 
          (success tramp node-key nil continue-index))        
        (let [new-results-so-far (afs/conj-flat results-so-far parsed-result)]
          (push-listener tramp [continue-index parser]
                         (PlusListener new-results-so-far parser continue-index
                                       node-key tramp))            
          (success tramp node-key new-results-so-far continue-index))))))

(defn PlusFullListener [results-so-far parser prev-index node-key tramp]
  (fn [result]
    (let [{parsed-result :result continue-index :index} result]
      (if (= continue-index prev-index)
        (when (zero? (count results-so-far))
          (success tramp node-key nil continue-index))
        (let [new-results-so-far (afs/conj-flat results-so-far parsed-result)]
          (if (= continue-index (count (:text tramp)))
            (success tramp node-key new-results-so-far continue-index)
            (push-listener tramp [continue-index parser]
                           (PlusFullListener new-results-so-far parser continue-index 
                                             node-key tramp))))))))

; The fifth kind of listener is a RepListener, which wants between m and n repetitions of a parser

(defn RepListener [results-so-far n-results-so-far parser m n prev-index node-key tramp]
  (fn [result]
    (let [{parsed-result :result continue-index :index} result]
      ;(dprintln "Rep" (type results-so-far))
      (let [new-results-so-far (afs/conj-flat results-so-far parsed-result)
            new-n-results-so-far (inc n-results-so-far)]
        (when (<= m new-n-results-so-far n)
          (success tramp node-key new-results-so-far continue-index))
        (when (< new-n-results-so-far n)
          (push-listener tramp [continue-index parser]
                         (RepListener new-results-so-far new-n-results-so-far
                                      parser m n continue-index
                                      node-key tramp)))))))

(defn RepFullListener [results-so-far n-results-so-far parser m n prev-index node-key tramp]
  (fn [result]
    (let [{parsed-result :result continue-index :index} result]
      ;(dprintln "RepFull" (type parsed-result))
      (let [new-results-so-far (afs/conj-flat results-so-far parsed-result)
            new-n-results-so-far (inc n-results-so-far)]
        (if (= continue-index (count (:text tramp)))
          (when (<= m new-n-results-so-far n)
            (success tramp node-key new-results-so-far continue-index))
          (when (< new-n-results-so-far n)
            (push-listener tramp [continue-index parser]
                           (RepFullListener new-results-so-far new-n-results-so-far
                                            parser m n continue-index
                                            node-key tramp))))))))

; The top level listener is the final kind of listener

(defn TopListener [tramp] 
  (fn [result] 
    (reset! (:success tramp) result)))

;; Parsers

(defn string-parse
  [this index tramp]
  (let [string (:string this)
        text (:text tramp)
        end (min (count text) (+ index (count string)))
        head (sub-sequence text index end)]      
    (if (= string head)
      (success tramp [index this] string end)
      (fail tramp [index this] index
            {:tag :string :expecting string}))))

(defn string-full-parse
  [this index tramp]
  (let [string (:string this)
        text (:text tramp)
        end (min (count text) (+ index (count string)))
        head (sub-sequence text index end)]      
    (if (and (= end (count text)) (= string head))
      (success tramp [index this] string end)
      (fail tramp [index this] index
            {:tag :string :expecting string :full true}))))

#?(:clj
   (defn equals-ignore-case [^String s1 ^String s2]
     (.equalsIgnoreCase s1 s2))
   :cljs
   (defn equals-ignore-case [s1 s2]
     (= (.toUpperCase s1) (.toUpperCase s2))))

(defn string-case-insensitive-parse
  [this index tramp]
  (let [string (:string this)
        text (:text tramp)
        end (min (count text) (+ index (count string)))
        head (sub-sequence text index end)]      
    (if (equals-ignore-case string head)
      (success tramp [index this] string end)
      (fail tramp [index this] index
            {:tag :string :expecting string}))))

(defn string-case-insensitive-full-parse
  [this index tramp]
  (let [string (:string this)
        text (:text tramp)
        end (min (count text) (+ index (count string)))
        head (sub-sequence text index end)]      
    (if (and (= end (count text)) (equals-ignore-case string head))
      (success tramp [index this] string end)
      (fail tramp [index this] index
            {:tag :string :expecting string :full true}))))

#?(:clj
   (defn single-char-code-at
     "Returns the int value of a single char at the given index,
  assuming we're looking for up to 0xFFFF (the maximum value for a
  UTF-16 single char)."
     [^CharSequence text index]
     (int (.charAt text index)))
   :cljs
   (defn single-char-code-at
     [text index]
     (.charCodeAt text index)))

#?(:clj
   (defn unicode-code-point-at
     "Returns the unicode code point representing one or two chars at
  the given index."
     [^CharSequence text index]
     (Character/codePointAt text (int index)))
   :cljs
   (defn unicode-code-point-at
     [text index]
     (u/getCodePointAround text (int index))))

#?(:clj
   (defn code-point->chars
     "Takes a Unicode code point, and returns a string of one or two chars."
     [code-point]
     (String. (Character/toChars code-point)))
   :cljs
   (defn code-point->chars
     [code-point]
     (u/fromCharCode code-point)))

(defn char-range-parse
  [this index tramp]
  (let [lo (:lo this)
        hi (:hi this)
        text (:text tramp)]
    (cond
      (>= index (count text)) (fail tramp [index this] index
                                    {:tag :char :expecting {:char-range true :lo lo :hi hi}})
      (<= hi 0xFFFF) (let [code (single-char-code-at text index)]
                       (if (<= lo code hi)
                         (success tramp [index this] (str (char code)) (inc index))
                         (fail tramp [index this] index
                               {:tag :char :expecting {:char-range true :lo lo :hi hi}})))
      :else (let [code-point (unicode-code-point-at text index)
                  char-string (code-point->chars code-point)]
              (if (<= lo code-point hi)
                (success tramp [index this] char-string
                         (+ index (count char-string)))
                (fail tramp [index this] index
                      {:tag :char :expecting {:char-range true :lo lo :hi hi}}))))))

(defn char-range-full-parse
  [this index tramp]
  (let [lo (:lo this)
        hi (:hi this)
        text (:text tramp)
        end (count text)]
    (cond
      (>= index (count text)) (fail tramp [index this] index
                                    {:tag :char :expecting {:char-range true :lo lo :hi hi}})
      (<= hi 0xFFFF) (let [code (single-char-code-at text index)]
                       (if (and (= (inc index) end) (<= lo code hi))
                         (success tramp [index this] (str (char code)) end)
                         (fail tramp [index this] index
                               {:tag :char :expecting {:char-range true :lo lo :hi hi}})))
      :else (let [code-point (unicode-code-point-at text index)
                  char-string (code-point->chars code-point)]
              (if (and (= (+ index (count char-string)) end) (<= lo code-point hi))
                (success tramp [index this] char-string end)
                (fail tramp [index this] index
                      {:tag :char :expecting {:char-range true :lo lo :hi hi} :full true}))))))

#?(:clj
   (defn re-match-at-front [regexp text]
     (let [^java.util.regex.Matcher matcher (re-matcher regexp text)
           match? (.lookingAt matcher)]
       (when match?
         (.group matcher))))
   :cljs
   (defn re-match-at-front [regexp text]
     (let [re (js/RegExp. (.-source regexp) (str "g" (regexp-flags regexp)))
           m (.exec re text)]
       (when (and m (zero? (.-index m)))
         (first m)))))
    
(defn regexp-parse
  [this index tramp]
  (let [regexp (:regexp this)
        ^Segment text (:segment tramp)
        substring (sub-sequence text index)
        match (re-match-at-front regexp substring)]
    (if match
      (success tramp [index this] match (+ index (count match)))
      (fail tramp [index this] index
            {:tag :regexp :expecting regexp}))))

(defn regexp-full-parse
  [this index tramp]
  (let [regexp (:regexp this)
        ^Segment text (:segment tramp)
        substring (sub-sequence text index)
        match (re-match-at-front regexp substring)
        desired-length (- (count text) index)]
    (if (and match (= (count match) desired-length))
      (success tramp [index this] match (count text))
      (fail tramp [index this] index
            {:tag :regexp :expecting regexp :full true}))))

(defn cat-parse
  [this index tramp]
  (let [parsers (:parsers this)]
    ; Kick-off the first parser, with a CatListener ready to pass the result on in the chain
    ; and with a final target of notifying this parser when the whole sequence is complete
    (push-listener tramp [index (first parsers)] 
                   (CatListener afs/EMPTY (next parsers) [index this] tramp))))	      

(defn cat-full-parse
  [this index tramp]
  (let [parsers (:parsers this)]
    ; Kick-off the first parser, with a CatListener ready to pass the result on in the chain
    ; and with a final target of notifying this parser when the whole sequence is complete
    (push-listener tramp [index (first parsers)] 
                   (CatFullListener afs/EMPTY (next parsers) [index this] tramp))))	      

(defn plus-parse
  [this index tramp]
  (let [parser (:parser this)]
    (push-listener tramp [index parser] 
                   (PlusListener afs/EMPTY parser index [index this] tramp))))       

(defn plus-full-parse
  [this index tramp]
  (let [parser (:parser this)]
    (push-listener tramp [index parser] 
                   (PlusFullListener afs/EMPTY parser index [index this] tramp))))       

(defn rep-parse
  [this index tramp]
  (let [parser (:parser this),
        m (:min this),
        n (:max this)]     
    (if (zero? m)
      (do 
        (success tramp [index this] nil index)
        (when (>= n 1)
          (push-listener tramp [index parser]
                         (RepListener afs/EMPTY 0 parser 1 n index [index this] tramp))))
      (push-listener tramp [index parser]
                     (RepListener afs/EMPTY 0 parser m n index [index this] tramp)))))

(defn rep-full-parse
  [this index tramp]
  (let [parser (:parser this),
        m (:min this),
        n (:max this)]
    (if (zero? m)
      (do 
        (success tramp [index this] nil index)
        (when (>= n 1)
          (push-listener tramp [index parser]
                         (RepFullListener afs/EMPTY 0 parser 1 n index [index this] tramp))))
      (push-listener tramp [index parser]
                     (RepFullListener afs/EMPTY 0 parser m n index [index this] tramp)))))                 

(defn star-parse
  [this index tramp]
  (let [parser (:parser this)]
    (push-listener tramp [index parser] 
                   (PlusListener afs/EMPTY parser index [index this] tramp))              
    (success tramp [index this] nil index)))

(defn star-full-parse
  [this index tramp]
  (let [parser (:parser this)]
    (if (= index (count (:text tramp)))
      (success tramp [index this] nil index)
      (do
        (push-listener tramp [index parser] 
                       (PlusFullListener afs/EMPTY parser index [index this] tramp))))))

(defn alt-parse
  [this index tramp]
  (let [parsers (:parsers this)]
    (doseq [parser parsers]
      (push-listener tramp [index parser] (NodeListener [index this] tramp)))))      

(defn alt-full-parse
  [this index tramp]
  (let [parsers (:parsers this)]
    (doseq [parser parsers]
      (push-full-listener tramp [index parser] (NodeListener [index this] tramp)))))        

(defn ordered-alt-parse
  [this index tramp]
  (let [parser1 (:parser1 this)
        parser2 (:parser2 this)
        node-key-parser1 [index parser1]
        node-key-parser2 [index parser2]
        listener (NodeListener [index this] tramp)]
    (push-listener tramp node-key-parser1 listener)
    (push-negative-listener 
      tramp       
      node-key-parser1
      #(push-listener tramp node-key-parser2 listener))))
          
(defn ordered-alt-full-parse
  [this index tramp]
  (let [parser1 (:parser1 this)
        parser2 (:parser2 this)
        node-key-parser1 [index parser1]
        node-key-parser2 [index parser2]
        listener (NodeListener [index this] tramp)]
    (push-full-listener tramp node-key-parser1 listener)
    (push-negative-listener 
      tramp       
      node-key-parser1
      #(push-full-listener tramp node-key-parser2 listener))))
  
(defn opt-parse
  [this index tramp]
  (let [parser (:parser this)]
    (push-listener tramp [index parser] (NodeListener [index this] tramp))      
    (success tramp [index this] nil index)))

(defn opt-full-parse
  [this index tramp]
  (let [parser (:parser this)]
    (push-full-listener tramp [index parser] (NodeListener [index this] tramp))    
    (if (= index (count (:text tramp)))
      (success tramp [index this] nil index)
      (fail tramp [index this] index {:tag :optional :expecting :end-of-string}))))    

(defn non-terminal-parse
  [this index tramp]
  (let [parser (get-parser (:grammar tramp) (:keyword this))]
    (push-listener tramp [index parser] (NodeListener [index this] tramp))))      

(defn non-terminal-full-parse
  [this index tramp]
  (let [parser (get-parser (:grammar tramp) (:keyword this))]
    (push-full-listener tramp [index parser] (NodeListener [index this] tramp))))      

(defn lookahead-parse
  [this index tramp]
  (let [parser (:parser this)]
    (push-listener tramp [index parser] (LookListener [index this] tramp))))      

(defn lookahead-full-parse
  [this index tramp]
  (if (= index (count (:text tramp)))
    (lookahead-parse this index tramp)
    (fail tramp [index this] index {:tag :lookahead :expecting :end-of-string})))

;(declare negative-parse?)
;(defn negative-lookahead-parse
;  [this index tramp]
;  (let [parser (:parser this)
;        remaining-text (sub-sequence (:text tramp) index)]
;    (if (negative-parse? (:grammar tramp) parser remaining-text)
;      (success tramp [index this] nil index)
;      (fail tramp index :negative-lookahead))))

(defn negative-lookahead-parse
  [this index tramp]
  (let [parser (:parser this)        
        node-key [index parser]]
    (if (result-exists? tramp node-key)
      (fail tramp [index this] index {:tag :negative-lookahead})
      (do 
        (push-listener tramp node-key 
                       (let [fail-send (delay (fail tramp [index this] index
                                                    {:tag :negative-lookahead
                                                     :expecting {:NOT 
                                                                 (print/combinators->str parser)}}))] 
                         (fn [result] (force fail-send))))     
        (push-negative-listener 
          tramp
          node-key
          #(when (not (result-exists? tramp node-key))
             (success tramp [index this] nil index)))))))      

(defn epsilon-parse
  [this index tramp] (success tramp [index this] nil index))
(defn epsilon-full-parse
  [this index tramp]
  (if (= index (count (:text tramp)))
    (success tramp [index this] nil index)
    (fail tramp [index this] index {:tag :Epsilon :expecting :end-of-string})))
    
;; Parsing functions

(defn start-parser [tramp parser partial?]
  (if partial?
    (push-listener tramp [0 parser] (TopListener tramp))
    (push-full-listener tramp [0 parser] (TopListener tramp))))

(defn parses [grammar start text partial?]
  (profile (clear!))
  (let [tramp (make-tramp grammar text)
        parser (nt start)]
    (start-parser tramp parser partial?)
    (if-let [all-parses (run tramp)]
      all-parses 
      (with-meta () 
        (fail/augment-failure @(:failure tramp) text))))) 

(defn parse [grammar start text partial?]
  (profile (clear!))
  (let [tramp (make-tramp grammar text)
        parser (nt start)]
    (start-parser tramp parser partial?)
    (if-let [all-parses (run tramp)]
      (first all-parses) 
      (fail/augment-failure @(:failure tramp) text))))

;; The node builder function is what we use to build the failure nodes
;; but we want to include start and end metadata as well.

(defn build-node-with-meta [node-builder tag content start end]
  (with-meta
    (node-builder tag content)
    {::start-index start ::end-index end}))

(defn build-total-failure-node [node-builder start text]
  (let [build-failure-node
        (build-node-with-meta node-builder :instaparse/failure text 0 (count text)),            
        build-start-node
        (build-node-with-meta node-builder start build-failure-node 0 (count text))]
    build-start-node))

(defn parses-total-after-fail 
  [grammar start text fail-index partial? node-builder]
  ;(dprintln "Parses-total-after-fail")  
  (let [tramp (make-tramp grammar text fail-index node-builder)
        parser (nt start)]
    (log tramp "Parse failure. Restarting for total parse.")
    (start-parser tramp parser partial?)
    (if-let [all-parses (run tramp)]
      all-parses
      (list (build-total-failure-node node-builder start text)))))

(defn merge-meta
  "A variation on with-meta that merges the existing metamap into the new metamap,
rather than overwriting the metamap entirely."
  [obj metamap]
  (with-meta obj (merge metamap (meta obj))))
      
(defn parses-total 
  [grammar start text partial? node-builder]
  (profile (clear!))
  (let [all-parses (parses grammar start text partial?)]
    (if (seq all-parses)
      all-parses
      (merge-meta
        (parses-total-after-fail grammar start text 
                                 (:index (meta all-parses)) 
                                 partial? node-builder)
        (meta all-parses)))))

(defn parse-total-after-fail 
  [grammar start text fail-index partial? node-builder]
  ;(dprintln "Parse-total-after-fail")  
  (let [tramp (make-tramp grammar text fail-index node-builder)
        parser (nt start)]
    (log tramp "Parse failure. Restarting for total parse.")
    (start-parser tramp parser partial?)
    (if-let [all-parses (run tramp)]
      (first all-parses)
      (build-total-failure-node node-builder start text))))

(defn parse-total 
  [grammar start text partial? node-builder]
  (profile (clear!))
  (let [result (parse grammar start text partial?)]
    (if-not (instance? Failure result)
      result
      (merge-meta        
        (parse-total-after-fail grammar start text 
                                (:index result) 
                                partial? node-builder)
        result))))

;; Variation, but not for end-user

;(defn negative-parse? 
;  "takes pre-processed grammar and parser" 
;  [grammar parser text]  
;  (let [tramp (make-tramp grammar text)]
;    (push-listener tramp [0 parser] (TopListener tramp))    
;    (empty? (run tramp))))
;    

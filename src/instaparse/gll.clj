(ns instaparse.gll
  "The heart of the parsing mechanism.  Contains the trampoline structure,
   the parsing dispatch function, the nodes where listeners are stored,
   the different types of listeners, and the loop for executing the various
   listeners and parse commands that are on the stack."
  
  ;; Incremental vector provides a more performant hashing strategy 
  ;; for this use-case for vectors
  (:require [instaparse.incremental-vector :as iv])
  
  ;; failure contains the augment-failure function, which is called to
  ;; add enough information to the failure object for pretty printing 
  (:require [instaparse.failure :as fail])
  
  ;; reduction contains code relating to reductions and flattening.
  (:require [instaparse.reduction :as red])
  
  ;; Two of the public combinators from here are needed.
  (:require [instaparse.combinators :refer [Epsilon nt]])
  
  ;; Need a way to convert parsers into strings for printing and error messages.
  (:require [instaparse.print :as print])
    
  (:use clojure.pprint clojure.repl))

(def DEBUG nil)
(defmacro debug [& body]
  (when DEBUG
    `(do ~@body)))
(defmacro dprintln [& body]
  `(debug (println ~@body)))

;TODO
;Decide whether to make pretty printing the default.
;Rename combinators-private namespace
;Check for valid grammar
;total and partial parses
;build parser from combinators
;Documentation
;Concurrency
;Allow parsing of arbitrary sequences.

(debug (def stats (atom {})))
(debug (defn add! [call] (swap! stats update-in [call] (fnil inc 0))))
(debug (defn clear! [] (reset! stats {})))

(defn get-parser [grammar p]
  (get grammar p p))

(declare alt-parse cat-parse string-parse epsilon-parse non-terminal-parse
         opt-parse plus-parse star-parse regexp-parse lookahead-parse
         negative-lookahead-parse ordered-alt-parse)
(defn -parse [parser index tramp]
  (dprintln "-parse" index (:tag parser))
  (case (:tag parser)
    :nt (non-terminal-parse parser index tramp)
    :alt (alt-parse parser index tramp)
    :cat (cat-parse parser index tramp)
    :string (string-parse parser index tramp)
    :epsilon (epsilon-parse index tramp)
    :opt (opt-parse parser index tramp)
    :plus (plus-parse parser index tramp)
    :star (star-parse parser index tramp)
    :regexp (regexp-parse parser index tramp)
    :look (lookahead-parse parser index tramp)
    :neg (negative-lookahead-parse parser index tramp)
    :ord (ordered-alt-parse parser index tramp)))

(declare alt-full-parse cat-full-parse string-full-parse epsilon-full-parse 
         non-terminal-full-parse opt-full-parse plus-full-parse star-full-parse
         regexp-full-parse lookahead-full-parse ordered-alt-full-parse)
(defn -full-parse [parser index tramp]
  (dprintln "-full-parse" index (:tag parser))
  (case (:tag parser)
    :nt (non-terminal-full-parse parser index tramp)
    :alt (alt-full-parse parser index tramp)
    :cat (cat-full-parse parser index tramp)
    :string (string-full-parse parser index tramp)
    :epsilon (epsilon-full-parse index tramp)
    :opt (opt-full-parse parser index tramp)
    :plus (plus-full-parse parser index tramp)
    :star (star-full-parse parser index tramp)
    :regexp (regexp-full-parse parser index tramp)
    :look (lookahead-full-parse parser index tramp)
    :neg (negative-lookahead-parse parser index tramp)
    :ord (ordered-alt-full-parse parser index tramp)))

(defrecord Failure [index reason])  
(defmethod clojure.core/print-method Failure [x writer]
  (binding [*out* writer]
    (fail/pprint-failure x)))

; The trampoline structure contains the grammar, text to parse, a stack and a nodes
; Also contains an atom to hold successes and one to hold index of failure point.
; grammar is a map from non-terminals to parsers
; text is a string
; stack is an atom of a vector containing items implementing the Execute protocol.
; nodes is an atom containing a map from [index parser] pairs to Nodes
; success contains a successful parse
; failure contains the index of the furthest-along failure

(defrecord Tramp [grammar text stack next-stack generation 
                  negative-listeners msg-cache nodes success failure])
(defn make-tramp [grammar text] 
  (Tramp. grammar text (atom []) (atom []) (atom 0) (atom []) 
          (atom {}) (atom {}) (atom nil) (atom (Failure. 0 []))))
  
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
  (debug (add! :push-stack))
  (swap! (:stack tramp) conj item))

(defn push-message
  "Pushes onto stack a message to a given listener about a result"
  [tramp listener result]
  (let [cache (:msg-cache tramp)
        i (:index result)
        k [listener i]
        c (get @cache k 0)
        f #(listener result)]    
    #_(dprintln "push-message" i c @(:generation tramp) (count @(:stack tramp))
             (count @(:next-stack tramp)))
    #_(dprintln "listener result" listener result)
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
        (debug (add! :create-node))
        (swap! nodes assoc node-key node)
        node))))

(defn push-result
  "Pushes a result into the trampoline's node.
   Categorizes as either result or full-result.
   Schedules notification to all existing listeners of result
   (Full listeners only get notified about full results)"
  [tramp node-key result]
  (dprintln "Push result" (node-key 0) (:tag (node-key 1)) result)
  (let [node (node-get tramp node-key)
        parser (node-key 1)
        ;; reduce result with reduction function if it exists
        result (if (:hide parser)
                 (assoc result :result nil)
                 result)
        result (if-let [reduction-function (:red parser)]
                 (assoc result :result 
                        (red/apply-reduction reduction-function
                                           (:result result)))
                 result)              
        total? (total-success? tramp result)
        results (if total? (:full-results node) (:results node))]
    (when (not (@results result))  ; when result is not already in @results
      (debug (add! :push-result))
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
  (let [listener-already-exists? (listener-exists? tramp node-key)
        node (node-get tramp node-key)
        listeners (:listeners node)]
    (debug (add! :push-listener))
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
    (debug (add! :push-full-listener))
    (swap! listeners conj listener)
    (doseq [result @(:full-results node)]
      (push-message tramp listener result))
    (when (not full-listener-already-exists?)
      (push-stack tramp #(-full-parse (node-key 1) (node-key 0) tramp)))))

(defn push-negative-listener
  "Pushes a thunk onto the trampoline's negative-listener stack."
  [tramp negative-listener]
  (swap! (:negative-listeners tramp) conj negative-listener))  

;(defn success [tramp node-key result end]
;  (push-result tramp node-key (make-success result end)))

(defmacro success [tramp node-key result end]
  `(push-result ~tramp ~node-key (make-success ~result ~end)))

(defn fail [tramp index reason]  
  (swap! (:failure tramp) 
         (fn [failure] 
           (let [current-index (:index failure)]
             (case (compare index current-index)
               1 (Failure. index [reason])
               0 (Failure. index (conj (:reason failure) reason))
               -1  failure)))))                 

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
      ;_ (dprintln found-result? (count @(:stack tramp)) (count @(:next-stack tramp)))
      (cond
        @(:success tramp)
        (lazy-seq (cons (:result @(:success tramp))
                        (do (reset! (:success tramp) nil)
                          (run tramp true))))
        
        (pos? (count @stack))
        (do (dprintln "stacks" (count @stack) (count @(:next-stack tramp)))
          (step stack) (recur tramp found-result?))
        
        (pos? (count @(:negative-listeners tramp)))
        (let [listener (peek @(:negative-listeners tramp))]
          (listener)
          (swap! (:negative-listeners tramp) pop)
          (recur tramp found-result?))
        
        found-result?
        (let [next-stack (:next-stack tramp)]
          (dprintln "Swapping stacks" (count @(:stack tramp)) 
                   (count @(:next-stack tramp)))
          (reset! stack @next-stack) 
          (reset! next-stack [])
          (swap! (:generation tramp) inc)  
          (dprintln "Swapped stacks" (count @(:stack tramp)) 
                   (count @(:next-stack tramp)))          
          (recur tramp nil))        
      
        :else nil))))

;; Listeners

; There are five kinds of listeners that receive notifications
; The first kind is a NodeListener which simply listens for a completed parse result
; Takes the node-key of the parser which is awaiting this result.

(defn NodeListener [node-key tramp]  
  (fn [result]
    (dprintln "Listener" [(node-key 0) (:tag (node-key 1))] "result" result)
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
;  (pprint {:tag :CatListener
;           :results-so-far results-so-far
;           :parser-sequence (map :tag parser-sequence)
;           :node-key [(node-key 0) (:tag (node-key 1))]})
  (fn [result] 
    (let [{parsed-result :result continue-index :index} result
          new-results-so-far (conj results-so-far parsed-result)]
      (if (seq parser-sequence)
        (push-listener tramp [continue-index (first parser-sequence)]
                       (CatListener new-results-so-far (next parser-sequence) node-key tramp))          
        (success tramp node-key new-results-so-far continue-index)))))

(defn CatFullListener [results-so-far parser-sequence node-key tramp]
;  (pprint {:tag :CatFullListener
;           :results-so-far results-so-far
;           :parser-sequence (map :tag parser-sequence)
;           :node-key [(node-key 0) (:tag (node-key 1))]})
  (fn [result] 
    (let [{parsed-result :result continue-index :index} result
          new-results-so-far (conj results-so-far parsed-result)]
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
      (when (> continue-index prev-index)
        ;(dprintln "PLUS" (type results-so-far))
        (let [new-results-so-far (conj results-so-far parsed-result)]
          (push-listener tramp [continue-index parser]
                         (PlusListener new-results-so-far parser continue-index
                                       node-key tramp))            
          (success tramp node-key new-results-so-far continue-index))))))

(defn PlusFullListener [results-so-far parser prev-index node-key tramp]
  (fn [result]
    (let [{parsed-result :result continue-index :index} result]
      (when (> continue-index prev-index)
        ;(dprintln "plusfull" (type parsed-result))
        (let [new-results-so-far (conj results-so-far parsed-result)]
          (if (= continue-index (count (:text tramp)))
            (success tramp node-key new-results-so-far continue-index)
            (push-listener tramp [continue-index parser]
                           (PlusFullListener new-results-so-far parser continue-index 
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
        head (subs text index end)]      
    (if (= string head)
      (success tramp [index this] string end)
      (fail tramp index
            {:tag :string :expecting string}))))

(defn string-full-parse
  [this index tramp]
  (let [string (:string this)
        text (:text tramp)
        end (min (count text) (+ index (count string)))
        head (subs text index end)]      
    (if (and (= end (count text)) (= string head))
      (success tramp [index this] string end)
      (fail tramp index
            {:tag :string :expecting string}))))

(defn re-seq-no-submatches [regexp text]
  (for [match (re-seq regexp text)]
    (if (vector? match) (match 0) match)))

(defn regexp-parse
  [this index tramp]
  (let [regexp (:regexp this)
        text (:text tramp)
        matches (re-seq-no-submatches regexp (subs text index))]
    (if (seq matches)
      (doseq [match matches]
        (success tramp [index this] match (+ index (count match))))
      (fail tramp index
            {:tag :regexp :expecting regexp}))))

(defn regexp-full-parse
  [this index tramp]
  (let [regexp (:regexp this)
        text (:text tramp)
        matches (re-seq-no-submatches regexp (subs text index))
        desired-length (- (count text) index)
        filtered-matches (filter #(= (count %) desired-length) matches)]
    (if-let [seq-filtered-matches (seq filtered-matches)]
      (doseq [match seq-filtered-matches]
        (success tramp [index this] match (count text)))
      (fail tramp index
            {:tag :regexp :expecting regexp}))))
        
(let [empty-cat-result (red/make-flattenable iv/EMPTY)]
	(defn cat-parse
	  [this index tramp]
	  (let [parsers (:parsers this)]
	    ; Kick-off the first parser, with a CatListener ready to pass the result on in the chain
	    ; and with a final target of notifying this parser when the whole sequence is complete
	    (push-listener tramp [index (first parsers)] 
                    (CatListener empty-cat-result (next parsers) [index this] tramp))))	      
	
	(defn cat-full-parse
	  [this index tramp]
	  (let [parsers (:parsers this)]
	    ; Kick-off the first parser, with a CatListener ready to pass the result on in the chain
	    ; and with a final target of notifying this parser when the whole sequence is complete
	    (push-listener tramp [index (first parsers)] 
                    (CatFullListener empty-cat-result (next parsers) [index this] tramp))))	      
 
 (defn plus-parse
	  [this index tramp]
	  (let [parser (:parser this)]
	    (push-listener tramp [index parser] 
                    (PlusListener empty-cat-result parser index [index this] tramp))))       
 
 (defn plus-full-parse
   [this index tramp]
   (let [parser (:parser this)]
     (push-listener tramp [index parser] 
                    (PlusFullListener empty-cat-result parser index [index this] tramp))))       
 
 (defn star-parse
	  [this index tramp]
	  (let [parser (:parser this)]
	    (push-listener tramp [index parser] 
                    (PlusListener empty-cat-result parser index [index this] tramp))              
     (success tramp [index this] nil index)))

 (defn star-full-parse
   [this index tramp]
   (let [parser (:parser this)]
     (if (= index (count (:text tramp)))
       (success tramp [index this] nil index)
       (do
         (push-listener tramp [index parser] 
                        (PlusFullListener empty-cat-result parser index [index this] tramp))))))         
 )

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
    ; If parser1 already has a result, we won't ever need to bother with parser2
    (when (not (result-exists? tramp node-key-parser1))
      (push-negative-listener 
        tramp       
        #(when (not (result-exists? tramp node-key-parser1))
           (push-listener tramp node-key-parser2 listener))))))
          
(defn ordered-alt-full-parse
  [this index tramp]
  (let [parser1 (:parser1 this)
        parser2 (:parser2 this)
        node-key-parser1 [index parser1]
        node-key-parser2 [index parser2]
        listener (NodeListener [index this] tramp)]
    (push-full-listener tramp node-key-parser1 listener)
    ; Also kick off a regular parse of parser1 to determine negative lookahead.
    (push-stack tramp #(-parse parser1 index tramp))
    ; If parser1 already has a result, we won't ever need to bother with parser2
    (when (not (result-exists? tramp node-key-parser1))
      (push-negative-listener 
        tramp       
        #(when (not (result-exists? tramp node-key-parser1))
           (push-full-listener tramp node-key-parser2 listener))))))
  
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
      (fail tramp index {:tag :optional :expecting :end-of-file}))))    

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
    (fail tramp index {:tag :lookahead :expecting :end-of-file})))

;(declare negative-parse?)
;(defn negative-lookahead-parse
;  [this index tramp]
;  (let [parser (:parser this)
;        remaining-text (subs (:text tramp) index)]
;    (if (negative-parse? (:grammar tramp) parser remaining-text)
;      (success tramp [index this] nil index)
;      (fail tramp index :negative-lookahead))))

(defn negative-lookahead-parse
  [this index tramp]
  (let [parser (:parser this)        
        node-key [index parser]]
    (if (result-exists? tramp node-key)
      (fail tramp index {:tag :negative-lookahead})
      (do 
        (push-listener tramp node-key 
                       (let [fail-send (delay (fail tramp index
                                                    {:tag :negative-lookahead
                                                     :expecting {:NOT (print/parser->str parser)}}))] 
                         (fn [result] (force fail-send))))     
        (push-negative-listener 
          tramp
          #(when (not (result-exists? tramp node-key))
             (success tramp [index this] nil index)))))))      

(defn epsilon-parse
  [index tramp] (success tramp [index Epsilon] nil index))
(defn epsilon-full-parse
  [index tramp] 
  (if (= index (count (:text tramp)))
    (success tramp [index Epsilon] nil index)
    (fail tramp index {:tag :Epsilon :expecting :end-of-file})))
    
;; End-user parsing functions

(defn parses [grammar start text]
  (debug (clear!))
  (let [tramp (make-tramp grammar text)
        parser (nt start)]
    (push-full-listener tramp [0 parser] (TopListener tramp))    
    (if-let [all-parses (run tramp)]
      all-parses 
      (with-meta () 
        (fail/augment-failure @(:failure tramp) text)))))

(defn parse [grammar start text]
  (debug (clear!))
  (let [tramp (make-tramp grammar text)
        parser (nt start)]
    (push-full-listener tramp [0 parser] (TopListener tramp))    
    (if-let [all-parses (run tramp)]
      (first all-parses) 
      (fail/augment-failure @(:failure tramp) text))))

;; Variation, but not for end-user

;(defn negative-parse? 
;  "takes pre-processed grammar and parser" 
;  [grammar parser text]  
;  (let [tramp (make-tramp grammar text)]
;    (push-listener tramp [0 parser] (TopListener tramp))    
;    (empty? (run tramp))))
;    

(ns instaparse.gll)
; Don't push onto stack things parsing tasks that have been previously pushed

(defprotocol Parser
   ;; parse a text starting at index using a given trampoline structure
  (-parse [this index tramp]))

; The trampoline structure contains the text to parse, a stack and a nodes
; text is a string
; stack is an atom of a vector containing items implementing the Execute protocol.
; nodes is an atom containing a map from [index parser] pairs to Nodes
; success contains a successful parse
; failure contains the index of the furthest-along failure

(defrecord Tramp [text stack nodes success failure])
(defn make-tramp [text] (Tramp. text (atom []) (atom {}) (atom nil) (atom 0)))
  
; A Success record contains the result and the index to continue from
(defn make-success [result index] {:result result :index index})
(defn total-success? [tramp s]
  (= (count (:text tramp)) (:index s)))

; Task is the protocol used by items on the stack

(defprotocol Task (execute [_ tramp]))

; NotificationTask is one kind of thing that can live on the stack
; A NotificationTask is comprised of a Listener and a result to send to the listener. 

(defprotocol Listener (notify [listener result tramp]))

(defrecord Notification [listener result]
  Task
  (execute [_ tramp] (notify listener result tramp)))

; ParseTask is another thing that can live on the stack

(defrecord ParseTask [parser index]
  Task
  (execute [_ tramp] (-parse parser index tramp)))

; The trampoline's nodes field is map from [index parser] pairs to Nodes
; Nodes track the results of a given parser at a given index, and the listeners
; who care about the result.
; Both results and listeners are expected to be refs of sets.

(defrecord Node [listeners results])
(defn make-node [] (Node. (ref #{}) (ref #{})))

;; Trampoline helper functions

(defn push-stack
  "Pushes an item onto the trampoline's stack"
  [tramp item]
  (swap! (:stack tramp) conj item)) 

(defn node-get
  "Gets node if already exists, otherwise creates one"
  [tramp node-key]
  (let [nodes (:nodes tramp)
        node-key (if (instance? clojure.lang.Var (node-key 1))
                   [(node-key 0) @(node-key 1)]
                   node-key)]
    (if-let [node (@nodes node-key)]
      node 
      (let [node (make-node)]
        (swap! nodes assoc node-key node)
        node))))

(defn push-result
  "Pushes a result into the trampoline's node.
   Schedules notification to all existing listeners of result."
  [tramp node-key result]
  (dosync
    (let [node (node-get tramp node-key)
          results (:results node)]
      (when (not (@results result))  ; when result is not already in @results
        (alter results conj result)
        (doseq [listener @(:listeners node)]
          (push-stack tramp (Notification. listener result))))))) 

(defn push-listener
  "Pushes a listener into the trampoline's node.
   Schedules notification to listener of all existing results."
  [tramp node-key listener]
  (dosync
    (let [node (node-get tramp node-key)
          listeners (:listeners node)]
      (when (not (@listeners listener))  ; when listener is not already in listeners
        (alter listeners conj listener)
        (doseq [result @(:results node)]
          (push-stack tramp (Notification. listener result))))))) 

(defn success [tramp node-key result end]
  (push-result tramp node-key (make-success result end)))

(defn fail [tramp index]  
  (swap! (:failure tramp) (fn [i] (max i index)))) 

;; Stack helper functions

;(defn step
;  "Executes one thing on the stack"
;  [stack]
;  ; It's a little tricky to atomically pop an item off of an atom-based stack
;  ; We need to get down-and-dirty with the low-level compare-and-set! function.
;  ; If step ends up being used from only one thread, this can be simplified.
;  (let [current-stack @stack
;        top (peek current-stack)
;        new-stack (pop current-stack)]
;    (if (and top (compare-and-set! stack current-stack new-stack))
;      (execute top)
;      (recur))))

(defn step
  "Executes one thing on the stack (not threadsafe)"
  [tramp]
  (let [stack (:stack tramp), top (peek @stack)]
    (swap! stack pop)
    (execute top tramp)))

(defn run
  "Executes the stack until exhausted"
  [tramp]
  (let [stack (:stack tramp)]
    (while (pos? (count @stack))
      (step tramp))))

;; Listeners

; There are three kinds of listeners that receive notifications
; The first kind is a NodeListener which simply listens for a completed parse result
; Takes the node-key of the parser which is awaiting this result.

(defrecord NodeListener [node-key]
  Listener
  (notify [_ result tramp] (push-result tramp node-key result)))

; The second kind of listener is a CatListener which listens at each stage of the
; concatenation parser to carry on the next step.  Think of it as a parse continuation.
; A CatListener needs to know the sequence of results for the parsers that have come
; before, and a list of parsers that remain.  Also, the node-key of the final node
; that needs to know the overall result of the cat parser.

(defrecord CatListener [results-so-far parser-sequence node-key]
  Listener
  (notify [_ result tramp] 
    (let [{parsed-result :result continue-index :index} result
          new-results-so-far (conj results-so-far parsed-result)]
      (if (seq parser-sequence)
        (do (push-listener tramp [continue-index (first parser-sequence)]
                           (CatListener. new-results-so-far (next parser-sequence) node-key))
          (push-stack tramp (ParseTask. (first parser-sequence) continue-index)))
        (push-result tramp node-key (make-success new-results-so-far continue-index))))))

; The top level listener

(def TopListener 
  (reify
    Listener
    (notify [_ result tramp] 
      (when (total-success? tramp result)
        (swap! (:success tramp) conj result)))))

;; Parsers

(extend-type String Parser
  (-parse [string index tramp]
    (let [text (:text tramp)
          end (min (count text) (+ index (count string)))
          head (subs text index end)]      
      (if (= string head)
        (success tramp [index string] string end)
        (fail tramp index)))))

(defrecord Cat [parsers]
  Parser
  (-parse [this index tramp]
    ; Kick-off the first parser, with a CatListener ready to pass the result on in the chain
    ; and with a final target of notifying this parser when the whole sequence is complete
    (push-listener tramp [index (first parsers)] (CatListener. [] (next parsers) [index this]))
    (push-stack tramp (ParseTask. (first parsers) index))))

(defrecord Alt [parsers]
  Parser
  (-parse [this index tramp]
    (doseq [parser parsers]
      (push-listener tramp [index parser] (NodeListener. [index this]))
      (push-stack tramp (ParseTask. parser index)))))

(def Epsilon
  (reify Parser
    (-parse [this index tramp] (success tramp [index this] nil index))))

(def End
  (reify Parser
    (-parse [this index tramp] (if (= index (count (:text tramp)))
                                 (success tramp [index this] nil index)
                                 (fail tramp index)))))
    
(extend-type clojure.lang.Var
  Parser
  (-parse [this index tramp]
    (-parse (deref this) index tramp)))

(defn alt [& parsers] (Alt. parsers))
(defn cat [& parsers] (Cat. parsers))

(defn parse [parser text]
  (let [tramp (make-tramp text)]
    (push-listener tramp [0 parser] TopListener)
    (push-stack tramp (ParseTask. parser 0))
    (run tramp)
    (if @(:success tramp) @(:success tramp) @(:failure tramp))))

(def a (Alt. [(Cat. ["a" #'a]) "a"]))
(def c (Alt. ["a" "b"]))
(def b (Alt. ["a" "aa" "aaa"]))

(declare x y)
(def y "b")
(def x (cat "a" #'y))

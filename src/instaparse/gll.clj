(ns instaparse.gll)

; The trampoline structure contains the text to parse, a stack and a nodes
; text is a string
; stack is an atom of a vector containing items implementing the Execute protocol.
; nodes is an atom containing a map from [index parser] pairs to Nodes
; success contains a successful parse
; failure contains the index of the furthest-along failure

(defrecord Tramp [text stack nodes success failure])
(defn make-tramp [text] (Tramp. text (atom []) (atom {}) (atom nil) (atom 0)))
  
; A Success record contains the result and the index to continue from
(defrecord Success [result index])
(defn total-success? [tramp s]
  (= (count (:text tramp)) (:index s)))

; Task is the protocol used by items on the stack

(defprotocol Task (execute [_ tramp]))

; NotificationTask is one kind of thing that can live on the stack
; A NotificationTask is comprised of a Listener and a result to send to the listener. 

(defprotocol Listener (notify [listener result]))

(defrecord Notification [listener result]
  Task
  (execute [_ tramp] (notify listener result)))

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
  (let [nodes (:nodes tramp)]
    (if-let [node (@nodes node-key)]
      node 
      (swap! nodes assoc node-key (make-node)))))

(defn push-result
  "Pushes a result into the trampoline's node.
   Schedules notification to all existing listeners of result."
  [tramp node-key result]
  (let [node (node-get tramp node-key)
        results (:results node)]
    (when (not (@results result))  ; when result is not already in @results
      (swap! results conj result)
      (doseq [listener @(:listeners node)]
        (push-stack tramp (Notification. listener result)))))) 

(defn push-listener
  "Pushes a listener into the trampoline's node.
   Schedules notification to listener of all existing results."
  [tramp node-key listener]
  (let [node (node-get tramp node-key)
        listeners (:listeners node)]
    (when (not (@listeners listener))  ; when listener is not already in listeners
      (swap! listeners conj listener)
      (doseq [result @(:results node)]
        (push-stack tramp (Notification. listener result)))))) 

(defn success [tramp index this string end]
  (push-result tramp [index this] (Success. string end)))

(defn fail [tramp index]  
  (swap! (:fail tramp) (fn [i] (max i index)))) 

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
  [stack]
  (let [top (peek @stack)]
    (swap! stack pop)
    (execute top)))

(defn run
  "Executes the stack until exhausted"
  [stack]
  (while (pos? (count stack))
    (step stack)))






(defprotocol Parser
   ;; parse a text starting at index
  (-parse [this index tramp]))

(defrecord Literal [string]
  Parser
  (-parse [this index tramp]
    (let [text (:text tramp)
          end (min (count text) (+ index (count string)))
          head (subs index end)]      
      (if (= string head)
        (success tramp index this string end)
        (fail tramp index)))))

(defrecord Cat [parsers])


(ns instaparse.gll)

; The trampoline structure contains a stack and a nodes
; stack is an atom of a vector containing items implementing the Execute protocol.
; nodes is an atom containing a map from [index parser] pairs to Nodes

(defrecord Tramp [stack nodes])
(defn make-tramp [] (Tramp. (atom []) (atom {})))

; Task is the protocol used by items on the stack

(defprotocol Task (execute [_]))

; NotificationTask is one kind of thing that can live on the stack
; A NotificationTask is comprised of a Listener and a result to send to the listener. 

(defprotocol Listener (notify [listener result]))

(defrecord Notification [listener result]
  Task
  (execute [_] (notify listener result)))

; ParseTask is another thing that can live on the stack

(defrecord ParseTask [parser index])

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

(defn push-result
  "Pushes a result into the trampoline's nodes"
  [tramp index parser result]
  (let [node (node-get tramp [tramp index])
        results (:results node)]
    (when (not (@results result))  ;result is not already in @results
      (swap! results conj result)
      (doseq [listener @(:listeners node)]
        (push-stack tramp (Notification. listener result))) 

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
  (-parse [this tramp text index]))

(defrecord Literal [string]
  Parser
  (-parse [this tramp text index] 
    (let [end (min (count text) (+ index (count string)))
          head (subs index end)]      
      (if (= string head)
        (success this tramp text index string end)
        (fail tramp index)))))

(defrecord Cat [parsers])


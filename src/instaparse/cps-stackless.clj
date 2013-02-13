(ns instaparse.cps-stackless
  (:use instaparse.trampoline-stackless))

(defn success
  [val rest]
  {:type :success, :val val, :rest rest})

(defn failure
  [rest]
  {:type :failure, :rest rest})

(defn success?
  [obj]
  (and (map? obj)
       (= (:type obj) :success)))

(defn failure?
  [obj]
  (and (map? obj)
       (= (:type obj) :failure)))
        

(def p-succeed
  (memoize
    (fn [val]
      (fn [str tramp cont]
        (push-stack tramp cont (success val str))))))

(def p-string
  (memoize (fn [match]
             (fn [str tramp cont]
               (let [len (min (count str) (count match))
                     head (subs str 0 len)
                     tail (subs str len)]
                 (if (= head match)
                   (push-stack tramp cont (success head tail))
                   (push-stack tramp cont (failure str))))))))

(def p-alt
  (memoize (fn [a b]
             (fn [str tramp cont]
               ;(printf "Trying first alternative on %s\n" str)
               (push tramp a str cont)
               ;(printf "Trying second alternative on %s\n" str)
               (push tramp b str cont)))))

(defn p-bind
  [p f]
  (fn [str tramp cont]
    (p str tramp
       (fn [result]
         (let [{val :val rest :rest} result]
           (if (success? result)
             ((f val) rest tramp cont)
             (push-stack tramp cont result)))))))

(def p-seq
  (memoize (fn [a b]
             (p-bind a (fn [x]
                         (p-bind b (fn [y]
                                     (p-succeed (vector x y)))))))))

(defn run-parser
  [parser tramp s]
  (let [results (atom [])]
    (parser s tramp (fn [result]
                      (if (and (success? result) (empty? (:rest result)))
                        (swap! results conj result))))
    (run tramp)
    @results))

(defmacro defparser
  [parser body]
  `(defn ~parser
     [& args#]
     (apply ~body args#)))

;;;;;;;;;; Random Junk

(defparser a
  (p-alt  (p-seq (p-string "a") a)
          (p-string "a")))

(defparser a2
  (p-alt  (p-seq a (p-string "a") )
          (p-string "a")))


(defparser abc
  (p-alt (p-seq (p-string "a") abc)
         (p-string "c")))

(defparser s
  (p-alt (p-string "b")
         (p-alt (p-seq s s)
                (p-seq (p-seq s s) s))))           
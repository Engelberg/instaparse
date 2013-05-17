(ns instaparse.core
  (:use instaparse.clone)
  (:require [instaparse.gll :as gll] 
            [instaparse.cfg :as cfg]
            [instaparse.failure :as fail]
            [instaparse.print :as print]
            [instaparse.reduction :as red]
            [instaparse.transform :as t]
            [instaparse.abnf :as abnf]))
  ;(:use clojure.tools.trace)

(def ^:dynamic *default-output-format* :hiccup)
(defn set-default-output-format!
  "Changes the default output format.  Input should be :hiccup or :enlive"
  [type]
  {:pre [(#{:hiccup :enlive} type)]}
  (alter-var-root #'*default-output-format* (constantly type)))

(def ^:dynamic *default-input-format* :ebnf)
(defn set-default-input-format!
  "Changes the default input format.  Input should be :abnf or :ebnf"
  [type]
  {:pre [(#{:abnf :ebnf} type)]}
  (alter-var-root #'*default-input-format* (constantly type)))
    
(defn parse 
  "Use parser to parse the text.  Returns first parse tree found
   that completely parses the text.  If no parse tree is possible, returns
   a Failure object.
   
   Optional keyword arguments:
   :start :keyword  (where :keyword is name of starting production rule)
   :partial true    (parses that don't consume the whole string are okay)
   :total true      (if parse fails, embed failure node in tree)"
  [parser text &{:as options}]
  (let [start-production 
        (get options :start (:start-production parser)),
        
        partial?
        (get options :partial false)]
    
    (cond
      (:total options)
      (gll/parse-total (:grammar parser) start-production text 
                       partial? (red/node-builders (:output-format parser)))

      :else
      (gll/parse (:grammar parser) start-production text partial?))))
  
(defn parses 
  "Use parser to parse the text.  Returns lazy seq of all parse trees
   that completely parse the text.  If no parse tree is possible, returns
   () with a Failure object attached as metadata.
   
   Optional keyword arguments:
   :start :keyword  (where :keyword is name of starting production rule)
   :partial true    (parses that don't consume the whole string are okay)
   :total true      (if parse fails, embed failure node in tree)"
  [parser text &{:as options}]
  (let [start-production 
        (get options :start (:start-production parser)),
        
        partial?
        (get options :partial false)]

    (cond
      (:total options)
      (gll/parses-total (:grammar parser) start-production text 
                        partial? (red/node-builders (:output-format parser)))
      
      :else
      (gll/parses (:grammar parser) start-production text partial?))))
  
(defrecord Parser [grammar start-production output-format]
  clojure.lang.IFn
  (invoke [parser text] (parse parser text))
  (invoke [parser text key1 val1] (parse parser text key1 val1))
  (invoke [parser text key1 val1 key2 val2] (parse parser text key1 val1 key2 val2))
  (invoke [parser text key1 val1 key2 val2 key3 val3] (parse parser text key1 val1 key2 val2 key3 val3))
  (applyTo [parser args] (apply parse parser args)))


(defmethod clojure.core/print-method Parser [x writer]
  (binding [*out* writer]
    (println (print/Parser->str x))))

(defn parser
  "Takes a string specification of a context-free grammar,
   or a URI for a text file containing such a specification,
   or a map of parser combinators and returns a parser for that grammar.

   Optional keyword arguments:
   :input-format :ebnf
   or
   :input-format :abnf

   :output-format :enlive
   or
   :output-format :hiccup
   
   :start :keyword (where :keyword is name of starting production rule)"
  [grammar-specification &{:as options}]
  {:pre [(contains? #{:abnf :ebnf nil} (get options :input-format))
         (contains? #{:enlive :hiccup :lisp nil} (get options :output-format))]}
  (let [input-format (get options :input-format *default-input-format*)
        build-parser (case input-format 
                       :abnf abnf/build-parser
                       :ebnf cfg/build-parser)
        output-format (get options :output-format *default-output-format*)
        start (get options :start nil)]    
    (cond
      (string? grammar-specification)
      (let [parser
            (try (let [spec (slurp grammar-specification)]
                   (build-parser spec output-format))
              (catch java.io.FileNotFoundException e 
                (build-parser grammar-specification output-format)))]
        (if start (map->Parser (assoc parser :start-production start))
          (map->Parser parser)))
      
      (map? grammar-specification)
      (let [parser
            (cfg/build-parser-from-combinators grammar-specification
                                               output-format
                                               start)]
        (map->Parser parser))
      
      (vector? grammar-specification)
      (let [start (if start start (grammar-specification 0))
            parser
            (cfg/build-parser-from-combinators (apply hash-map grammar-specification)
                                               output-format
                                               start)]
        (map->Parser parser)))))
        

(defn failure?
  "Tests whether a parse result is a failure."
  [result]
  (or
    (instance? instaparse.gll.Failure result)
    (instance? instaparse.gll.Failure (meta result))))

(defn get-failure
  "Extracts failure object from failed parse result."
  [result]
  (cond
    (instance? instaparse.gll.Failure result)
    result
    (instance? instaparse.gll.Failure (meta result))
    (meta result)
    :else
    nil))

(defclone transform t/transform)
(ns instaparse.core
  (:require [instaparse.gll :as gll] 
            [instaparse.cfg :as cfg]
            [instaparse.errors :as err]))

(def ^:dynamic *default-output-format* :hiccup)
(defn set-default-output-format!
  "Changes the default output format.  Input should be :hiccup or :enlive"
  [type]
  {:pre [(#{:hiccup :enlive} type)]}
  (alter-var-root #'*default-output-format* (constantly type)))
    
(defn parse [parser text &{:keys [partial total] :as options}]
  "Use parser to parse the text.  Returns first parse tree found
   that completely parses the text.  If no parse tree is possible, returns
   a Failure object.
   
   Optional keyword arguments:
   :start :keyword  (where :keyword is name of starting production rule)
   :partial true    (parses that don't consume the whole string are okay)
   :total true      (if parse fails, embed failure node in tree)"
  (let [start-production 
        (get options :start (:start-production parser))]
    (gll/parse (:grammar parser)
               start-production
               text)))
  
(defn parses [parser text &{:keys [partial total] :as options}]
  "Use parser to parse the text.  Returns lazy seq of all parse trees
   that completely parse the text.  If no parse tree is possible, returns
   () with a Failure object attached as metadata.
   
   Optional keyword arguments:
   :start :keyword  (where :keyword is name of starting production rule)
   :partial true    (parses that don't consume the whole string are okay)
   :total true      (if parse fails, embed failure node in tree)"
  (let [start-production 
        (get options :start (:start-production parser))]
    (gll/parses (:grammar parser)
                start-production
                text)))
  
(defrecord Parser [grammar start-production]
  clojure.lang.IFn
  (invoke [parser text] (parse parser text))
  (invoke [parser text key1 val1] (parse parser text key1 val1))
  (invoke [parser text key1 val1 key2 val2] (parse parser text key1 val1 key2 val2))
  (invoke [parser text key1 val1 key2 val2 key3 val3] (parse parser text key1 val1 key2 val2 key3 val3)))

(defn parser
  "Takes a string specification of a context-free grammar,
   or a URI for a text file containing such a specification,
   and returns a parser for that grammar.

   Optional keyword argument:
   :output-format :enlive
   or
   :output-format :hiccup"
  [string-or-uri &{:as options}]
  (let [output-format (get options :output-format *default-output-format*)] 
    (map->Parser
      (try (let [spec (slurp string-or-uri)]
             (cfg/build-parser spec output-format))
        (catch java.io.FileNotFoundException e 
          (cfg/build-parser string-or-uri output-format))))))

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

(defn pprint-failure
  "Pretty-prints failure message for failed parse result to standard output.
   If input is anything other than a failed parse result, nothing is printed.
   Either way, the function returns the input unchanged, so this function 
   can be used as an identity function in a chained series of functions."
  [result]
  (when (failure? result)
    (err/pprint-failure (get-failure result))))
    
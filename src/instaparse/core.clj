(ns instaparse.core
  (:require [instaparse.gll :as gll] 
            [instaparse.cfg :as cfg]
            [instaparse.errors :as err]))

(defprotocol IParser
  (parse [parser text] [parser production text]
    "Use parser to parse the text.  Returns first parse tree found
     that completely parses the text.  If no parse tree is possible, returns
     a Failure object.
     
     By default, parsing begins with the first production rule, but optionally 
     you can provide a keyword specifying another production to start from.")
  (parses [parser text] [parser production text]
     "Use parser to parse the text.  Returns a lazy sequence of all parse trees
     that completely parse the text.  If no parse tree is possible, returns
     () with a Failure object attached as metadata.
     
     By default, parsing begins with the first production rule, but optionally 
     you can provide a keyword specifying another production to start from."))     
    
(defrecord Parser [grammar start-production]
  IParser
  (parse [parser text] (gll/parse (:grammar parser)
                              (:start-production parser)
                              text))
  (parse [parser production text] (gll/parse (:grammar parser)
                                         production
                                         text))
  (parses [parser text] (gll/parses (:grammar parser)
                              (:start-production parser)
                              text))
  (parses [parser production text] (gll/parse (:grammar parser)
                                         production
                                         text))
  
  clojure.lang.IFn
  (invoke [parser text] (parse parser text))
  (invoke [parser production text] (parse parser production text)))
  

(defn parser
  "Takes a string specification of a context-free grammar,
   or a URI for a text file containing such a specification,
   and returns a parser for that grammar."
  [string-or-uri]
  (map->Parser
    (try (let [spec (slurp string-or-uri)]
           (cfg/build-parser spec))
      (catch java.io.FileNotFoundException e 
        (cfg/build-parser string-or-uri)))))

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

(defn report-failure
  "Pretty-prints failure message for failed parse result to standard output.
   If input is anything other than a failed parse result, nothing is printed.
   Either way, the function returns the input unchanged, so this function 
   can be used as an identity function in a chained series of functions."
  [result]
  (when (failure? result)
    (err/pprint-failure result))
  result)
    
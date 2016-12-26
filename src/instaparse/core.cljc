(ns instaparse.core
  (#?(:clj :require :cljs :require-macros)
    [instaparse.macros :refer [defclone
                               set-global-var!]])
  (:require [clojure.walk :as walk]
            [instaparse.gll :as gll]
            [instaparse.cfg :as cfg]
            [instaparse.failure :as fail]
            [instaparse.print :as print]
            [instaparse.reduction :as red]
            [instaparse.transform :as t]
            [instaparse.abnf :as abnf]
            [instaparse.repeat :as repeat]
            [instaparse.combinators-source :as c]
            [instaparse.line-col :as lc]
            [instaparse.viz :as viz]
            [instaparse.util :refer [throw-illegal-argument-exception]]))

(def ^:dynamic *default-output-format* :hiccup)
(defn set-default-output-format!
  "Changes the default output format.  Input should be :hiccup or :enlive"
  [type]
  {:pre [(#{:hiccup :enlive} type)]}
  (set-global-var! *default-output-format* type))

(def ^:dynamic *default-input-format* :ebnf)
(defn set-default-input-format!
  "Changes the default input format.  Input should be :abnf or :ebnf"
  [type]
  {:pre [(#{:abnf :ebnf} type)]}
  (set-global-var! *default-input-format* type))

(declare failure? standard-whitespace-parsers enable-tracing!)

(defn- unhide-parser [parser unhide]
  (case unhide
    nil parser
    :content 
    (assoc parser :grammar (c/unhide-all-content (:grammar parser)))
    :tags 
    (assoc parser :grammar (c/unhide-tags (:output-format parser) 
                                          (:grammar parser)))
    :all
    (assoc parser :grammar (c/unhide-all (:output-format parser)
                                         (:grammar parser)))))
  
(defn parse 
  "Use parser to parse the text.  Returns first parse tree found
   that completely parses the text.  If no parse tree is possible, returns
   a Failure object.
   
   Optional keyword arguments:
   :start :keyword  (where :keyword is name of starting production rule)
   :partial true    (parses that don't consume the whole string are okay)
   :total true      (if parse fails, embed failure node in tree)
   :unhide <:tags or :content or :all> (for this parse, disable hiding)
   :optimize :memory   (when possible, employ strategy to use less memory)

   Clj only:
   :trace true      (print diagnostic trace while parsing)"
  [parser text &{:as options}]
  {:pre [(contains? #{:tags :content :all nil} (get options :unhide))
         (contains? #{:memory nil} (get options :optimize))]}
  (let [start-production 
        (get options :start (:start-production parser)),
        
        partial?
        (get options :partial false)
        
        optimize?
        (get options :optimize false)
        
        unhide
        (get options :unhide)
        
        trace?
        (get options :trace false)
        
        #?@(:clj [_ (when (and trace? (not gll/TRACE)) (enable-tracing!))])
        
        parser (unhide-parser parser unhide)]
    (->> (cond
           (:total options)
           (gll/parse-total (:grammar parser) start-production text 
                            partial? (red/node-builders (:output-format parser)))

           (and optimize? (not partial?))
           (let [result (repeat/try-repeating-parse-strategy parser text start-production)]
             (if (failure? result)
               (gll/parse (:grammar parser) start-production text partial?)
               result))

           :else
           (gll/parse (:grammar parser) start-production text partial?))

         #?(:clj (gll/bind-trace trace?)))))
  
(defn parses 
  "Use parser to parse the text.  Returns lazy seq of all parse trees
   that completely parse the text.  If no parse tree is possible, returns
   () with a Failure object attached as metadata.
   
   Optional keyword arguments:
   :start :keyword  (where :keyword is name of starting production rule)
   :partial true    (parses that don't consume the whole string are okay)
   :total true      (if parse fails, embed failure node in tree)
   :unhide <:tags or :content or :all> (for this parse, disable hiding)

   Clj only:
   :trace true      (print diagnostic trace while parsing)"
  [parser text &{:as options}]
  {:pre [(contains? #{:tags :content :all nil} (get options :unhide))]}
  (let [start-production 
        (get options :start (:start-production parser)),
        
        partial?
        (get options :partial false)
        
        unhide
        (get options :unhide)
        
        trace?
        (get options :trace false)
        
        #?@(:clj [_ (when (and trace? (not gll/TRACE)) (enable-tracing!))])
        
        parser (unhide-parser parser unhide)]
    (->> (cond
           (:total options)
           (gll/parses-total (:grammar parser) start-production text 
                             partial? (red/node-builders (:output-format parser)))
        
           :else
           (gll/parses (:grammar parser) start-production text partial?))

         #?(:clj (gll/bind-trace trace?)))))
  
(defrecord Parser [grammar start-production output-format]
#?@(:clj
    [clojure.lang.IFn
     (invoke [parser text] (parse parser text))
     (invoke [parser text key1 val1] (parse parser text key1 val1))
     (invoke [parser text key1 val1 key2 val2] (parse parser text key1 val1 key2 val2))
     (invoke [parser text key1 val1 key2 val2 key3 val3] (parse parser text key1 val1 key2 val2 key3 val3))
     (applyTo [parser args] (apply parse parser args))]

    :cljs
    [IFn
     (-invoke [parser text] (parse parser text))
     (-invoke [parser text key1 val1] (parse parser text key1 val1))
     (-invoke [parser text key1 val1 key2 val2] (parse parser text key1 val1 key2 val2))
     (-invoke [parser text key1 val1 key2 val2 key3 val3] (parse parser text key1 val1 key2 val2 key3 val3))
     (-invoke [parser text a b c d e f g h] (parse parser text a b c d e f g h))
     (-invoke [parser text a b c d e f g h i j] (parse parser text a b c d e f g h i j))
     (-invoke [parser text a b c d e f g h i j k l] (parse parser text a b c d e f g h i j k l))
     (-invoke [parser text a b c d e f g h i j k l m n] (parse parser text a b c d e f g h i j k l m n))
     (-invoke [parser text a b c d e f g h i j k l m n o p] (parse parser text a b c d e f g h i j k l m n o p))
     (-invoke [parser text a b c d e f g h i j k l m n o p q r] (parse parser text a b c d e f g h i j k l m n o p))
     (-invoke [parser text a b c d e f g h i j k l m n o p q r s more] (apply parse parser text a b c d e f g h i j k l m n o p q r s more))]))

#?(:clj
   (defmethod clojure.core/print-method Parser [x writer]
     (binding [*out* writer]
       (println (print/Parser->str x))))
   :cljs
   (extend-protocol IPrintWithWriter
     instaparse.core/Parser
     (-pr-writer  [parser writer _]
       (-write writer (print/Parser->str parser)))))

(defn parser
  "Takes a string specification of a context-free grammar,
  or a URI for a text file containing such a specification (Clj only),
  or a map of parser combinators and returns a parser for that grammar.

  Optional keyword arguments:
  :input-format :ebnf
  or
  :input-format :abnf

  :output-format :enlive
  or
  :output-format :hiccup

  :start :keyword (where :keyword is name of starting production rule)

  :string-ci true (treat all string literals as case insensitive)

  :auto-whitespace (:standard or :comma)
  or
  :auto-whitespace custom-whitespace-parser

  Clj only:
  :no-slurp true (disables use of slurp to auto-detect whether
                  input is a URI.  When using this option, input
                  must be a grammar string or grammar map.  Useful
                  for platforms where slurp is slow or not available.)"
  [grammar-specification &{:as options}]
  {:pre [(contains? #{:abnf :ebnf nil} (get options :input-format))
         (contains? #{:enlive :hiccup nil} (get options :output-format))
         (let [ws-parser (get options :auto-whitespace)]
           (or (nil? ws-parser)
               (contains? standard-whitespace-parsers ws-parser)
               (and
                 (map? ws-parser)
                 (contains? ws-parser :grammar)
                 (contains? ws-parser :start-production))))]}
  (let [input-format (get options :input-format *default-input-format*)
        build-parser (case input-format 
                       :abnf abnf/build-parser
                       :ebnf (if (get options :string-ci)
                               (fn [spec output-format]
                                 (binding [cfg/*case-insensitive-literals* true]
                                   (cfg/build-parser spec output-format)))
                               cfg/build-parser))
        output-format (get options :output-format *default-output-format*)
        start (get options :start nil)

        built-parser
        (cond
          (string? grammar-specification)
          (let [parser
                #?(:clj
                   (if (get options :no-slurp)
                     ;; if :no-slurp is set to true, string is a grammar spec
                     (build-parser grammar-specification output-format)                  
                     ;; otherwise, grammar-specification might be a URI,
                     ;; let's slurp to see
                     (try (let [spec (slurp grammar-specification)]
                            (build-parser spec output-format))
                          (catch java.io.FileNotFoundException e 
                            (build-parser grammar-specification output-format))))
                   :cljs
                   (build-parser grammar-specification output-format))]
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
            (map->Parser parser))

          :else
          #?(:clj
             (let [spec (slurp grammar-specification)
                   parser (build-parser spec output-format)]
               (map->Parser parser))
             :cljs
             (throw-illegal-argument-exception
              "Expected string, map, or vector as grammar specification, got "
              (pr-str grammar-specification))))]

    (let [auto-whitespace (get options :auto-whitespace)
          ; auto-whitespace is keyword, parser, or nil
          whitespace-parser (if (keyword? auto-whitespace)
                              (get standard-whitespace-parsers auto-whitespace)
                              auto-whitespace)]
      (if-let [{ws-grammar :grammar ws-start :start-production} whitespace-parser]
        (assoc built-parser :grammar
               (c/auto-whitespace (:grammar built-parser) (:start-production built-parser)
                                  ws-grammar ws-start))
        built-parser))))

#?(:clj
   (defmacro defparser
     "Takes a string specification of a context-free grammar,
  or a URI for a text file containing such a specification,
  or a map of parser combinators and sets a variable to a parser for that grammar.

  String specifications are processed at macro-time, not runtime, so this is an
  appealing alternative to (def _ (parser \"...\")) for ClojureScript users.

  Optional keyword arguments unique to `defparser`:
  - :instaparse.abnf/case-insensitive true"
     [name grammar & {:as opts}]
     (if (string? grammar)
       `(def ~name
          (map->Parser
           ~(binding [abnf/*case-insensitive* (:instaparse.abnf/case-insensitive opts false)]
              (let [macro-time-opts (select-keys opts [:input-format :output-format
                                                       :string-ci :start])
                    runtime-opts (dissoc opts :start)
                    macro-time-parser (apply parser grammar (apply concat macro-time-opts))
                    pre-processed-grammar (:grammar macro-time-parser)

                    grammar-producing-code
                    (->> pre-processed-grammar
                         (walk/postwalk
                           (fn [form]
                             (cond
                               ;; Lists cannot be evaluated verbatim
                               (seq? form)
                               (list* 'list form)

                               ;; Regexp terminals are handled differently in cljs
                               (= :regexp (:tag form))
                               `(merge (c/regexp ~(str (:regexp form)))
                                       ~(dissoc form :tag :regexp))

                               :else form))))

                    start-production
                    (:start-production macro-time-parser)]
                `(parser ~grammar-producing-code
                         :start ~start-production
                         ~@(apply concat runtime-opts))))))
       `(def ~name (parser ~grammar ~@(apply concat opts))))))
        
(defn failure?
  "Tests whether a parse result is a failure."
  [result]
  (or
    (instance? gll/failure-type result)
    (instance? gll/failure-type (meta result))))

(defn get-failure
  "Extracts failure object from failed parse result."
  [result]
  (cond
    (instance? gll/failure-type result)
    result
    (instance? gll/failure-type (meta result))
    (meta result)
    :else
    nil))

(def ^:private standard-whitespace-parsers
  {:standard (parser "whitespace = #'\\s+'")
   :comma (parser "whitespace = #'[,\\s]+'")})

#?(:clj
   (defn enable-tracing!
     "Recompiles instaparse with tracing enabled.
  This is called implicitly the first time you invoke a parser with
  `:trace true` so usually you will not need to call this directly."
     []
     (alter-var-root #'gll/TRACE (constantly true))
     (alter-var-root #'gll/PROFILE (constantly true))
     (require 'instaparse.gll :reload)))

#?(:clj
   (defn disable-tracing!
     "Recompiles instaparse with tracing disabled.
  Call this to restore regular performance characteristics, eliminating
  the small performance hit imposed by tracing."
     []
     (alter-var-root #'gll/TRACE (constantly false))
     (alter-var-root #'gll/PROFILE (constantly false))
     (require 'instaparse.gll :reload)))
   
(defclone transform t/transform)

(defclone add-line-and-column-info-to-metadata lc/add-line-col-spans)

(defclone span viz/span)

#?(:clj (defclone visualize viz/tree-viz))

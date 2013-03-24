(ns instaparse.cfg
  (:use instaparse.gll)
  (:use clojure.pprint clojure.repl))
;
;(def single-quoted-string "'(\\'|[^'])*'")
;(def single-quoted-string "'([^']|(?<!\\\\)')*?'") 
;(def single-quoted-string #"'[^\\]*?'")
;(def single-quoted-string #"'([^\\']|\\\\|\\')*'")
;(def single-quoted-string #"'([^']|\\')*'")
(def single-quoted-string #"'(?:[^\\']|\\.)*'")
(def single-quoted-regexp #"#'(?:[^\\']|\\.)*'")
(def double-quoted-string #"\"(?:[^\\\"]|\\.)*\"")
(def double-quoted-regexp #"#\"(?:[^\\\"]|\\.)*\"")

(def opt-whitespace (hide (nt :opt-whitespace)))

(def cfg 
  (apply-standard-reductions 
    {:rules (hide-tag (cat opt-whitespace
                           (plus (nt :rule))))
     :whitespace (regexp "[,\\s]+")
     :opt-whitespace (regexp "[,\\s]*")
     :rule-separator (alt (string ":")
                          (string ":=")
                          (string "::=")
                          (string "="))
     :rule (cat (alt (nt :nt)
                     (nt :hide-nt))
                opt-whitespace
                (hide (nt :rule-separator))
                opt-whitespace
                (nt :alt-or-ord)
                (hide (alt (nt :opt-whitespace)
                           (regexp "\\s*[.;]\\s*"))))          
     :nt (cat
           (neg (nt :epsilon))
           (regexp "[^, \\r\\t\\n<>(){}\\[\\]+*?:=|'\"#&!;./]+"))
          :hide-nt (cat (hide (string "<"))
                        opt-whitespace
                        (nt :nt)
                        opt-whitespace
                        (hide (string ">")))
          :alt-or-ord (hide-tag (alt (nt :alt) (nt :ord)))
          :alt (cat (nt :cat)                           
                    (star
                      (cat
                        opt-whitespace
                        (hide (string "|"))
                        opt-whitespace
                        (nt :cat))))
          :ord (cat (nt :cat)
                    (plus
                      (cat
                        opt-whitespace
                        (hide (string "/"))
                        opt-whitespace
                        (nt :cat))))
          :paren (cat (hide (string "("))
                      opt-whitespace
                      (nt :alt-or-ord)
                      opt-whitespace
                      (hide (string ")")))
          :hide (cat (hide (string "<"))
                     opt-whitespace	
                     (nt :alt-or-ord)
                     opt-whitespace
                     (hide (string ">")))
          :cat (plus (cat
                       opt-whitespace
                       (alt (nt :factor) (nt :look) (nt :neg))
                       opt-whitespace))
          :string (alt
                    (regexp single-quoted-string)
                    (regexp double-quoted-string))
          :regexp (alt
                    (regexp single-quoted-regexp)
                    (regexp double-quoted-regexp))
          :opt (alt
                 (cat (hide (string "["))
                      opt-whitespace
                      (nt :alt-or-ord)
                      opt-whitespace
                      (hide (string "]")))
                 (cat (nt :factor)
                      opt-whitespace
                      (hide (string "?"))))
          :star (alt
                  (cat (hide (string "{"))
                       opt-whitespace
                       (nt :alt-or-ord)
                       opt-whitespace
                       (hide (string "}")))
                  (cat (nt :factor)
                       opt-whitespace
                       (hide (string "*"))))
          :plus (cat (nt :factor)
                     opt-whitespace
                     (hide (string "+")))
          :look (cat (hide (string "&"))
                     opt-whitespace
                     (nt :factor))
          :neg (cat (hide (string "!"))
                    opt-whitespace
                    (nt :factor))
          :epsilon (alt (string "Epsilon")
                        (string "epsilon")
                        (string "EPSILON")
                        (string "eps")
                        (string "\u03b5"))
          :factor (hide-tag (alt (nt :nt)
                                 (nt :string)
                                 (nt :regexp)
                                 (nt :opt)     
                                 (nt :star)
                                 (nt :plus)
                                 (nt :paren)
                                 (nt :hide)
                                 (nt :epsilon)))}))
  
(def tag first)
(def contents next)
(def content fnext)

(defn process-string
  "Converts single quoted string to double-quoted"
  [s]
  (let [stripped
        (subs s 1 (dec (count s)))
        remove-escape
        (clojure.string/replace stripped "\\'" "'")]
    remove-escape))

(defn build-rule [tree]
  ;(println tree)
  (case (tag tree)
    :rule (let [[nt alt-or-ord] (contents tree)]
            (if (= (tag nt) :hide-nt)
              [(keyword (content (content nt)))
               (hide-tag (build-rule alt-or-ord))]
              [(keyword (content nt))
               (build-rule alt-or-ord)]))
    :nt (nt (keyword (content tree)))
    :alt (apply alt (map build-rule (contents tree)))
    :ord (apply ord (map build-rule (contents tree)))
    :paren (recur (content tree))
    :hide (hide (nt (content tree)))
    :cat (apply cat (map build-rule (contents tree)))
    :string (string (process-string (content tree))) ;TBD escape chars
    :regexp (regexp (process-string (content tree)))
    :opt (opt (build-rule (content tree)))
    :star (star (build-rule (content tree)))
    :plus (plus (build-rule (content tree)))
    :look (look (build-rule (content tree)))
    :neg (neg (build-rule (content tree)))
    :epsilon Epsilon))
    
;; TBD Check grammar
(defn build-parser [spec]
  (if-let [rules (parse cfg :rules spec)]    
    (let [productions (map build-rule rules)
          start-production (first (first productions))] 
      {:grammar (apply-standard-reductions (into {} productions))
       :start-production start-production})))


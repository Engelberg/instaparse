(ns instaparse.abnf-test
  (:use clojure.test)
  (:use instaparse.core))

(deftest abnf-uri
  (let [uri-parser (binding [instaparse.abnf/*case-insensitive* true]
                     (parser (slurp "test/instaparse/abnf_uri.txt") :input-format :abnf))]
    (are [x y] (= x y)
         (uri-parser "http://www.google.com")
         [:URI [:SCHEME [:ALPHA "h"] [:ALPHA "t"] [:ALPHA "t"] [:ALPHA "p"]] ":" [:HIER-PART "//" [:AUTHORITY [:HOST [:REG-NAME [:UNRESERVED [:ALPHA "w"]] [:UNRESERVED [:ALPHA "w"]] [:UNRESERVED [:ALPHA "w"]] [:UNRESERVED "."] [:UNRESERVED [:ALPHA "g"]] [:UNRESERVED [:ALPHA "o"]] [:UNRESERVED [:ALPHA "o"]] [:UNRESERVED [:ALPHA "g"]] [:UNRESERVED [:ALPHA "l"]] [:UNRESERVED [:ALPHA "e"]] [:UNRESERVED "."] [:UNRESERVED [:ALPHA "c"]] [:UNRESERVED [:ALPHA "o"]] [:UNRESERVED [:ALPHA "m"]]]]] [:PATH-ABEMPTY]]]
         
         (uri-parser "ftp://ftp.is.co.za/rfc/rfc1808.txt")
         [:URI [:SCHEME [:ALPHA "f"] [:ALPHA "t"] [:ALPHA "p"]] ":" [:HIER-PART "//" [:AUTHORITY [:HOST [:REG-NAME [:UNRESERVED [:ALPHA "f"]] [:UNRESERVED [:ALPHA "t"]] [:UNRESERVED [:ALPHA "p"]] [:UNRESERVED "."] [:UNRESERVED [:ALPHA "i"]] [:UNRESERVED [:ALPHA "s"]] [:UNRESERVED "."] [:UNRESERVED [:ALPHA "c"]] [:UNRESERVED [:ALPHA "o"]] [:UNRESERVED "."] [:UNRESERVED [:ALPHA "z"]] [:UNRESERVED [:ALPHA "a"]]]]] [:PATH-ABEMPTY "/" [:SEGMENT [:PCHAR [:UNRESERVED [:ALPHA "r"]]] [:PCHAR [:UNRESERVED [:ALPHA "f"]]] [:PCHAR [:UNRESERVED [:ALPHA "c"]]]] "/" [:SEGMENT [:PCHAR [:UNRESERVED [:ALPHA "r"]]] [:PCHAR [:UNRESERVED [:ALPHA "f"]]] [:PCHAR [:UNRESERVED [:ALPHA "c"]]] [:PCHAR [:UNRESERVED [:DIGIT "1"]]] [:PCHAR [:UNRESERVED [:DIGIT "8"]]] [:PCHAR [:UNRESERVED [:DIGIT "0"]]] [:PCHAR [:UNRESERVED [:DIGIT "8"]]] [:PCHAR [:UNRESERVED "."]] [:PCHAR [:UNRESERVED [:ALPHA "t"]]] [:PCHAR [:UNRESERVED [:ALPHA "x"]]] [:PCHAR [:UNRESERVED [:ALPHA "t"]]]]]]]
         
         (uri-parser "mailto:John.Doe@example.com")
         [:URI [:SCHEME [:ALPHA "m"] [:ALPHA "a"] [:ALPHA "i"] [:ALPHA "l"] [:ALPHA "t"] [:ALPHA "o"]] ":" [:HIER-PART [:PATH-ROOTLESS [:SEGMENT-NZ [:PCHAR [:UNRESERVED [:ALPHA "J"]]] [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "h"]]] [:PCHAR [:UNRESERVED [:ALPHA "n"]]] [:PCHAR [:UNRESERVED "."]] [:PCHAR [:UNRESERVED [:ALPHA "D"]]] [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "e"]]] [:PCHAR "@"] [:PCHAR [:UNRESERVED [:ALPHA "e"]]] [:PCHAR [:UNRESERVED [:ALPHA "x"]]] [:PCHAR [:UNRESERVED [:ALPHA "a"]]] [:PCHAR [:UNRESERVED [:ALPHA "m"]]] [:PCHAR [:UNRESERVED [:ALPHA "p"]]] [:PCHAR [:UNRESERVED [:ALPHA "l"]]] [:PCHAR [:UNRESERVED [:ALPHA "e"]]] [:PCHAR [:UNRESERVED "."]] [:PCHAR [:UNRESERVED [:ALPHA "c"]]] [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "m"]]]]]]]
         
         (uri-parser "tel:+1-816-555-1212")
         [:URI [:SCHEME [:ALPHA "t"] [:ALPHA "e"] [:ALPHA "l"]] ":" [:HIER-PART [:PATH-ROOTLESS [:SEGMENT-NZ [:PCHAR [:SUB-DELIMS "+"]] [:PCHAR [:UNRESERVED [:DIGIT "1"]]] [:PCHAR [:UNRESERVED "-"]] [:PCHAR [:UNRESERVED [:DIGIT "8"]]] [:PCHAR [:UNRESERVED [:DIGIT "1"]]] [:PCHAR [:UNRESERVED [:DIGIT "6"]]] [:PCHAR [:UNRESERVED "-"]] [:PCHAR [:UNRESERVED [:DIGIT "5"]]] [:PCHAR [:UNRESERVED [:DIGIT "5"]]] [:PCHAR [:UNRESERVED [:DIGIT "5"]]] [:PCHAR [:UNRESERVED "-"]] [:PCHAR [:UNRESERVED [:DIGIT "1"]]] [:PCHAR [:UNRESERVED [:DIGIT "2"]]] [:PCHAR [:UNRESERVED [:DIGIT "1"]]] [:PCHAR [:UNRESERVED [:DIGIT "2"]]]]]]]
         
         (uri-parser "telnet://192.0.2.16:80/")
         [:URI [:SCHEME [:ALPHA "t"] [:ALPHA "e"] [:ALPHA "l"] [:ALPHA "n"] [:ALPHA "e"] [:ALPHA "t"]] ":" [:HIER-PART "//" [:AUTHORITY [:HOST [:REG-NAME [:UNRESERVED [:DIGIT "1"]] [:UNRESERVED [:DIGIT "9"]] [:UNRESERVED [:DIGIT "2"]] [:UNRESERVED "."] [:UNRESERVED [:DIGIT "0"]] [:UNRESERVED "."] [:UNRESERVED [:DIGIT "2"]] [:UNRESERVED "."] [:UNRESERVED [:DIGIT "1"]] [:UNRESERVED [:DIGIT "6"]]]] ":" [:PORT [:DIGIT "8"] [:DIGIT "0"]]] [:PATH-ABEMPTY "/" [:SEGMENT]]]]
         
         (uri-parser "urn:oasis:names:specification:docbook:dtd:xml:4.1.2")
         [:URI [:SCHEME [:ALPHA "u"] [:ALPHA "r"] [:ALPHA "n"]] ":" [:HIER-PART [:PATH-ROOTLESS [:SEGMENT-NZ [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "a"]]] [:PCHAR [:UNRESERVED [:ALPHA "s"]]] [:PCHAR [:UNRESERVED [:ALPHA "i"]]] [:PCHAR [:UNRESERVED [:ALPHA "s"]]] [:PCHAR ":"] [:PCHAR [:UNRESERVED [:ALPHA "n"]]] [:PCHAR [:UNRESERVED [:ALPHA "a"]]] [:PCHAR [:UNRESERVED [:ALPHA "m"]]] [:PCHAR [:UNRESERVED [:ALPHA "e"]]] [:PCHAR [:UNRESERVED [:ALPHA "s"]]] [:PCHAR ":"] [:PCHAR [:UNRESERVED [:ALPHA "s"]]] [:PCHAR [:UNRESERVED [:ALPHA "p"]]] [:PCHAR [:UNRESERVED [:ALPHA "e"]]] [:PCHAR [:UNRESERVED [:ALPHA "c"]]] [:PCHAR [:UNRESERVED [:ALPHA "i"]]] [:PCHAR [:UNRESERVED [:ALPHA "f"]]] [:PCHAR [:UNRESERVED [:ALPHA "i"]]] [:PCHAR [:UNRESERVED [:ALPHA "c"]]] [:PCHAR [:UNRESERVED [:ALPHA "a"]]] [:PCHAR [:UNRESERVED [:ALPHA "t"]]] [:PCHAR [:UNRESERVED [:ALPHA "i"]]] [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "n"]]] [:PCHAR ":"] [:PCHAR [:UNRESERVED [:ALPHA "d"]]] [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "c"]]] [:PCHAR [:UNRESERVED [:ALPHA "b"]]] [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "k"]]] [:PCHAR ":"] [:PCHAR [:UNRESERVED [:ALPHA "d"]]] [:PCHAR [:UNRESERVED [:ALPHA "t"]]] [:PCHAR [:UNRESERVED [:ALPHA "d"]]] [:PCHAR ":"] [:PCHAR [:UNRESERVED [:ALPHA "x"]]] [:PCHAR [:UNRESERVED [:ALPHA "m"]]] [:PCHAR [:UNRESERVED [:ALPHA "l"]]] [:PCHAR ":"] [:PCHAR [:UNRESERVED [:DIGIT "4"]]] [:PCHAR [:UNRESERVED "."]] [:PCHAR [:UNRESERVED [:DIGIT "1"]]] [:PCHAR [:UNRESERVED "."]] [:PCHAR [:UNRESERVED [:DIGIT "2"]]]]]]]
         
         (uri-parser "ldap://[2001:db8::7]/c=GB?objectClass?one")
         [:URI [:SCHEME [:ALPHA "l"] [:ALPHA "d"] [:ALPHA "a"] [:ALPHA "p"]] ":" [:HIER-PART "//" [:AUTHORITY [:HOST [:IP-LITERAL "[" [:IPV6ADDRESS [:H16 [:HEXDIG "2"] [:HEXDIG "0"] [:HEXDIG "0"] [:HEXDIG "1"]] ":" [:H16 [:HEXDIG "d"] [:HEXDIG "b"] [:HEXDIG "8"]] "::" [:H16 [:HEXDIG "7"]]] "]"]]] [:PATH-ABEMPTY "/" [:SEGMENT [:PCHAR [:UNRESERVED [:ALPHA "c"]]] [:PCHAR [:SUB-DELIMS "="]] [:PCHAR [:UNRESERVED [:ALPHA "G"]]] [:PCHAR [:UNRESERVED [:ALPHA "B"]]]]]] "?" [:QUERY [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "b"]]] [:PCHAR [:UNRESERVED [:ALPHA "j"]]] [:PCHAR [:UNRESERVED [:ALPHA "e"]]] [:PCHAR [:UNRESERVED [:ALPHA "c"]]] [:PCHAR [:UNRESERVED [:ALPHA "t"]]] [:PCHAR [:UNRESERVED [:ALPHA "C"]]] [:PCHAR [:UNRESERVED [:ALPHA "l"]]] [:PCHAR [:UNRESERVED [:ALPHA "a"]]] [:PCHAR [:UNRESERVED [:ALPHA "s"]]] [:PCHAR [:UNRESERVED [:ALPHA "s"]]] "?" [:PCHAR [:UNRESERVED [:ALPHA "o"]]] [:PCHAR [:UNRESERVED [:ALPHA "n"]]] [:PCHAR [:UNRESERVED [:ALPHA "e"]]]]])))
         
(deftest phone-uri
  (let [phone-uri-parser (binding [instaparse.abnf/*case-insensitive* true]
                           (parser (slurp "test/instaparse/phone_uri.txt") :input-format :abnf))]
    (are [x y] (= x y)
         (phone-uri-parser "tel:+1-201-555-0123")
         [:TELEPHONE-URI
          "tel:"
          [:TELEPHONE-SUBSCRIBER
           [:GLOBAL-NUMBER
            [:GLOBAL-NUMBER-DIGITS
             "+"
             [:DIGIT "1"]
             [:PHONEDIGIT [:VISUAL-SEPARATOR "-"]]
             [:PHONEDIGIT [:DIGIT "2"]]
             [:PHONEDIGIT [:DIGIT "0"]]
             [:PHONEDIGIT [:DIGIT "1"]]
             [:PHONEDIGIT [:VISUAL-SEPARATOR "-"]]
             [:PHONEDIGIT [:DIGIT "5"]]
             [:PHONEDIGIT [:DIGIT "5"]]
             [:PHONEDIGIT [:DIGIT "5"]]
             [:PHONEDIGIT [:VISUAL-SEPARATOR "-"]]
             [:PHONEDIGIT [:DIGIT "0"]] 
             [:PHONEDIGIT [:DIGIT "1"]]
             [:PHONEDIGIT [:DIGIT "2"]]
             [:PHONEDIGIT [:DIGIT "3"]]]]]])))

(def abnf-german
  "Testing the ABNF regular expressions"
  (parser
    "
; a parser for the German programming language
; http://esolangs.org/wiki/German

S = <*1space> (A / B) *(<space> (A / B)) <*1space>
A = #'BEER'
B = #'SCHNITZEL'
space = #'\\s+'
" :input-format :abnf))

(deftest german
  (are [x y] (= x y)
       (abnf-german " BEER SCHNITZEL BEER BEER SCHNITZEL SCHNITZEL 
                     BEER BEER BEER ")
       [:S
        [:A "BEER"]
        [:B "SCHNITZEL"]
        [:A "BEER"]
        [:A "BEER"]
        [:B "SCHNITZEL"]
        [:B "SCHNITZEL"]
        [:A "BEER"]
        [:A "BEER"]
        [:A "BEER"]]))

(def abnf-abc
  "Trying the \"equal amount of A's, B's, and C's\" parser in ABNF,
to test the lookahead"
  (parser
    "S = &(A 'c') 1*'a' B
     A = 'a' [A] 'b'
     <B> = 'b' [B] 'c'"
    :input-format :abnf))

(deftest abc
  (are [x y] (= x y)
       (abnf-abc "aaaabbbbcccc")
       [:S "a" "a" "a" "a" "b" "b" "b" "b" "c" "c" "c" "c"]
       (abnf-abc "aaabbbc" :total true)
       [:S "a" "a" "a" "b" "b" "b" "c" [:instaparse/failure ""] [:instaparse/failure ""]]))

(def reps
  "Testing the different kinds of repetitions"
  (parser
    "S = A B C D E
     A = *'a'
     B = 2*'b'
     C = *2'c'
     D = 2'd'
     E = 2*4'e'"
    :input-format :abnf))

(deftest rep-test
  (are [x] (not (instance? instaparse.gll.Failure x))
       (reps "aabbccddee")
       (reps "bbbbbbddeeee")
       (reps "bbcddee")))

(deftest rep-test-errors
  (are [x] (instance? instaparse.gll.Failure x)
       (reps "")
       (reps "bccddee")
       (reps "aaaabbbbcccddee")))

(def regex-chars
  "Testing %d42-91. The boundary chars are \"*\" and \"[\", which normally aren't allowed in a regex."
  (parser
    "S = %d42-91"
    :input-format :abnf))

(deftest regex-char-test
  (doseq [i (range 1 (inc 100))
          :let [c (char i)]]
    (if (<= 42 i 91)
      (is (not (instance? instaparse.gll.Failure (regex-chars (str c)))))
      (is (instance? instaparse.gll.Failure (regex-chars (str c)))))))

(deftest unicode-test
  (let [poop "\uD83D\uDCA9"]  ; U+1F4A9 PILE OF POO
    (let [parser1 (parser "S = %x1F4A9"
                          :input-format :abnf)]
      (are [x y] (= x y)
           (parses parser1 poop) [[:S poop]])
      (are [x] (instance? instaparse.gll.Failure x)
           (parser1 (str poop poop))
           (parser1 (str (first poop)))
           ;; shouldn't work on the surrogate characters individually
           (parser1 (str (second poop)))))
    (let [parser2 (parser "S = %x1F4A8-1F4A9"
                          :input-format :abnf)]
      (are [x y] (= x y)
           (parses parser2 poop) [[:S poop]])
      (are [x] (instance? instaparse.gll.Failure x)
           (parser2 (str poop poop))
           (parser2 (str (first poop)))
           (parser2 (str (second poop)))))
    (let [parser3 (parser "S = %x1F4A9.1F4A9.1F4A9"
                          :input-format :abnf)]
      (are [x y] (= x y)
           (parses parser3 (str poop poop poop)) [[:S poop poop poop]])
      (are [x] (instance? instaparse.gll.Failure x)
           (parser3 (str poop))))
    ;; it would be cool if EBNF supported unicode in a parser spec
    ;; (ABNF doesn't allow that though)
    (let [parser4 (parser (str "S = '" poop "'*"))]
      (are [x y] (= x y)
           (parses parser4 (str poop poop poop)) [[:S poop poop poop]])
      (are [x] (instance? instaparse.gll.Failure x)
           (parser4 (str (first poop)))
           (parser4 (str (second poop)))
           (parser4 (str poop poop (first poop)))))))

(ns instaparse.failure-test
  (:require
    #?(:clj  [instaparse.failure :refer [marker pprint-failure]]
       :cljs [instaparse.failure :refer [marker pprint-failure]])
    #?(:clj [clojure.test :refer [deftest are is]]
       :cljs [cljs.test]))
  #?(:cljs (:require-macros
             [cljs.test :refer [is are deftest]])))

;; Tests new marker function by counting the number of tabs in both text
;; and marker lines to make sure the count is the same.
(deftest marker-test
  (let [text           "\t\ti'm a sample error line with tabs."
        n              16
        marker         (marker text n)]
    (let [text-tabs   (count (filter #{"\t"} text))
          marker-tabs (count (filter #{"\t"} marker))]
      (is (= text-tabs marker-tabs)))))

;; No assertions but should print marker line with caret under 'e' in error.
(deftest pprint-failure-test
  (let [request {:line 3
                 :column 16
                 :text "\t\ti'm a sample error line with tabs."
                 :reason "for testing"}]
    (println "Test passes if '^' appears under the 'e' in 'error':")
    (pprint-failure request)
    ;; Just testing for no exceptions here.
    (is (= 1 1))))

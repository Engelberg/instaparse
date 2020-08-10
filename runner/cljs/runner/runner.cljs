(ns instaparse.runner.runner
  (:require [cljs.nodejs :as nodejs]
            [instaparse.abnf-test]
            [instaparse.auto-flatten-seq-test]
            [instaparse.core-test]
            [instaparse.defparser-test]
            [instaparse.failure-test]
            [instaparse.grammars]
            [instaparse.repeat-test]
            [instaparse.specs]
            [cljs.test :as test :refer-macros [run-tests]]))

(nodejs/enable-util-print!)

(defmethod cljs.test/report [:cljs.test/default :end-run-tests] [m]
  (if (test/successful? m)
    (println "Tests succeeded!")
    (do
      (println "Tests failed.")
      ((aget js/process "exit") 1))))

(defn -main []
  (run-tests 'instaparse.abnf-test
             'instaparse.auto-flatten-seq-test
             'instaparse.core-test
             'instaparse.defparser-test
             'instaparse.failure-test
             'instaparse.grammars
             'instaparse.repeat-test
             'instaparse.specs))

(set! *main-cli-fn* -main)

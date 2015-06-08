(ns instaparse.runner.runner
  (:require [cljs.nodejs :as nodejs]
            [instaparse.abnf-test]
            [instaparse.auto-flatten-seq-test]
            [instaparse.core-test]
            [instaparse.grammars]
            [instaparse.repeat-test]
            [instaparse.specs]
            [cljs.test :as test :refer-macros [run-tests]]))

(nodejs/enable-util-print!)

(defn -main []
  (run-tests 'instaparse.abnf-test
             'instaparse.auto-flatten-seq-test
             'instaparse.core-test
             'instaparse.grammars
             'instaparse.repeat-test
             'instaparse.specs))

(set! *main-cli-fn* -main)

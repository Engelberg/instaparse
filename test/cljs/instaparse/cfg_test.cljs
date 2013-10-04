(ns instaparse.cfg-test
  (:require [cemerick.cljs.test :as t]
            [instaparse.cfg :refer [safe-read-string]])
  (:require-macros [cemerick.cljs.test :refer (is deftest with-test 
                                                  run-tests testing)]))

(deftest safe-read-string-test
  (is (= "foo" (safe-read-string "foo\"")))
  (is (= "foo\nbar" (safe-read-string "foo\\nbar\"")))
  (is (= "foo\"bar" (safe-read-string "foo\\\"bar\"")))

  (is (thrown-with-msg? js/Error #"escape" (safe-read-string "foo\\gbar\"")))
  (is (thrown-with-msg? js/Error #"EOF" (safe-read-string "foobar"))))

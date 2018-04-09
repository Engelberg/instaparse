(defproject instaparse "1.4.9"
  :description "Instaparse: No grammar left behind"
  :url "https://github.com/Engelberg/instaparse"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :profiles {:dev {:dependencies
                   [[org.clojure/clojurescript "1.8.40"]
                    [org.clojure/tools.trace "0.7.9"]
                    [criterium "0.4.4"]
                    [rhizome "0.2.9"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]
                                  [org.clojure/clojurescript "1.7.28"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [org.clojure/clojurescript "1.8.34"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [org.clojure/clojurescript "1.10.238"]
                                  [org.clojure/tools.reader "1.2.1"]]}}
  :aliases {"test-all" ["with-profile" "+1.5:+1.6:+1.7:+1.8:+1.9" "test"]
            "test-cljs" ["cljsbuild" "test" "unit-tests"]
            "test-cljs-all" ["with-profile" "+1.7:+1.8:+1.9" "do" "clean," "test-cljs"]}
  :test-paths ["test/" "target/generated/test/clj"]
  :source-paths ["src/" "target/generated/src/clj"]
  :cljsee {:builds [{:source-paths ["src/"]
                     :output-path "target/generated/src/clj"
                     :rules :clj}
                    {:source-paths ["test/"]
                     :output-path "target/generated/test/clj"
                     :rules :clj}]}
  :plugins [[lein-cljsbuild "1.1.7"]
            [cljsee "0.1.0"]]
  ;:hooks [leiningen.cljsbuild]
  :target-path "target"
  :scm {:name "git"
        :url "https://github.com/Engelberg/instaparse"}
  :prep-tasks [["cljsee" "once"]]
  :cljsbuild {:builds [{:id "none"
                        :source-paths ["src/"]
                        :compiler {:output-to "target/js/none.js"
                                   :optimizations :none
                                   :pretty-print true}}
                       {:id "test"
                        :source-paths ["src/"
                                       "test/"
                                       "runner/cljs"]
                        :compiler {:output-to "target/js/advanced-test.js"
                                   :optimizations :advanced
                                   :target :nodejs
                                   :pretty-print false}}]
              :test-commands {"unit-tests" ["node" "target/js/advanced-test.js"]}})

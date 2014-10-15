(defproject instaparse "1.3.5"
  :description "Instaparse: No grammar left behind"
  :url "https://github.com/Engelberg/instaparse"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2342"]
                 [com.cemerick/clojurescript.test "0.3.1"]]
  :profiles {:dev {:dependencies 
                   [[org.clojure/tools.trace "0.7.5"]
                    [criterium "0.3.1"]
                    [rhizome "0.1.8"]]
                   :plugins [[com.cemerick/austin "0.1.4"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.1"]]}
             :1.6 {:dependencies [[org.clojure/clojure "1.6.0"]]}}
  :aliases {"test-all" ["with-profile" "+1.5:+1.6" "test"]}
  :test-paths ["target/generated/src/clj" "target/generated/test/clj"]
  :source-paths ["src/cljs" "src/clj" ;"target/generated/src/clj" "target/generated/src/cljs"
                 ]
  :clj {:source-paths  ["src/clj",  "target/generated/src/clj"]
        :test-paths  ["test/clj",  "target/generated/test/clj"]}
  :cljx {:builds [{:source-paths  ["src/cljx"]
                   :output-path "target/generated/src/clj"
                   :rules :clj}
                  {:source-paths ["src/cljx"]
                   :output-path "target/generated/src/cljs"
                   :rules :cljs}
                  {:source-paths  ["test/cljx"]
                   :output-path "target/generated/test/clj"
                   :rules :clj}
                  {:source-paths  ["test/cljx"]
                   :output-path  "target/generated/test/cljs"
                   :rules :cljs}]}   
  :plugins [[lein-cljsbuild "1.0.3"]
            [com.keminglabs/cljx "0.4.0" :exclusions [org.clojure/clojure]]
            [com.cemerick/clojurescript.test "0.3.1"]]
  :hooks  [leiningen.cljsbuild cljx.hooks]
  :target-path "target"
  :scm {:name "git"
        :url "https://github.com/Engelberg/instaparse"}
  :cljsbuild {:builds [{:source-paths ["src/cljs" 
                                       ;"test/cljs" 
                                       "target/generated/test/clj"
                                       ;"target/generated/src/cljs" 
                                       "target/generated/test/cljs"]
                        :compiler {:output-to "target/test.js"
                                   :optimizations :advanced
                                   :pretty-print true}}]
              :test-commands {"unit-tests" ["phantomjs" :runner "target/test.js"]}})

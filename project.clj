(defproject instaparse "1.3.3.1"
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
  :test-paths ["test/clj"]
  :source-paths ["src/cljs" "src/clj"]
  :plugins [[lein-cljsbuild "1.0.3"]
            [com.cemerick/clojurescript.test "0.3.1"]]
  :hooks  [leiningen.cljsbuild]
  :target-path "target"
  :scm {:name "git"
        :url "https://github.com/Engelberg/instaparse"}
  :cljsbuild {:builds [{:source-paths ["src/cljs" "test/cljs"]
                        :compiler {:output-to "target/test.js"
                                   :pretty-print true}}]
              :test-commands {"unit-tests" ["phantomjs" :runner "target/test.js"]}})

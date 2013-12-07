(defproject instaparse "1.2.2"
  :description "Instaparse: No grammar left behind"
  :url "https://github.com/Engelberg/instaparse"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2030"]
                 [com.cemerick/clojurescript.test "0.0.4"]]

  :source-paths ["src/clj"] 
  :plugins [[lein-cljsbuild "1.0.0"]]
  :profiles {:dev {:dependencies 
                   [[org.clojure/tools.trace "0.7.5"]
                    [criterium "0.3.1"]
                    [rhizome "0.1.8"]]}}
  :test-paths ["test/clj"]
  :target-path "target"
  :scm {:name "git"
        :url "https://github.com/Engelberg/instaparse"}

  :cljsbuild {:builds [{:source-paths ["src/cljs" "test/cljs"]
                        :compiler {:output-to "target/test.js"
                                   :pretty-print true}}]
              :test-commands {"unit-tests" ["test/cljs/runners/phantomjs.js" "target/test.js"]}} 
)

(defproject instaparse "1.1.0-SNAPSHOT"
  :description "Instaparse: No grammar left behind"
  :url "https://github.com/Engelberg/instaparse"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies 
                   [[org.clojure/tools.trace "0.7.5"]
                    [criterium "0.3.1"]]}}
  :test-paths ["test"]
  :scm {:name "git"
        :url "https://github.com/Engelberg/instaparse"})

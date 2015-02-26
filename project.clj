(defproject mkremins/decodn "0.0-SNAPSHOT"
  :dependencies
  [[org.clojure/clojure "1.6.0"]
   [org.clojure/clojurescript "0.0-2913"]]

  :plugins
  [[lein-cljsbuild "1.0.4"]]

  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test"]
             :compiler {:main decodn.test-runner
                        :output-dir "target"
                        :output-to "target/test.js"
                        :optimizations :none
                        :target :nodejs}}]
   :test-commands {"test" ["node" "target/test.js"]}})

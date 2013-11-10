(defproject async-space-invaders "0.1.0-SNAPSHOT"
  :description "Space invaders, using core.async!"
  :url "http://codeofrob.com"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2014"]
                 [org.clojure/core.incubator "0.1.3"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]]

  :plugins [[lein-cljsbuild "1.0.0-alpha2"]]

  :source-paths ["src"]

  :cljsbuild { 
    :builds [{:id "asi"
              :source-paths ["src"]
              :compiler {
                :output-to "asi.js"
                :output-dir "out"
                :optimizations :none
                :source-map true}}]})

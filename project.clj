(defproject diff-test "0.1.1-SNAPSHOT"
  :description "DIFF SELF TEST"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [juji/editscript "0.6.6"]]

  :source-paths ["."]
  :resource-paths ["."]
  :main main
  :repl-options {:init-ns user})

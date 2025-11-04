(ns build-diff-files
  (:require [clojure.java.io :as io]))

(defn write-to-diff-file [edits deleted replace added]
  (with-open [writer-replace (io/writer replace :append true)
              writer-added (io/writer added :append true)
              writer-deleted (io/writer deleted :append true)]
    (doseq [edit edits]
      (let [[line-key true-line op original-content new-content] edit]
        (case op
          :+ (.write writer-added (str (first true-line) " - " (first new-content) "\n"))
          :- (.write writer-deleted (str (first true-line) " - " (first original-content) "\n"))
          :r (.write writer-replace (str (first true-line) " - " (first original-content) " ** " (first new-content) "\n")))))))

(defn merge-diffs [deleted replace added]
  (with-open [final-diff (io/writer (str "DIFF" (hash (.toEpochMilli (java.time.Instant/now))) ".txt") :append true)]
    (.write final-diff "---- DELETED ----\n")
    (doseq [line deleted]
      (.write final-diff (str line "\n")))
    (.write final-diff "\n---- REPLACED ----\n")
    (doseq [line replace]
      (.write final-diff (str line "\n")))
    (.write final-diff "\n---- ADDED ----\n")
    (doseq [line added]
      (.write final-diff (str line "\n")))))
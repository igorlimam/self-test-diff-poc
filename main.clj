(ns main
  (:require [build-block-map :refer [build-index-block-map]]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [build-edits :refer [search-diff make-del-diff make-add-diff]]
            [build-diff-files :refer [write-to-diff-file merge-diffs]]))

(defn str->reader [s]
  (java.io.BufferedReader. (java.io.StringReader. s)))

(defn stream->temp-file [stream hash]
  (let [temp-file (java.io.File/createTempFile (str "DIFF|" hash) ".tmp")]
    (with-open [reader (java.io.BufferedReader. (io/reader stream))
                writer (clojure.java.io/writer temp-file)]
      (doseq [line (line-seq reader)]
        (.write writer (str line "\n"))))
    temp-file))

(defn key-rank [k]
  (let [s (if (or (keyword? k) (symbol? k)) (name k) (str k))]
    (cond
      (= s "0") [0 0]
      (re-matches #"(?i)[A-Za-z].*" s) [1 (clojure.string/upper-case s)]
      (re-matches #"\d+" s) [2 (Integer/parseInt s)]
      :else [3 s])))

(defn -main
  [& args]
  (let [stream-model (io/input-stream "./modelo.txt")
        stream-gerado (io/input-stream "./gerado.txt")
        modelo-tmp-file (stream->temp-file stream-model (hash "modeloHash"))
        gerado-tmp-file (stream->temp-file stream-gerado (hash "geradoHash"))
        ;modelo-tmp-file (stream->temp-file (str->reader "0001|GOUM\n0000| OYOYOLDRUSTY\n0001| OYOYOLD\nA000| OYOYOLDRUSTY") (hash "modeloHash"))
        ;gerado-tmp-file (stream->temp-file (str->reader "0001|GOUM\n0001| OYOYOLDRUSTY\nA001| GOMA") (hash "geradoHash"))
        tmp-diff-replace-file (java.io.File/createTempFile "DIFF-REPLACE" "txt")
        tmp-diff-added-file (java.io.File/createTempFile "DIFF-ADDED" "txt")
        tmp-diff-deleted-file (java.io.File/createTempFile "DIFF-DELETED" "txt")]

    (try
      (let [modelo (io/reader modelo-tmp-file)
            gerado (io/reader gerado-tmp-file)
            modelo-blocks (build-index-block-map modelo)
            gerado-blocks (build-index-block-map gerado)
            common  (set/intersection (set (keys modelo-blocks)) (set (keys gerado-blocks)))
            deleted (set/difference (set (keys modelo-blocks)) (set (keys gerado-blocks)))
            added   (set/difference (set (keys gerado-blocks)) (set (keys modelo-blocks)))]

        (loop [block (sort-by key-rank (set/union (set (keys modelo-blocks)) (set (keys gerado-blocks))))
               anchor 0]
          (let [current-block (first block)
                model-stream (io/reader modelo-tmp-file)
                gerado-stream (io/reader gerado-tmp-file)]
            (cond
              (contains? common current-block) (let [[ed new-anchor] (search-diff model-stream (get modelo-blocks current-block) gerado-stream (get gerado-blocks current-block) anchor)]
                                                 (write-to-diff-file ed tmp-diff-deleted-file tmp-diff-replace-file tmp-diff-added-file)
                                                 (recur (rest block) new-anchor))
              (contains? deleted (first block)) (let [[ed new-anchor] (make-del-diff (io/reader modelo-tmp-file) (get modelo-blocks (first block)) tmp-diff-deleted-file anchor)]
                                                  (write-to-diff-file ed tmp-diff-deleted-file tmp-diff-replace-file tmp-diff-added-file)
                                                  (recur (rest block) new-anchor))

              (contains? added (first block)) (let [[ed new-anchor] (make-add-diff (io/reader gerado-tmp-file) (get gerado-blocks (first block)) tmp-diff-added-file anchor)]
                                                 (write-to-diff-file ed tmp-diff-deleted-file tmp-diff-replace-file tmp-diff-added-file)
                                                 (recur (rest block) new-anchor))
              :else (println (str "END OF DIFF")))))
        (merge-diffs (line-seq (io/reader tmp-diff-deleted-file)) (line-seq (io/reader tmp-diff-replace-file)) (line-seq (io/reader tmp-diff-added-file))))
      (finally
        (.delete modelo-tmp-file)
        (.delete gerado-tmp-file)
        (.delete tmp-diff-replace-file)
        (.delete tmp-diff-added-file)
        (.delete tmp-diff-deleted-file)))))
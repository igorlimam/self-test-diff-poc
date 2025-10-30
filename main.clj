(ns main
  (:require [build-block-map :refer [build-index-block-map]]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [editscript.core :as editscript]))

(defn str->reader [s]
  (java.io.BufferedReader. (java.io.StringReader. s)))

(defn stream->temp-file [stream hash]
  (let [temp-file (java.io.File/createTempFile (str "DIFF|" hash) ".tmp")]
    (with-open [writer (clojure.java.io/writer temp-file)]
      (doseq [line (line-seq stream)]
        (.write writer (str line "\n"))))
    temp-file))



(defn make-diff [modelo-vec gerado-vec]
  (println (str "Making diff between modelo lines: " modelo-vec " and gerado lines: " gerado-vec))
  (let [diff (editscript/diff modelo-vec gerado-vec {:algo :quick :str-diff :line :str-change-limit 0.9999999 :vec-timeout 7200000})
        edits (editscript/get-edits diff)]
    (println (str "Diff edits: " edits))
    edits))

(defn get-block-vec [file-line-seq block-info]
  (vec (take (inc (- (:f block-info) (:i block-info)))
             (drop (:i block-info) file-line-seq))))

(defn search-diff
  ([modelo-stream modelo-block gerado-stream gerado-block] (search-diff modelo-stream modelo-block gerado-stream gerado-block 0))
  ([modelo-stream modelo-block gerado-stream gerado-block anchor]
   (let [modelo (line-seq modelo-stream)
         gerado (line-seq gerado-stream)]
     (println (str "Comparing block " modelo-block " with " gerado-block))
     (if (not= (:h modelo-block) (:h gerado-block))
       (let [edits (make-diff (get-block-vec modelo modelo-block) (get-block-vec gerado gerado-block))
             anchor (edit-lines edits modelo-block gerado-block anchor)]))

      ;add at line x of model
      ;remove from model -> model
      ;replace line 1 -> model
      [])))

(defn key-rank [k]
  (let [s (if (or (keyword? k) (symbol? k)) (name k) (str k))]
    (cond
      (= s "0") [0 0]
      (re-matches #"(?i)[A-Za-z].*" s) [1 (clojure.string/upper-case s)]
      (re-matches #"\d+" s) [2 (Integer/parseInt s)]
      :else [3 s])))


(defn -main
  [& args]
  (println (build-index-block-map (str->reader "0001|GOUM\n0000| OYOYOLDRUSTY")))
  (let [modelo-tmp-file (stream->temp-file (str->reader "0001|GOUM\n0000| OYOYOLDRUSTY") (hash "modeloHash"))
        gerado-tmp-file (stream->temp-file (str->reader "0001|GOUM\n0001| OYOYOLDRUSTY") (hash "geradoHash"))
        modelo (io/reader modelo-tmp-file)
        gerado (io/reader gerado-tmp-file)
        m (build-index-block-map modelo)
        g (build-index-block-map gerado)]
    (println "Modelo blocks:" (keys m))
    (println "Gerado blocks:" (keys g))
    (let [common  (set/intersection (set (keys m)) (set (keys g)))
          deleted (set/difference (set (keys m)) (set (keys g)))
          added   (set/difference (set (keys g)) (set (keys m)))]
      (doseq [block (sort-by key-rank (set/union (set (keys m)) (set (keys g))))]
        (cond
          (contains? common block) (search-diff (io/reader modelo-tmp-file) (get m block) (io/reader gerado-tmp-file) (get g block))
          (contains? deleted block) (println (str "HERE WE HAVE DELETED BLOCK: " block))
          (contains? added block) (println (str "HERE WE HAVE ADDED BLOCK: " block))
          :else (println (str "END OF DIFF")))))))






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
  (let [diff (editscript/diff modelo-vec gerado-vec {:algo :quick :str-diff :line :str-change-limit 0.9999999 :vec-timeout 7200000})
        edits (editscript/get-edits diff)]
    edits))

(defn get-block-vec [file-line-seq block-info]
  (vec (take (inc (- (:f block-info) (:i block-info)))
             (drop (:i block-info) file-line-seq))))

(defn remove-vec [vec]
  (loop [vec vec]
    (if (seq? vec)
      (recur (first vec))
      vec)))

(defn edit-lines [edits model-initial-line anchor]
  ;[[[1] :r [content]]]
  ;[[0] :r ["|E001|1|"]]
  ;[[0] :-]
  ;[[0] :+ "|D001|1|"]
  (loop [edits edits
         true-line-edits []
         anchor anchor
         initial-anchor anchor]
    (if (not (empty? edits))
      (let [[line op & content] (first edits)
            line (first line)
            [true-line new-anchor] (case op
              :+ [(+ (+ model-initial-line line) initial-anchor) (inc anchor)]
              :- [(+ (+ model-initial-line line) initial-anchor) (dec anchor)]
              :r [(+ (+ model-initial-line line) initial-anchor) anchor])]
        (recur (rest edits) (conj true-line-edits [[true-line] op (if (nil? content) content [(remove-vec content)])]) new-anchor initial-anchor))
      [true-line-edits anchor])))

(defn search-diff [modelo-stream modelo-block gerado-stream gerado-block anchor]
  (let [modelo (line-seq modelo-stream)
        gerado (line-seq gerado-stream)]
    (println (str "Comparing block " modelo-block " with " gerado-block))
    (if (not= (:h modelo-block) (:h gerado-block))
      (let [modelo-content-block (get-block-vec modelo modelo-block)
            gerado-content-block (get-block-vec gerado gerado-block)
            edits (make-diff modelo-content-block gerado-content-block)]
        (edit-lines edits (:i modelo-block) anchor))
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
  (let [modelo-tmp-file (stream->temp-file (str->reader "0001|GOUM\n0000| OYOYOLDRUSTY\nA000| OYOYOLDRUSTY") (hash "modeloHash"))
        gerado-tmp-file (stream->temp-file (str->reader "0001|GOUM\n00002|NOOOOOOO\n0001| OYOYOLDRUSTY\nA001| OYOYOLDRUSTY") (hash "geradoHash"))
        modelo (io/reader modelo-tmp-file)
        gerado (io/reader gerado-tmp-file)
        m (build-index-block-map modelo)
        g (build-index-block-map gerado)]

    (let [common  (set/intersection (set (keys m)) (set (keys g)))
          deleted (set/difference (set (keys m)) (set (keys g)))
          added   (set/difference (set (keys g)) (set (keys m)))]

      (loop [block (sort-by key-rank (set/union (set (keys m)) (set (keys g))))
             anchor 0]
        (let [current-block (first block)
              model-stream (io/reader modelo-tmp-file)
              gerado-stream (io/reader gerado-tmp-file)]
          (cond
            (contains? common current-block) (let [[ed new-anchor] (search-diff model-stream (get m current-block) gerado-stream (get g current-block) anchor)]
                                               ;write to file here
                                               (println ed)
                                               (recur (rest block) new-anchor))
            (contains? deleted (first block)) (println (str "HERE WE HAVE DELETED BLOCK: " (first block)))
            (contains? added (first block)) (println (str "HERE WE HAVE ADDED BLOCK: " (first block)))
            :else (println (str "END OF DIFF"))))))))






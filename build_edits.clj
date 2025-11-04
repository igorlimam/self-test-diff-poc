(ns build-edits
  (:require [editscript.core :as editscript]))

(defn remove-vec [vec]
  (loop [vector vec]
    (if (coll? vector)
      (recur (first vector))
      vector)))

(defn edit-lines [edits model-initial-line modelo-vec]
  (loop [processed-edits edits
         true-line-edits []
         block-anchor 0]
    (if (not (empty? processed-edits))
      (if (nil? processed-edits)
        (recur (rest processed-edits) true-line-edits block-anchor)
        (let [[line op & content] (first processed-edits)
              line (first line)
              [true-line file-line new-block-anchor] (case op
                                                       :+ [(+ model-initial-line line) line (dec block-anchor)]
                                                       :- [(+ model-initial-line line) line (inc block-anchor)]
                                                       :r [(+ model-initial-line line block-anchor) (+ line block-anchor) block-anchor])]
          (println (str "OPERATION: " op " model initial line: " model-initial-line " line: " line " block-anchor: " block-anchor))
          (println (str "FILE LINE " file-line " OF " (count modelo-vec) " ACCOUNTING FOR TRUE LINE " true-line))
          (recur (rest processed-edits)
                 (conj true-line-edits [[line] [true-line] op [(nth modelo-vec (min (dec (count modelo-vec)) file-line))] (if (nil? content) content [(remove-vec content)])])
                 new-block-anchor)))

      true-line-edits)))

(defn make-diff [modelo-vec gerado-vec]
  (let [diff (editscript/diff modelo-vec gerado-vec {:algo :quick :str-diff :line :str-change-limit 0.2 :vec-timeout 7200000})
        edits (editscript/get-edits diff)]
    edits))

(defn get-block-vec [file-line-seq block-info]
  (vec (take (inc (- (:f block-info) (:i block-info)))
             (drop (:i block-info) file-line-seq))))

(defn search-diff [modelo-stream modelo-block gerado-stream gerado-block]
  (let [modelo (line-seq modelo-stream)
        gerado (line-seq gerado-stream)]
    (println (str "Comparing block " modelo-block " with " gerado-block))
    (if (not= (:h modelo-block) (:h gerado-block))
      (let [modelo-content-block (get-block-vec modelo modelo-block)
            gerado-content-block (get-block-vec gerado gerado-block)
            edits (make-diff modelo-content-block gerado-content-block)]
        (edit-lines edits (:i modelo-block) modelo-content-block))
      [])))

(defn make-del-diff [modelo-stream modelo-block]
  (println (str "Making delete diff for block " modelo-block))
  (let [modelo (line-seq modelo-stream)
        modelo-content-block (get-block-vec modelo modelo-block)
        edits (map (fn [idx]
                     [[idx] :- [(nth modelo-content-block idx)]])
                   (range 0 (count modelo-content-block)))]
    (edit-lines (vec edits) (:i modelo-block) modelo-content-block)))

(defn make-add-diff [gerado-stream gerado-block]
  (let [gerado (line-seq gerado-stream)
        gerado-content-block (get-block-vec gerado gerado-block)
        edits (map (fn [idx]
                     [[idx] :+ [(nth gerado-content-block idx)]])
                   (range 0 (count gerado-content-block)))]
    (edit-lines (vec edits) (:i gerado-block) gerado-content-block)))
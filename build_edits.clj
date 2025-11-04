(ns build-edits
  (:require [editscript.core :as editscript]))

(defn remove-vec [vec]
  (loop [vector vec]
    (if (coll? vector)
      (recur (first vector))
      vector)))

(defn edit-lines [edits model-initial-line anchor modelo-vec]
  (loop [processed-edits edits
         true-line-edits []
         current-anchor anchor
         block-anchor 0]
    #_(println (str "Processing edit: " (first processed-edits) " with data: " (first processed-edits)))
    (if (not (empty? processed-edits))
      (if (nil? processed-edits)
        (recur (rest processed-edits) true-line-edits current-anchor block-anchor)
        (let [[line op & content] (first processed-edits)
              line (first line)
              [true-line new-block-anchor new-anchor] (case op
                                                        :+ [(+ (+ model-initial-line line) block-anchor) (dec block-anchor) (inc current-anchor)]
                                                        :- [(+ (+ model-initial-line line) block-anchor) (inc block-anchor) (dec current-anchor)]
                                                        :r [(+ (+ model-initial-line line) block-anchor) block-anchor current-anchor])
              file-line (max 0 (min (dec (count modelo-vec)) (+ line block-anchor)))]
          #_(println (str "true line: " true-line " with file line: " file-line " and op: " op " and anchor " new-anchor " and block anchor " block-anchor " model vec size " (count modelo-vec)))
          #_(println (str "modelo vec: " modelo-vec))
          (recur (rest processed-edits) (conj true-line-edits [[line] [true-line] op [(nth modelo-vec file-line)] (if (nil? content) content [(remove-vec content)])]) new-anchor new-block-anchor)))

      [true-line-edits current-anchor])))

(defn make-diff [modelo-vec gerado-vec]
  (let [diff (editscript/diff modelo-vec gerado-vec {:algo :quick :str-diff :line :str-change-limit 0.2 :vec-timeout 7200000})
        edits (editscript/get-edits diff)]
    edits))

(defn get-block-vec [file-line-seq block-info]
  (vec (take (inc (- (:f block-info) (:i block-info)))
             (drop (:i block-info) file-line-seq))))

(defn search-diff [modelo-stream modelo-block gerado-stream gerado-block anchor]
  (let [modelo (line-seq modelo-stream)
        gerado (line-seq gerado-stream)]
    (println (str "Comparing block " modelo-block " with " gerado-block))
    (if (not= (:h modelo-block) (:h gerado-block))
      (let [modelo-content-block (get-block-vec modelo modelo-block)
            gerado-content-block (get-block-vec gerado gerado-block)
            edits (make-diff modelo-content-block gerado-content-block)]
        (edit-lines edits (:i modelo-block) anchor modelo-content-block))
      [[] anchor])))

(defn make-del-diff [modelo-stream modelo-block diff-file anchor]
  (println (str "Making delete diff for block " modelo-block))
  (println (str "Anchor at: " anchor))
  (let [modelo (line-seq modelo-stream)
        modelo-content-block (get-block-vec modelo modelo-block)
        edits (map (fn [idx]
                     [[(- idx anchor)] :- [(nth modelo-content-block idx)]])
                   (range 0 (count modelo-content-block)))]
    (edit-lines (vec edits) (:i modelo-block) anchor modelo-content-block)))

(defn make-add-diff [gerado-stream gerado-block diff-file anchor]
  (let [gerado (line-seq gerado-stream)
        gerado-content-block (get-block-vec gerado gerado-block)
        edits (map (fn [idx]
                     [[(+ idx anchor)] :+ [(nth gerado-content-block idx)]])
                   (range 0 (count gerado-content-block)))]
    (edit-lines (vec edits) (:i gerado-block) anchor gerado-content-block)))
(ns build-block-map)

(def possible-blocks ["0" "A" "B" "C100"])

(defn get-block-id [line]
  (str (first (clojure.string/replace (clojure.string/trim line) #"\|" ""))))

(defn build-index-block-map [file-stream]
  (loop [lines (line-seq file-stream)
         block-map {}
         current-block-id nil
         current-block-initial-line 0
         current-line 0
         accumulated-hash 0]
    (if (not (empty? lines))
      (if (nil? current-block-id)
        (recur lines block-map (get-block-id (first lines)) 0 0 0)
        (let [curr-id (get-block-id (first lines))]
          (cond
            (= curr-id current-block-id)
            (recur (rest lines) block-map current-block-id current-block-initial-line (inc current-line) (hash (str accumulated-hash (first lines))))
            :else
            (recur (rest lines)
                   (assoc block-map (keyword current-block-id)
                                    {:i current-block-initial-line
                                     :f (dec current-line)
                                     :h accumulated-hash})
                   curr-id
                   current-line
                   (inc current-line)
                   (hash (first lines))))))
      (if (nil? current-block-id)
        block-map
        (assoc block-map (keyword current-block-id)
                         {:i current-block-initial-line
                          :f (dec current-line)
                          :h accumulated-hash})))))
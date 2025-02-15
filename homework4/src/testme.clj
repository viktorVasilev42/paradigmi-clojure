(ns testme)

(defn shift-row [row]
  (conj (vec (butlast row)) (last row)))

(defn check-diff [rows]
  (every? #(apply distinct? %) (apply map vector rows)))

(defn solve [rows acc-rows n]
  (cond
    ;; All rows placed, check validity
    (empty? rows) (if (check-diff acc-rows) acc-rows nil)

    ;; Try placing the row without shifting
    (let [result (solve (rest rows) (conj acc-rows (first rows)) 0)]
      (if result
        result
        ;; Try shifting the current row
        (if (>= n (count (first rows)))
          nil
          (solve (cons (shift-row (first rows)) (rest rows)) acc-rows (inc n)))))))

(defn solve-final [rows]
  (solve rows [] 0))

(defn -main []
  (let [result (solve-final '((3 4 1 0 2)
                              (3 1 2 4 0)
                              (4 0 3 2 1)
                              (3 1 2 4 0)
                              (3 1 0 2 4)))]
    (println result)))


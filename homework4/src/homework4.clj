(ns homework4)

(defn shift-row [row]
  (let [
    rev-row (reverse row)
  ]
    (cons (first rev-row) (reverse (rest rev-row)))  
  )
)

(defn check-diff [rows]
  (cond
    (empty? (first rows)) true
    :else
      (and
        (let [
          firsts (map #(first %) rows)
        ]
          (= (count firsts) (count (distinct firsts)))
        )
        (check-diff (map #(rest %) rows))
      )
  )  
)

(defn solve [rows, acc-rows, n]
  (cond
    (empty? rows) acc-rows
    (>= n (count (first rows))) nil
    (check-diff (cons (first rows) acc-rows))
      (or
        (solve (rest rows) (cons (first rows) acc-rows) 0)
        (solve (cons (shift-row (first rows)) (rest rows)) acc-rows (inc n))
      )
    :else
      (solve (cons (shift-row (first rows)) (rest rows)) acc-rows (inc n))
  )
)

(defn solve-final [rows]
  (solve rows '() 0)
)

(defn -main []
  (let [
    result (solve-final '(
      (2 0 3 1) 
      (0 2 1 3)
      (0 2 3 1)
    ))
  ]
    (println result)  
  )
)

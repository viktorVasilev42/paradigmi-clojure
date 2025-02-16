(ns boja)

(defn shift-row 
  "rotiraj redica ednash"
  [row]
  (let [
    rev-row (reverse row)
  ]
    (cons (first rev-row) (reverse (rest rev-row)))  
  )
)

(defn check-diff 
  "proveri dali se validni kolonite vo puzzle"
  [rows]
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

(defn solve-inner 
  "trial and error so backtracking za
  celiot puzzle. so n broime dali sme gi
  probale site rotiranja za dadena redica
  so acc-rows gi akumulirame site prethodni
  validni redici"
  [rows, acc-rows, n]
  (cond
    (empty? rows) acc-rows
    (>= n (count (first rows))) nil
    (check-diff (cons (first rows) acc-rows))
      (or
        (solve-inner (rest rows) (cons (first rows) acc-rows) 0)
        (solve-inner (cons (shift-row (first rows)) (rest rows)) acc-rows (inc n))
      )
    :else
      (solve-inner (cons (shift-row (first rows)) (rest rows)) acc-rows (inc n))
  )
)

(defn solve 
  "pocni backtracking so prazen acc-rows i 
  0 rotiranja za prvata redica"
  [rows]
  (solve-inner rows '() 0)
)

(defn show [result]
  (cond
    (nil? result)
      (println "NO SOLUTION")
    :else  
      (doseq 
        [x (range (count result))] (println (nth result x))
      )
  )
)

(defn -main []
  (let [
    result (solve '(
      (1 1 1 1)
      (1 1 1 1)
      (1 1 1 1)
    ))
  ]
    (show result)
  )
)

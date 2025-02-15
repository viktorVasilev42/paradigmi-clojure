(ns homework2)

(defn get-row [x, puzzle]
  (nth puzzle x)
)

(defn get-col [y, puzzle]
  (map (fn[x] (nth x y)) puzzle)
)

(defn get-el-at [coor, puzzle]
  (let [x (first coor)
        y (second coor)
        result (nth (nth puzzle x) y)
  ]
    (cond
      (coll? result) (first result)
      :else result
    )
  )
)

(defn get-set-at-coor-as-list [coor, puzzle]
  (let [
    x (first coor)
    y (second coor)
  ]
    (seq (nth (nth puzzle x) y))
  )
)

(defn get-set-at-coor [coor, puzzle]
  (let [
    x (first coor)
    y (second coor)
  ]
    (nth (nth puzzle x) y)
  )  
)

(defn get-square [coor, puzzle]
  (let [
    x (first coor)
    y (second coor)
    square-x (* 3 (int (/ x 3)))
    square-y (* 3 (int (/ y 3)))
  ]
    (for [x (range square-x (+ square-x 3))
          y (range square-y (+ square-y 3))
         ]
      (get-set-at-coor-as-list(list x y) puzzle)
    )
  )
)

(defn transform-vector [vect]
  (map
    (fn [x]
      (cond
        ; (= x 0) #{1 2 3 4 5 6 7 8 9}
        (= x 0) #{1 2 3 4 5 6 7 8 9}
        :else #{x}
      )
    )
    vect
  )
)

(defn transform [puzzle]
  (map transform-vector puzzle)
)

(defn find-unit-in-list [lista, visited, row]
  (letfn [
    (inner-find-unit [ll, n]
      (cond
        (empty? ll) nil
        (and (= (count (first ll)) 1) (not (contains? visited (list row n)))) n
        :else (inner-find-unit (rest ll) (inc n))
      )
    )
  ]
    (inner-find-unit lista 0)
  )
)

(defn find-first-unit [puzzle, visited]
  (first
    (filter
      (fn[noo]
        (not= nil (second noo))
      )
      (map
        (fn[no] (list (first no) (find-unit-in-list (second no) visited (first no))))
        (map (fn[n] (list n (get-row n puzzle))) (range 0 9))
      )
    )
  )
)

(defn remove-el-from-sets [el, lista]
  (map
    (fn[curr-set]
      (cond
        (= (count curr-set) 1) curr-set
        :else (disj curr-set el)
      )
    )
    lista
  )
)

(defn remove-el-from-single-set [el, ss]
  (cond
    (= 1 (count ss)) ss
    :else (disj ss el)
  )
)

(defn clear-row [coor, puzzle]
  (cond
    (nil? coor) puzzle
    :else
      (let [
          x (first coor)
          currel (get-el-at coor puzzle)
      ]
        (map
          (fn[no]
            (cond
              (= (first no) x)
                (remove-el-from-sets currel (second no))
    
              :else
                (second no)
            )
          )
          (map
            (fn[n] (list n (get-row n puzzle)))
            (range 0 9)
          )
        )
      )
  )
)

(defn clear-col [coor, puzzle]
  (cond
    (nil? coor) puzzle 
    :else
      (let [
        y (second coor)
        currel (get-el-at coor puzzle)
      ]
        (map
          (fn[numrow]
            (map
              (fn [numset]
                (cond
                  (= (first numset) y)
                    (remove-el-from-single-set currel (second numset))
    
                  :else
                    (second numset)
                )
              )
              numrow
            )
          )
          (map
            (fn[row]
              (map (fn[n] (list n (nth row n))) (range 0 9))
            )
            puzzle
          )
        )
      )
  )
)

(defn clear-square [coor, puzzle]
  (cond 
    (nil? coor)
      puzzle
    :else
      (let [
        x (first coor)
        y (second coor)
        currel (get-el-at coor puzzle)
        square-x-low (* 3 (int (/ x 3)))
        square-x-high (+ 3 square-x-low)
        square-y-low (* 3 (int (/ y 3)))
        square-y-high (+ 3 square-y-low)
       ]
        (map
          (fn [row]
            (cond
              (and (>= row square-x-low) (< row square-x-high))
                (for [col (range 9)] 
                  (cond
                    (and (>= col square-y-low) (< col square-y-high))
                      (let [
                        set-at-coor (get-set-at-coor (list row col) puzzle)
                      ]
                        (if (contains? set-at-coor currel)
                          (remove-el-from-single-set currel set-at-coor)
                          set-at-coor
                        )
                      )
                    :else
                      (get-set-at-coor (list row col) puzzle)
                  )
                )  
              :else
                (get-row row puzzle)
            )
          )
          (range 0 9)  
        )
      )
  )
)

(defn first-step [puzzle, visited]
  (let [
    first-unit (find-first-unit puzzle visited)
  ]
    (cond
      (nil? first-unit) 
        puzzle  

      :else
        (let [
          new-visited (conj visited first-unit)
          new-puzzle 
            (clear-square first-unit 
              (clear-col first-unit 
                (clear-row first-unit 
                  puzzle
                )
              )
            )
        ]
          (recur new-puzzle new-visited)
        )
    )
  )
)

(defn num-of-other-occurs-in-row-col-square [coor, currel, puzzle]
  (let [
        x (first coor)
        y (second coor)
        row-x (get-row x puzzle)
        col-y (get-col y puzzle)
  ]
    (+
      ; + operand one
      (count
        (filter
          (fn [set-in-col]
            (contains? set-in-col currel)
          )
          (map
            (fn [n]
              (nth col-y n)
            )
            (filter (fn[i] (not= i x)) (range 0 9))
          )
        )
      )
      ; + operand two
      (count
        (filter
          (fn[set-in-row]
            (contains? set-in-row currel)
          )
          (map
            (fn [n]
              (nth row-x n)
            )
            (filter (fn[i] (not= i y)) (range 0 9))
          )
        )
      )
      ; + operand three
      (dec (count
        (filter
          (fn [x] (= currel x))
          (flatten (get-square coor puzzle))
        )
      ))
    )
  )
)

(defn update-list-at-pos [lista, x, newel]
  (letfn [
      (update-list-inner [li, xi, newi, ind]
        (cond
          (empty? li) '()
          (= ind xi) (cons newi (update-list-inner (rest li) xi newi (inc ind)))
          :else (cons (first li) (update-list-inner (rest li) xi newi (inc ind)))
        )
      )
  ]
    (update-list-inner lista x newel 0)  
  )  
)

(defn update-coor [coor, newel, puzzle]
  (let [
      x (first coor)
      y (second coor)
  ]
    (map
      (fn [n]
        (cond
          (= n x)
            (update-list-at-pos (get-row n puzzle) y newel)

          :else
            (get-row n puzzle)
        )
      )
      (range 0 9)  
    )
  )
)

(defn second-step-for-coor [coor, listset-at-coor, puzzle]
  (cond
    (empty? listset-at-coor)
      nil

    (= 0 (num-of-other-occurs-in-row-col-square
      coor
      (first listset-at-coor)
      puzzle
    ))
      (list coor #{(first listset-at-coor)})

    :else
      (recur coor (rest listset-at-coor) puzzle)
  )
)

(defn second-step [puzzle]
  (let [
    opt-res (some
      (fn [coor]
        (let [
            listset (get-set-at-coor-as-list coor puzzle)
            sec-step-rez (second-step-for-coor coor listset puzzle)
        ]
          (when (some? sec-step-rez) sec-step-rez)
        )
      )
      (for [x (range 0 9) y (range 0 9) 
            :when (not= 1 (count (get-set-at-coor (list x y) puzzle)))
           ] [x y])
    )
  ]
    (cond
      (some? opt-res)
        (second-step 
          (update-coor (first opt-res) (second opt-res) puzzle)
        )

      :else
        puzzle
    )
  )
)

(defn is-coor-valid [coor, puzzle]
  (let [
    listset (get-set-at-coor-as-list coor puzzle)
  ]
    (cond
      (= 1 (count listset))
        (= 0 (num-of-other-occurs-in-row-col-square coor (first listset) puzzle))  
      :else
        true
    )
  )
)

(defn is-puzzle-valid [puzzle]
  (every?
    (fn[coor] (is-coor-valid coor puzzle))
    (for [x (range 9) y (range 9)] [x y])  
  )
)

(defn is-coor-solved [coor, puzzle]
  (let [
    listset (get-set-at-coor-as-list coor puzzle)
  ]
    (cond
      (> 1 (count listset)) false
      :else (= 0 (num-of-other-occurs-in-row-col-square coor (first listset) puzzle))  
    )
  )
)

(defn is-puzzle-solved [puzzle]
  (every?
    (fn[coor] (is-coor-solved coor puzzle))
    (for [x (range 9) y (range 9)] [x y])  
  )  
)

(defn gameloop [result]
  (let [
    new-result (first-step result #{})
    new-result (second-step new-result)
   ]
    (cond
      (= new-result result)
        new-result
      :else 
        (gameloop new-result)
    )
  )
)

(defn filter-by-trying-at-coor [coor, puzzle]
  (letfn [
    (is-value-valid [currel]
      (let [
        try-puzz (update-coor coor #{currel} puzzle)
        try-puzz (gameloop try-puzz)
      ]
        (is-puzzle-valid try-puzz)
      )  
    )
  ]
    (let [
      listset (get-set-at-coor-as-list coor puzzle)
    ]
      (update-coor
        coor
        (set
          (filter
            (fn[x] (is-value-valid x))
            listset
          )
        )
        puzzle
      )
    )
  )
)

(defn filter-by-trying [puzzle]
  (letfn [
    (inner-filter [in_puzz, x, y]
      (let [
          new-puzzle (filter-by-trying-at-coor (list x y) in_puzz)
      ]
        (cond
          (= x 8)
            (cond
              (= y 8) new-puzzle  
              :else (inner-filter new-puzzle x (inc y))
            )
          :else
            (cond
              (= y 8) (inner-filter new-puzzle (inc x) 0)  
              :else (inner-filter new-puzzle x (inc y))
            )
        )
      )
    )
  ]
    (inner-filter puzzle 0 0)
  )
)

(defn find-min-non-solved [puzzle]
  (let [
      filt (filter
        (fn[vect] (> (first vect) 1))
        (map
          (fn[coor] 
            (let [ll (get-set-at-coor coor puzzle)]
              (list (count ll) (first coor) (second coor) ll)
            )
          )
          (for [x (range 9) y (range 9)] [x, y])
        )
      )
  ]
    (cond
      (empty? filt) nil  
      :else (rest(apply min-key first filt))
    )
  )

)

(defn print-sudoku [puzzle]
  (doseq [x (range 9)] (println (nth puzzle x)))
  ; (doseq [x (range 9)] 
    ; (doseq [y (range 9)]
      ; (let [
        ; curr-el (get-el-at (list x y) puzzle)
        ; to-print (cond
          ; (= 0 curr-el) "_"
          ; :else curr-el
        ; )
      ; ]
        ; (cond
          ; (contains? #{2 5} y)
            ; (print to-print "| ")  
          ; (= 8 y)
            ; (println to-print)
          ; :else
            ; (do (print to-print) (print " "))
        ; )
      ; )
    ; )
    ; (cond
      ; (contains? #{2 5} x)
        ; (println "------+-------+------")
    ; )
  ; )
)

(defn my-backtracking [puzzle]
  (let [
    unsolved (find-min-non-solved puzzle)
    unsolved-coor (list (first unsolved) (second unsolved))
    unsolved-set (last unsolved)
  ]
    (cond
      (is-puzzle-solved puzzle)
        puzzle 
      (nil? unsolved)
        nil
      :else
        (some #(my-backtracking (gameloop (update-coor unsolved-coor #{%} puzzle))) unsolved-set)
    )
  )
)

(defn solve [puzzle]
  (let [
    final (gameloop puzzle)
    final (filter-by-trying final)
    final (gameloop final)
    final (my-backtracking final)
  ]
    (cond 
      (nil? final) nil
      :else
        (vec
          (map
            (fn[row] 
              ;(vec
                (map
                  (fn[x] (first x))
                  row  
                )
              ;)
            )
            final  
          )  
        )
    )
  )
)


(defn -main []
  (let [
    my-puzzle [
[2 0 0  0 9 4  0 0 3]
   [0 0 3  0 0 0  6 0 0]
   [0 6 0  2 0 0  0 9 0]

   [0 3 0  0 0 0  7 0 0]
   [8 0 0  0 5 0  0 0 6]
   [0 0 5  0 0 0  0 3 0]

   [0 9 0  0 0 7  0 6 0]
   [0 0 6  0 0 0  1 0 0]
   [7 0 0  5 3 0  0 0 4]
    ]
    transpuzz (transform my-puzzle)
    final (solve transpuzz)
  ]
    (println "Your puzzle:")
    (print-sudoku my-puzzle)
    (println)

    (println "Solved:")
    (print-sudoku final)
  )
)

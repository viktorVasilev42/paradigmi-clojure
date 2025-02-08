(ns homework1)

; GRUPA 1

; a
(defn atomic? [v]
  (not (coll? v))
)

; b
(defn member? [x, lst]
  (cond
    (empty? lst) false
    (= x (first lst)) true
    :else (recur x (rest lst))
  )
)

; v
(defn my-count [lst]
  (cond
    (empty? lst) 0
    :else (inc (my-count (rest lst)))
  )
)

; g
(defn append [lst1, lst2]
  (cond
    (empty? lst1) lst2
    :else (cons (first lst1) (append (rest lst1) lst2))
  )
)

; d
(defn zip [lst1, lst2]
  (cond
    (or (empty? lst1) (empty? lst2)) '()
    :else (cons (list (first lst1) (first lst2)) (zip (rest lst1) (rest lst2)))
  )
)

; gj
(defn lookup [k, list-of-pairs]
  (cond
    (empty? list-of-pairs) nil
    (list? (first list-of-pairs))
      (if (= k (first (first list-of-pairs)))
        (second (first list-of-pairs))
        (lookup k (rest list-of-pairs))
      )
    :else
      (lookup k (rest list-of-pairs))
  )
)

; e
(defn my-merge [lst1, lst2]
  (cond
    (empty? lst1)
      lst2
    (empty? lst2)
      lst1
    (< (first lst1) (first lst2))
      (cons (first lst1) (my-merge (rest lst1) lst2))
    :else
      (cons (first lst2) (my-merge lst1 (rest lst2)))
  )
)

; zh
(defn count-all [lst]
  (cond
    (empty? lst) 0
    (coll? (first lst)) (+ (count-all (first lst)) (count-all (rest lst)))
    :else (inc (count-all (rest lst)))
  )
)

; z
(defn my-drop [n, lst]
  (cond
    (empty? lst) '()
    (= n 0) lst
    :else (my-drop (dec n) (rest lst))
  )
)

; dz
(defn my-take [n, lst]
  (letfn [
    (my-take-inner [in, ilst, iacc]
      (cond
        (or (= in 0) (empty? ilst)) (reverse iacc)
        :else (recur (dec in) (rest ilst) (cons (first ilst) iacc))
      )
    )
  ]
    (my-take-inner n lst '())
  )
)

; i
(defn my-reverse [lst]
  (letfn [
    (my-reverse-inner [ilst, iacc]
      (cond
        (empty? ilst) iacc
        :else (recur (rest ilst) (cons (first ilst) iacc))
      )
    )
  ]
    (my-reverse-inner lst '())
  )
)

; j
;
; (defn remove-duplicates [lst]
  ; (seq (set lst))
; )
(defn remove-duplicates [lst]
  (letfn [
    (remove-dupl-inner [ilst, iacc]
      (cond
        (empty? ilst) (reverse iacc)
        (member? (first ilst) iacc) (recur (rest ilst) iacc)
        :else (recur (rest ilst) (cons (first ilst) iacc))
      )
    )
  ]
    (remove-dupl-inner lst '())
  )
)

; k
(defn my-flatten [list-of-lists]
  (cond
    (empty? list-of-lists)
      '()
    (list? (first list-of-lists))
      (append (first list-of-lists) (my-flatten (rest list-of-lists)))
    :else
      (cons (first list-of-lists) (my-flatten (rest list-of-lists)))
  )
)

; GRUPA 2

; a
(defn buzz [list-of-ints]
  (map (fn[x]
      (cond
        (or
          (int? (/ x 5))
          (member? \5 (seq (str x)))
        )
          :buzz
        :else
          x
      )
    )
  list-of-ints)
)

; b
(defn divisors-of-n [n]
  (filter
    (fn[x]
      (= (mod n x) 0)
    )
    (range 2 n)
  )
)

; v
(defn longest [list-of-strings]
  (reduce
    (fn[x, y]
      (cond
        (> (count y) (count x)) y
        :else x
      )
    )
    list-of-strings
  )
)

; GRUPA 3

; a
(defn my-map [f, lst]
  (cond
    (empty? lst) '()
    :else (cons (f (first lst)) (my-map f (rest lst)))  
  )  
)

; b
(defn my-filter [pred?, lst]
  (cond
    (empty? lst) '()
    (pred? (first lst)) (cons (first lst) (my-filter pred? (rest lst)))
    :else (my-filter pred? (rest lst))
  )  
)

; v
(defn my-reduce
  ([f lst]
    (my-reduce f (first lst) (rest lst))
  )

  ([f value lst]
    (cond
      (empty? lst) value  
      :else (my-reduce f (f value (first lst)) (rest lst))
    )
  )
)

; g
(defn my-flat-map [f lst]
  (cond
    (empty? lst) '()
    :else (append (f (first lst)) (my-flat-map f (rest lst)))
  )
)

(defn -main [] 
  (let [
        result (zip '(1 2 3) '(a b c))
  ]
    (println result)  
  )
)

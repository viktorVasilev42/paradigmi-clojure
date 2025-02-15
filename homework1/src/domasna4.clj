(ns domasna4)

; GRUPA 1

; a
(defn atomic? 
  "vraka true ako v ne e collection"
  [v]
  (not (coll? v))
)

; b
(defn member? 
  "vraka true ako x e element vo lst"
  [x, lst]
  (cond
    (empty? lst) false
    (= x (first lst)) true
    :else (recur x (rest lst))
  )
)

; v
(defn my-count 
  "vraka broj na elementi vo lista"
  [lst]
  (cond
    (empty? lst) 0
    :else (inc (my-count (rest lst)))
  )
)

; g
(defn append 
  "concat na lst1 i lst2 vo edna lista"
  [lst1, lst2]
  (cond
    (empty? lst1) lst2
    :else (cons (first lst1) (append (rest lst1) lst2))
  )
)

; d
(defn zip 
  "gi kombinira lst1 i lst2 vo edinstvena lista so podlisti od tip (a b)
  kade a e od lst1, b e od lst2, a i b se naogaat na ista pozicija (indeks) vo soodvetnite listi"
  [lst1, lst2]
  (cond
    (or (empty? lst1) (empty? lst2)) '()
    :else (cons (list (first lst1) (first lst2)) (zip (rest lst1) (rest lst2)))
  )
)

; gj
(defn lookup 
  "vo lista od (key value) podlisti za daden key vrati go value od podlistata kade se naoga key"
  [k, list-of-pairs]
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
(defn my-merge 
  "spoi dve listi koi se veke odnapred sortirani vo edinstvena sortirana lista"
  [lst1, lst2]
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
(defn count-all 
  "izbroj atomicni elementi vo site nivoa na listata"
  [lst]
  (cond
    (empty? lst) 0
    (coll? (first lst)) (+ (count-all (first lst)) (count-all (rest lst)))
    :else (inc (count-all (rest lst)))
  )
)

; z
(defn my-drop 
  "izvadi prvi n elementi od lst"
  [n, lst]
  (cond
    (empty? lst) '()
    (= n 0) lst
    :else (my-drop (dec n) (rest lst))
  )
)

; dz
(defn my-take 
  "zemi gi samo prvite n elementi od lst"
  [n, lst]
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
(defn my-reverse 
  "prevrti gi elementite od lst vo obraten redosled"
  [lst]
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
(defn remove-duplicates 
  "otstrani duplikati vo ramki na listata"
  [lst]
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
(defn my-flatten 
  "otstrani edno nivo na vnatresni listi vo list-of-lists"
  [list-of-lists]
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
(defn buzz 
  "zameni gi so :buzz
  site elementi koi se delivi so 5 ili ja sodrzat cifrata 5"
  [list-of-ints]
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
(defn divisors-of-n 
  "najdi deliteli na n, razlicni od 1 i samiot n"
  [n]
  (filter
    (fn[x]
      (= (mod n x) 0)
    )
    (range 2 n)
  )
)

; v
(defn longest 
  "najdi najdolg string vo lista od strings"
  [list-of-strings]
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
(defn my-map 
  "primeni funkcija f na sekoj element od lst
  i vrati lista od rezultati"
  [f, lst]
  (cond
    (empty? lst) '()
    :else (cons (f (first lst)) (my-map f (rest lst)))  
  )  
)

; b
(defn my-filter 
  "primeni predikat pred? vrz sekoj element od lst
  i vrati lista od elementite koi vratile true"
  [pred?, lst]
  (cond
    (empty? lst) '()
    (pred? (first lst)) (cons (first lst) (my-filter pred? (rest lst)))
    :else (my-filter pred? (rest lst))
  )  
)

; v
(defn my-reduce
  "ja primenuva dvo-argumentnata funkcija f vrz value? i prviot
  element od sekvencata lst ako voopshto se napravi povik so naveden
  argument value, inaku ja primenuva funkcijata vrz prvite dva
  elementi od sekvencata; vo sledniot chekor funkcijata se primenuva
  vrz rezultatot od prviot ekor i sledniot (vtor ili tret) element
  vo sekvencata i postapkata prodolzuva dodeka ne se izminat site
  elementi od lst"
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
(defn my-flat-map 
  "primenuva funkcija f koja vraka lista vrz sekoj element
  i ja izramnuva rezultantnata lista (otstranuva 1 nivo)"
  [f lst]
  (cond
    (empty? lst) '()
    :else (append (f (first lst)) (my-flat-map f (rest lst)))
  )
)

(defn -main [] 
  (let [
        result (my-reduce (fn[x,y] (+ x y)) '(1 2 3))
  ]
    (println result)  
  )
)

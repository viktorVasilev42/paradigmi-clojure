(ns poker)

(defn suit 
  "vraka boja na karta"
  [card]
  (second card)
)

(defn rank 
  "vraka vrednost na karta"
  [card]
  (let [
    first-char (first card)
    cardmap {\T 10, \J 11, \Q 12, \K 13, \A 14}
  ]
    (cond
      (Character/isDigit first-char) (Integer/valueOf (str first-char))
      :else (get cardmap first-char)
    )
  )
)



(defn get-ranks-of-full-house 
  "za full house
  zemi vrednost na trite karti so ista vrednost
  i zemi vrednost na ostanatite dve karti"
  [hand]
  (let [
    freqs (frequencies
      (map
        #(rank %)
        hand
      )
    )
    rank-of-three (key (some
      #(when (= 3 (val %)) %)
      freqs
    ))
    rank-of-two (key (first (dissoc freqs rank-of-three)))
  ]
    (list rank-of-three rank-of-two)
  )
)

(defn full-house? 
  "proverka za dali rakata e full-house"
  [hand]
  (let [
    freqs (frequencies
      (map
        (fn[x] (rank x))
        hand
      )
    )
  ]
    (and
      (some (fn[x] (= 3 (val x))) freqs)
      (some (fn[x] (= 2 (val x))) freqs)
    )
  )
)

(defn get-ranks-of-three-of-kind 
  "za three of a kind
  zemi vrednosta na trite karti so ista vrednost
  potoa povisokata vrednost od dvete ostanati
  i na kraj poslednata vrednost"
  [hand]
  (let [
    freqs (frequencies
      (map
        #(rank %)
        hand
      )
    )
    rank-of-three (key (some
      #(when (= 3 (val %)) %)
      freqs
    ))
    freqs (dissoc freqs rank-of-three)
    second-rank (key (first freqs))
    third-rank (key (first (dissoc freqs second-rank)))
  ]
    (cond
      (>= second-rank third-rank)
        (list rank-of-three second-rank third-rank)
      :else
        (list rank-of-three third-rank second-rank)
    )
  )
)

(defn three-of-a-kind? 
  "proveri dali rakata e three of a kind"
  [hand]
  (and
    (not (full-house? hand))
    (some
      (fn[x] (= 3 (val x)))
      (frequencies
        (map
          (fn[x] (rank x))
          hand
        )
      )
    )
  )
)

(defn get-ranks-of-four-of-kind 
  "za four of a kind
  zemi vrednost na chetirite karti so ista vrednost
  i na kraj preostanatata vrednost"
  [hand]
  (let [
    freqs (frequencies
      (map
        #(rank %)
        hand
      )
    )
    rank-of-four (key (some
      #(when (= 4 (val %)) %)
      freqs
    ))
  ]
    (list rank-of-four (key (first (dissoc freqs rank-of-four))))
  )
)

(defn four-of-a-kind? 
  "proveri dali rakata e four of a kind"
  [hand]
  (some
    (fn[x] (= 4 (val x)))
    (frequencies
      (map
        (fn[x] (rank x))
        hand
      )
    )
  )
)

(defn are-in-sequence? 
  "proveri dali kartite vo rakata se vo redosled
  bez razlika vo koj redosled se navistina vo rakata"
  [hand]
  (letfn [
    (inner-are-seq [rank-hand, prev-rank]
      (cond
        (empty? rank-hand)
          true

       (= 14 (first rank-hand))
          (or
            (inner-are-seq (rest rank-hand) 14)
            (inner-are-seq (rest rank-hand) 1)
          )

        (= (first rank-hand) (inc prev-rank))
          (inner-are-seq (rest rank-hand) (first rank-hand))

        :else
          false
      )
    )
  ]
    (let [
      sorted-ranks (sort (map (fn[x] (rank x)) hand))
    ]
      (inner-are-seq
        sorted-ranks
        (dec (first sorted-ranks))
      )
    )
  )
)

(defn flush? 
  "proveri dali rakata e flush"
  [hand]
  (and
    (some
      (fn[x] (= 5 (val x)))
      (frequencies
        (map
          (fn[x] (suit x))
          hand
        )
      )
    )
    (not (are-in-sequence? hand))
  )
)



(defn get-ranks-of-two-pairs-desc 
  "za two pairs
  prvo zemi ja povisokata vrednost od vrednostite na dvata para
  potoa poniskata vrednost
  i na kraj nesparenata vrednost"
  [hand]
  (let [
    freqs (frequencies
      (map
        #(rank %)
        hand
      )
    )
    first-rank (key
      (some
        #(when (= 2 (val %)) %)
        freqs
      )
    )
    freqs (dissoc freqs first-rank)
    second-rank (key
      (some
        #(when (= 2 (val %)) %)
        freqs
      )
    )
    freqs (dissoc freqs second-rank)
    last-left (key (first freqs))
  ]
    (cond
      (> first-rank second-rank)
        (list first-rank second-rank last-left)
      :else
        (list second-rank first-rank last-left)
    )
  )
)

(defn two-pairs? 
  "proveri dali rakata e two pairs"
  [hand]
  (let [
    freqs (frequencies
      (map
        (fn[x] (rank x))
        hand
      )
    )
  ]
    (and
      (not (full-house? hand))
      (some (fn[x] (= 2 (val x))) freqs)
      (some
        (fn[x] (= 2 (val x)))
        (dissoc
          freqs
          (key (some (fn[x] (when (= 2 (val x)) x)) freqs))
        )
      )
    )
  )
)

(defn get-ranks-of-pair 
  "za pair
  zemi ja vrednosta na dvete karti so ista vrednost
  potoa ostanatite vrednosti na karti vo opagacki redosled"
  [hand]
  (let [
    freqs (frequencies
      (map
        #(rank %)
        hand
      )
    )
    rank-of-pair (key
      (some
        #(when (= 2 (val %)) %)
        freqs
      )
    )
    others (reverse (sort (map #(key %) (dissoc freqs rank-of-pair))))
  ]
    (cons rank-of-pair others)
  )
)

(defn pair? 
  "proveri dali rakata e pair"
  [hand]
  (some
      (fn[x] (= 2 (val x)))
      (frequencies
        (map
          (fn[x] (rank x))
          hand
        )
    )
  )
)

(defn straight? 
  "proveri dali rakata e straight"
  [hand]
  (and
    (are-in-sequence? hand)
    (not-any?
      (fn[x] (= 5 (val x)))
      (frequencies
        (map
          (fn[x] (suit x))
          hand
        )
      )
    )
  )
)

(defn straight-flush? 
  "proveri dali rakata e i straight i flush"
  [hand]
  (and
    (are-in-sequence? hand)
    (some
      (fn[x] (= 5 (val x)))
      (frequencies
        (map
          (fn[x] (suit x))
          hand
        )
      )
    )
  )
)

(defn value 
  "vraka jachina na kombincijata (rakata)"
  [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0
  )
)


(defn kickers 
  "vraka lista od vrednosti za dadena kombinacija
  koja se korsti pri ednakvost na jachina na kombinacija"
  [hand]
  (let [
    curr-value (value hand)
  ]
    (cond
      (= curr-value 2)
        (get-ranks-of-two-pairs-desc hand)
      (= curr-value 6)
        (get-ranks-of-full-house hand)
      (= curr-value 7)
        (get-ranks-of-four-of-kind hand)
      (= curr-value 3)
        (get-ranks-of-three-of-kind hand)
      (= curr-value 1)
        (get-ranks-of-pair hand)
      :else
        (reverse  
          (sort (map (fn[x] (rank x)) hand))
        )
    )
  )
)

(defn higher-kicker? 
  "proveri koj ima podobra kombinacija (raka) pri ednakvost
  na jachinata na rakata"
  [kicker1, kicker2]
  (cond
    (or (empty? kicker1) (empty? kicker2))
      false
    (> (first kicker1) (first kicker2))
      true
    (< (first kicker1) (first kicker2))
      false
    :else
      (higher-kicker? (rest kicker1) (rest kicker2))
  )
)

(defn beats? 
  "proveri koja od dve kombinacii e pobednik"
  [hand1, hand2]
  (let [
    value1 (value hand1)
    value2 (value hand2)
  ]
    (cond
      (> value1 value2) true
      (< value1 value2) nil
      :else
        (cond
          (true? (higher-kicker? (kickers hand1) (kickers hand2)))
            true
          :else
            nil
        )
    )
  )
)

(defn winning-hand 
  "za lista od kombinacii vrati koja od niv e pobednicka"
  [& hands]
  (reduce
    (fn[x, y]
      (cond
        (coll? (first x))
          (cond
            (beats? (first x) y)
              x
            (beats? y (first x))
              y
            :else
              (conj x y)
          )
        (beats? x y)
          x
        (beats? y x)
          y
        :else
          (list x y)
      )
    )
    hands
  )
)

(defn -main []
  (let [
    result (winning-hand
             ["2D" "2H" "8H" "TC" "TD"]
             ["2D" "2H" "2C" "TC" "TD"]
             ["2D" "2H" "2C" "TC" "TH"]
    )
  ]
    (println result)
  )
)

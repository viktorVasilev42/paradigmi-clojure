(ns poker-test
  (:require [clojure.test :refer [deftest, is, run-tests]]
    [poker :as hw3]
  )  
)

(def high-seven ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(deftest test-suit
  (is (= \H (hw3/suit "JH")))  
  (is (= \S (hw3/suit "TS")))  
  (is (= \C (hw3/suit "2C")))  
  (is (= \D (hw3/suit "QD")))  
  (is (= \H (hw3/suit "9H")))  
)

(deftest test-rank
  (is (= 11 (hw3/rank "JH")))  
  (is (= 10 (hw3/rank "TS")))  
  (is (= 2 (hw3/rank "2C")))  
  (is (= 12 (hw3/rank "QD")))  
  (is (= 9 (hw3/rank "9H")))  
)

(deftest test-pair?
  (is (true? (hw3/pair? pair-hand)))
  (is (not(true? (hw3/pair? three-of-a-kind-hand))))
)

(deftest test-three-of-a-kind?
  (is (true? (hw3/three-of-a-kind? three-of-a-kind-hand)))
  (is (not(true? (hw3/three-of-a-kind? four-of-a-kind-hand))))
)

(deftest test-four-of-a-kind?
  (is (true? (hw3/four-of-a-kind? four-of-a-kind-hand)))
  (is (not(true? (hw3/three-of-a-kind? flush-hand))))
)

(deftest test-flush?
  (is (true? (hw3/flush? flush-hand)))
  (is (not(true? (hw3/flush? straight-hand))))
)

(deftest test-full-house?
  (is (true? (hw3/full-house? full-house-hand)))
  (is (not(true? (hw3/full-house? three-of-a-kind-hand))))
)

(deftest test-two-pairs?
  (is (true? (hw3/two-pairs? two-pairs-hand)))
  (is (not(true? (hw3/two-pairs? four-of-a-kind-hand))))
)

(deftest test-straight?
  (is (true? (hw3/straight? straight-hand)))
  (is (not(true? (hw3/straight? straight-flush-hand))))
)

(deftest test-straight-flush?
  (is (true? (hw3/straight-flush? straight-flush-hand)))
  (is (not(true? (hw3/straight-flush? full-house-hand))))
)

(deftest test-value
  (is (= 5 (hw3/value flush-hand)))  
  (is (= 4 (hw3/value straight-hand)))  
  (is (= 8 (hw3/value straight-flush-hand)))  
  (is (= 2 (hw3/value two-pairs-hand)))  
  (is (= 7 (hw3/value four-of-a-kind-hand)))  
)

(deftest test-kickers
  (is (= '(9 7 5 4 2) (hw3/kickers flush-hand)))  
  (is (= '(14 13 12 11 10) (hw3/kickers high-ace-straight-flush-hand)))  
  (is (= '(4 2 7) (hw3/kickers two-pairs-hand)))  
  (is (= '(2 7) (hw3/kickers four-of-a-kind-hand)))  
  (is (= '(2 7 4) (hw3/kickers three-of-a-kind-hand)))  
)

(deftest test-higher-kicker?
  (is (false? 
    (hw3/higher-kicker?
      '(8 5 9)  
      '(8 7 3)
    )
  ))

  (is (true? 
    (hw3/higher-kicker?
      '(8 7 3)
      '(8 5 9)  
    )
  ))
)

(deftest test-beats?
  (is (true?
    (hw3/beats? straight-flush-hand straight-hand)
  ))

  (is (not (true?
    (hw3/beats? straight-hand straight-flush-hand)
  )))
)

(deftest test-winning-hand
  ; koj e pobednik od two-pair i four-of-a-kind
  ; treba da vrati four-of-a-kind-hand
  (is (=
    four-of-a-kind-hand
    (hw3/winning-hand
      two-pairs-hand
      four-of-a-kind-hand
    )    
  ))

  ; koj e pobednik od pair, two-pair i three-of-a-kind
  ; treba da vrati three-of-a-kind
  (is (=
    three-of-a-kind-hand
    (hw3/winning-hand
      pair-hand
      two-pairs-hand
      three-of-a-kind-hand  
    )    
  ))

  ; koj e pobednik od dve three-of-a-kind kombinacii
  ; igraat kickers '(2 7 4) i '(2 7 5)
  ; pobeduva vtoroto znaci ja vraka vtorata kombinacija
  (is (=
    ["2H" "2S" "2C" "5H" "7D"]
    (hw3/winning-hand
      three-of-a-kind-hand
      ["2H" "2S" "2C" "5H" "7D"]
    )    
  ))

  ; koj e pobednik od dve three-of-a-kind kombinacii
  ; igraat kickers '(2 7 4) i '(2 7 3)
  ; pobeduva prvoto znaci ja vraka prvata kombinacija
  (is (=
    three-of-a-kind-hand
    (hw3/winning-hand
      three-of-a-kind-hand
      ["2H" "2S" "2C" "3H" "7D"]
    )    
  ))

  ; koj e pobednik pomegju dve full-house kombinacii
  ; istiot par vrednosti, samo se razlikuvaat vo boi
  ; igraat kickers '(2 5) i '(2 5)
  ;
  ; izednacheno e, taka shto rezultatot e niza od
  ; dvete kombinacii
  (is (=
    [
      full-house-hand,
      ["5C" "5H" "2C" "2S" "2D"]
    ]
    (hw3/winning-hand
      full-house-hand
      ["5C" "5H" "2C" "2S" "2D"]
    )    
  ))
)

(run-tests)

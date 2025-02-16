(ns boja-test
  (:require [clojure.test :refer [deftest, is, run-tests]]
    [boja :as hw4]
  )  
)

(deftest test-shift-row
  (is (= '(3 1 2) (hw4/shift-row '(1 2 3) )))
  (is (= '(4 1 2 3) (hw4/shift-row '(1 2 3 4) )))
  (is (= '(3 4 1 2) (hw4/shift-row (hw4/shift-row '(1 2 3 4) ))))
  (is (= '(t x y z) (hw4/shift-row '(x y z t) )))
  (is (= '(z t x y) (hw4/shift-row (hw4/shift-row '(x y z t) ))))
)

(deftest test-check-diff
  (is (true? 
    (hw4/check-diff
      '(
         (1 2 3 4)
         (4 3 1 2)
         (2 1 4 3)
       )  
    )      
  ))

  (is (true? 
    (hw4/check-diff
      '(
         (1 2 3 4)
         (4 3 1 2)
         (2 1 4 3)
       )  
    )      
  ))

  (is (true? 
    (hw4/check-diff
      '(
         (3 1 0 2)
         (0 2 1 3)
         (2 0 3 1)
       )  
    )      
  ))

  (is (false? 
    (hw4/check-diff
      '(
         (3 1 0 2)
         (3 0 2 1)
         (2 0 3 1)
       )  
    )      
  ))

  (is (false? 
    (hw4/check-diff
      '(
         (3 1 0 2)
         (2 1 3 0)
         (1 2 0 3)
       )  
    )      
  ))
)

(deftest test-solve
  (is (=
    '(
       (2 0 3 1)
       (0 2 1 3)
       (3 1 0 2)
    )
    (hw4/solve
      '(
         (3 1 0 2)
         (2 1 3 0)
         (1 2 0 3)
       )  
    )     
  ))

  (is (nil?
    (hw4/solve
      '(
         (1 1 1 1)
         (0 2 1 3)
         (3 1 0 2)
       )  
    )     
  ))
)

(run-tests)

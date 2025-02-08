(ns test-homework1
  (:require [clojure.test :refer [deftest, is, run-tests]]
            [homework1 :as hw1]
  )  
)

(deftest test-atomic?
  (is (true? (hw1/atomic? 4))) ; proverka dali broj se smeta za atomic
  (is (true? (hw1/atomic? 'a))) ; proverka dali symbol se smeta za atomic
  (is (true? (hw1/atomic? "stringche"))) ; dali string se smeta za atomic
  (is (false? (hw1/atomic? '(1 2 3 4)))) ; dali list se smeta za atomic
  (is (false? (hw1/atomic? '[1 2 3 4]))) ; dali vector se smeta za atomic
  (is (false? (hw1/atomic? #{1 2 3 4}))) ; dali set se smeta za atomic
)

(deftest test-member?
  (is (true? (hw1/member? 'a '(b c a d)))) ; proverka za symbol
  (is (true? (hw1/member? '1 '(b 1 a d)))) ; proverka za broj 
  (is (true? (hw1/member? '(1 2 3) '(x y (1 2 3) z y)))) ; proverka za lista
  (is (true? (hw1/member? 'a '[b 1 a d]))) ; proverka za member vo vector
  (is (true? (hw1/member? '(1 2) '[b (1 2) a d]))) ; proverka za lista member vo vector
)

(deftest test-my-count
  (is (= 4 (hw1/my-count '(1 2 3 4)))) ; proverka za lista od broevi
  (is (= 2 (hw1/my-count '((1 2) (3 4))))) ; proverka za lista od listi
  (is (= 3 (hw1/my-count '[1 b 3]))) ; proverka za vector
  (is (= 2 (hw1/my-count '([a b] [c d])))) ; proverka za lista od vectori
  (is (= 3 (hw1/my-count '("abc" "xyz" "test")))) ; proverka za lista od stringovi
)

(deftest test-append
  ; concat na dve listi od numbers
  (is (= '(1 2 3 4) (hw1/append '(1 2) '(3 4))))

  ; concat na lista od symbols i lista od nubmers
  (is (= '(a b c 4) (hw1/append '(a b c) '(4))))

  ; concat na dve prazni listi
  (is (= '() (hw1/append '() '())))

  ; concat na dve listi od listi
  (is (= '((1 2) (a b) (c d)) (hw1/append '((1 2)) '((a b) (c d)))))

  ; concat na dva vektora
  (is (= '(1 2 3 4) (hw1/append '[1 2 3] '[4])))
)

(deftest test-zip
  (is (= '((x a) (y b) (z c)) (hw1/zip '(x y z) '(a b c))))  
  (is (= '((1 a) (2 b) (3 c)) (hw1/zip '(1 2 3) '(a b c))))  
  (is (= '((1 a) (2 b)) (hw1/zip '(1 2) '(a b c))))  
  (is (= '() (hw1/zip '(a b c) '())))
  (is (= '((a a) (b b) (c c)) (hw1/zip '(a b c) '(a b c))))  
)

(run-tests)

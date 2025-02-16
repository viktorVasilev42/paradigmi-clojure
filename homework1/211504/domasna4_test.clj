(ns domasna4-test
  (:require [clojure.test :refer [deftest, is, run-tests]]
    [domasna4 :as hw1]
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
  ; zip na dve listi od symbols
  (is (= '((x a) (y b) (z c)) (hw1/zip '(x y z) '(a b c))))  

  ; zip na lista od numbers i lista od symbols
  (is (= '((1 a) (2 b) (3 c)) (hw1/zip '(1 2 3) '(a b c))))  

  ; zip na pokratka lista od numbers i lista od symbols
  (is (= '((1 a) (2 b)) (hw1/zip '(1 2) '(a b c))))  

  ; zip na lista i prazna lista
  (is (= '() (hw1/zip '(a b c) '())))

  ; zip na dve identichni listi
  (is (= '((a a) (b b) (c c)) (hw1/zip '(a b c) '(a b c))))  
)

(deftest test-lookup
  ; get value of string key
  (is (= 5 (hw1/lookup "viktor" '( ("vasil" 2) ("viktor" 5) ) )))

  ; get value of key that is not present
  (is (= nil (hw1/lookup "viktor" '( ("vasil" 2) ) )))

  ; test lookup for an empty list
  (is (= nil (hw1/lookup "viktor" '() )))

  ; get value of keyword key
  (is (= 5 (hw1/lookup :viktor '( (:vasil 2) (:viktor 5) (:boris 4) ) )))

  ; get value of number key
  (is (= 2 (hw1/lookup 3 '( (3 2) (4 5) (5 4) ) )))
)

(deftest test-my-merge
  ; merge dve sortirani listi
  (is (= '(1 2 3 4 5) (hw1/my-merge '(2 4) '(1 3 5) ) ))  

  ; merge dve sortirani listi (obraten redosled)
  (is (= '(1 2 3 4 5) (hw1/my-merge '(1 3 5) '(2 4) ) ))  

  ; merge - prvata lista ima eden element i e najgolem
  (is (= '(11 22 33 44) (hw1/my-merge '(44) '(11 22 33) ) ))  

  ; merge - prvata lista e prazna
  (is (= '(11 22 33) (hw1/my-merge '() '(11 22 33) ) ))  

  ; merge - vtorata lista e prazna
  (is (= '(11 22 33) (hw1/my-merge '(11 22 33) '() ) ))  
)


(deftest test-count-all
  ; izbroj lista 1 nivo
  (is (= 3 (hw1/count-all '(1 2 3) )))  

  ; izbroj lista 2 nivoa
  (is (= 3 (hw1/count-all '(1 (2 3)) )))  

  ; izbroj lista 3 nivoa
  (is (= 3 (hw1/count-all '(1 (2 (3))) )))  

  ; izbroj lista 4 nivoa
  (is (= 4 (hw1/count-all '(1 (2 (3) ((4)) )) )))  

  ; izbroj lista 4 nivoa no nekoi se inner listi se prazni
  (is (= 2 (hw1/count-all '(1 (2 () (()) )) )))  
)

(deftest test-my-drop
  ; drop prvi 2 elementi od lista
  (is (= '(3 4 5) (hw1/my-drop 2 '(1 2 3 4 5) )))

  ; dorp prvi 3 elementi od lista
  (is (= '(4 5) (hw1/my-drop 3 '(1 2 3 4 5) )))

  ; drop 0 elementi od lista
  (is (= '(1 2 3 4 5) (hw1/my-drop 0 '(1 2 3 4 5) )))

  ; dorp 6 elementi od lista so 5 elementi
  (is (= '() (hw1/my-drop 6 '(1 2 3 4 5) )))

  ; dorp ogromen broj elementi od lista so 5 elementi
  (is (= '() (hw1/my-drop 100000 '(1 2 3 4 5) )))
)

(deftest test-my-take
  ; take prvi dva elementi od lista
  (is (= '(1 2) (hw1/my-take 2 '(1 2 3 4 5))))  

  ; take prvi 3 elementi od lista
  (is (= '(1 2 3) (hw1/my-take 3 '(1 2 3 4 5))))  

  ; take 0 elementi od lista
  (is (= '() (hw1/my-take 0 '(1 2 3 4 5))))  

  ; take 6 elementi od lista so 5 elementi
  (is (= '(1 2 3 4 5) (hw1/my-take 6 '(1 2 3 4 5))))  

  ; take ogromen broj elementi od lista so 5 elementi
  (is (= '(1 2 3 4 5) (hw1/my-take 100000 '(1 2 3 4 5))))  
)

(deftest test-my-reverse
  ; reverse lista od 3 elementi
  (is (= '(3 2 1) (hw1/my-reverse '(1 2 3) )))  

  ; reverse lista so inner lista so eden element
  (is (= '((3) 2 1) (hw1/my-reverse '(1 2 (3)) )))  

  ; reverse lista so inner lista so dva elementi
  (is (= '((2 3) 1) (hw1/my-reverse '(1 (2 3)) )))  

  ; reverse lista so poveke nivoa inner lists
  (is (= '((2 (3)) 1) (hw1/my-reverse '(1 (2 (3))) )))  

  ; reverse prazna lista
  (is (= '() (hw1/my-reverse '() )))  
)

(deftest test-remove-duplicates
  ; otstrani duplikati za lista bez duplikati
  (is (= '(1 2 3 4 5) (hw1/remove-duplicates '(1 2 3 4 5) )))  

  ; otstrani duplikati za lista so duplikati
  (is (= '(1 2 3) (hw1/remove-duplicates '(1 2 3 2 3) )))  

  ; proveri dali se zapazuva redosled
  (is (= '(3 1 2) (hw1/remove-duplicates '(3 1 2 3 2 3) )))  

  ; otstrani duplikati za prazna lista
  (is (= '() (hw1/remove-duplicates '() )))  

  ; otstrani duplikati za lista so inner-list
  (is (= '(3 1 2 (3)) (hw1/remove-duplicates '(3 1 2 (3) 2 3) )))  
)

(deftest test-my-flatten
  ; flatten edno nivo na lista
  (is (= '(1 2 3 4 5) (hw1/my-flatten '((1 2 3 4 5)) )))  

  ; flatten edno nivo na lista
  (is (= '(1 2 3 4 5) (hw1/my-flatten '((1 2 3) (4 5)) )))  

  ; proverka dali ostanuvaat podlabokite nivoa
  (is (= '(1 2 (3) 4 5) (hw1/my-flatten '((1 2 (3)) (4 5)) )))  

  ; flatten lista od prazni listi
  (is (= '() (hw1/my-flatten '(() () ()) )))  

  ; proverka dali ostanuva prazna lista od vtoro nivo
  (is (= '(()) (hw1/my-flatten '(() (()) ()) )))  
)

(deftest test-buzz
  ; test za lista bez buzz
  (is (= '(7 3) (hw1/buzz '(7 3) )))  

  ; test za lista so element 5
  (is (= '(:buzz 3) (hw1/buzz '(5 3) )))  

  ; test za lista so element deliv so 5 i uste
  ; eden koj sodrzi cifra 5
  (is (= '(:buzz 8 :buzz) (hw1/buzz '(10 8 51) )))  

  ; test za lista so 2 elementi delivi so 5
  ; eden od niv e 0
  (is (= '(:buzz :buzz 7) (hw1/buzz '(10 0 7) )))  

  ; test za prazna lista
  (is (= '() (hw1/buzz '() )))  
)

(deftest test-divisors-of
  ; najdi deliteli na 6
  (is (= '(2 3) (hw1/divisors-of-n 6)))

  ; najdi deliteli na 20
  (is (= '(2 4 5 10) (hw1/divisors-of-n 20)))

  ; deliteli na 1 treba da vrati prazna lista
  (is (= '() (hw1/divisors-of-n 1)))

  ; deliteli na 2 treba da vrati prazna lista
  ; bidejki negovi deliteli se 1 i samiot broj 2
  (is (= '() (hw1/divisors-of-n 2)))

  ; deliteli na prost broj treba da vrati prazna lista
  (is (= '() (hw1/divisors-of-n 7)))
)

(deftest test-longest
  ; najdi podolg od empty string i string
  (is (= "longest" (hw1/longest '("" "longest") )))

  ; najdi podolg od dva stringa
  (is (= "longest" (hw1/longest '("test" "longest") )))

  ; najdi najdolg od 3 stringa
  (is (= "longest" (hw1/longest '("test" "longest" "abc") )))

  ; najdi najdolg od 3 stringa od koi dva se so ista dolzina
  ; treba da go vrati prviot
  (is (= "longest" (hw1/longest '("test" "longest" "samelen") )))

  ; najdi najdolg od 2 prazni stringa
  ; treba da vrati prazen string
  (is (= "" (hw1/longest '("" "") )))
)

(deftest test-my-map
  ; podeli gi site broevi so 2
  (is (=
    '(1 2 3)
    (hw1/my-map 
      (fn[x] (/ x 2))  
      '(2 4 6)
    )
  ))

  ; mapiraj gi site elementi vo 10
  (is (=
    '(10 10 10 10)
    (hw1/my-map 
      (fn[_] 10)  
      '(1 2 3 4)
    )
  ))

  ; dodadi 5 pa povtorno dodadi 5 na sekoj element
  (is (=
    '(11 12 13)
    (hw1/my-map 
      (fn[x] (+ 5 (+ 5 x)))
      '(1 2 3)
    )
  ))

  ; dodadi 5 pa povtorno dodadi 5 na sekoj element
  (is (=
    '("1abc" "2abc" "3abc")
    (hw1/my-map 
      (fn[x] (str (str x) "abc"))  
      '(1 2 3)
    )
  ))

  ; mapiraj prazna lista - treba da vrati prazna lista
  (is (=
    '()
    (hw1/my-map 
      (fn[x] (+ 2 x))  
      '()
    )
  ))
)

(deftest test-my-filter
  ; neka ostanat samo parnite broevi vo novata lista
  (is (=
    '(2 4)
    (hw1/my-filter 
      (fn[x] (= 0 (rem x 2)))  
      '(1 2 3 4 5)
    )
  ))

  ; neka ostanat samo parni broevi, no tuka nemame parni broevi
  ; vo originalnata lista
  (is (=
    '()
    (hw1/my-filter 
      (fn[x] (= 0 (rem x 2)))  
      '(1 3 5)
    )
  ))

  ; neka ostanat samo 5ki vo novata lista
  (is (=
    '(5 5)
    (hw1/my-filter 
      (fn[x] (= 5 x))  
      '(1 2 5 3 4 5)
    )
  ))

  ; neka ostanat samo 3ki vo novata lista
  ; korstime poveke funkcii tuka
  (is (=
    '(3)
    (hw1/my-filter 
      (fn[x] (= 5 (+ 2 x)))  
      '(1 2 5 3 4 5)
    )
  ))

  ; neka ostanat samo inner lists koi imaat tocno 3 elementi
  (is (=
    '((1 2 3) (6 7 8))
    (hw1/my-filter 
      (fn[x] (= 3 (count x)))  
      '((1 2 3) (4 5) (6 7 8))
    )
  ))
)

(deftest test-my-reduce
  ; najdi zbir na elementi vo lista
  (is (=
    6
    (hw1/my-reduce
      (fn[x,y] (+ x y))  
      '(1 2 3)
    )
  ))

  ; najdi zbir na elementi vo lista
  ; no ovoj pat so function reference
  ; i pocnuvame od value 10
  (is (=
    16
    (hw1/my-reduce
      +
      10
      '(1 2 3)
    )
  ))

  ; najdi najgolem element vo lista
  (is (=
    9
    (hw1/my-reduce
      (fn[x,y] (max x y))  
      '(2 9 6 8)
    )
  ))

  ; spoi lista od stringovi vo eden string
  (is (=
    "eden dva tri"
    (hw1/my-reduce
      (fn[x,y] (str x y))  
      '("eden" " dva" " tri")
    )
  ))

  ; sekogash ke go bira prviot element
  ; vo listata
  (is (=
    4
    (hw1/my-reduce
      (fn[x,_] x)  
      '(4 6 7 9)
    )
  ))
)

(deftest test-my-flat-map
  (is (=
    '(1 2 3 4)
    (hw1/my-flat-map
      (fn[x] x)  
      '((1 2) (3 4))
    )
  ))

  (is (=
    '(1 2 2 3 3 4 4 5 5 6)
    (hw1/my-flat-map
      (fn[x] (list x (inc x)))  
      '(1 2 3 4 5)
    )
  ))

  (is (=
    '("sum" 6 "sum" 9)
    (hw1/my-flat-map
      (fn[x] (list "sum" (reduce + x)))
      '((1 2 3) (4 5))
    )
  ))

  (is (=
    '("abc" "cba" "xyz" "zyx")
    (hw1/my-flat-map
      (fn[x] (list x (reduce str (reverse x))))
      '("abc" "xyz")
    )
  ))

  (is (=
    '((1) (2) (3) (4))
    (hw1/my-flat-map
      (fn[x] (list (list x)))
      '(1 2 3 4)
    )
  ))
)

(run-tests)

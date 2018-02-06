(ns hw01.core
  (:gen-class))
;====================hw01======================
;Question 1
;This new-if funciton will return nil because of nature of clojure. Because clojure is an
;applicative language, it will evaluate the first thing, which will be 'print' with no arguement
;and that will return null. So it in short she will be very disappointed. 
;----------------------------------------------------------------------------------------------

;Question 2 
;Ben will see that in the applicative behavior interpreter gets caught in an infinite loop trying 
;the inside functions of the parenthese, in this case p is recursively defined and there for the 
;interpreter will never be able to move to evaluation. The normal order evaluater will imediately
;compare the given value, 0, to the test value of 0. Since this will be true, it will return 0 
;without evaluating anything else, there by avoiding p's loop definition.
;----------------------------------------------------------------------------------------------

;Question 3
; (defn avg [x y]
;   (/ (+ x y)2))

; (defn improve [guess number]
;   (float (avg guess (/ number guess))))

; (defn goodenough? [estimate number]
;   (if (and (< (- (Math/pow estimate 2) number) 0.001) (> (- (Math/pow estimate 2) number) -0.001)) true false))

; (defn sqrt [estimate number]
;   (if (goodenough? estimate number)
;     (float estimate)
;     (sqrt (improve estimate number) number)))

;----------------------------------------------------------------------------------------------

;Question 4
(def Stamps [10, 12, 16, 22, 25, 28, 34, 38, 40])

(defn isEqual? [L, x]
  (= 1 (reduce + L)))


(defn stampAdder [stamps combinations currentList]
  (cond (= (+ (reduce + currentList) (first stamps)) 100)
          (conj combinations (conj currentList (first stamps)))
        (< (+ (reduce + currentList) (first stamps)) 100)
          (if (= (next stamps) nil)
            '()
            (concat
              (stampAdder (rest stamps) combinations currentList) 
              (stampAdder (rest stamps) combinations (conj currentList (first stamps)))
            )
          )
        (> (+ (reduce + currentList) (first stamps)) 100)
          '()
  )
)


;----------------------------------------------------------------------------------------------
  
(defn -main
  "I don't do a whole lot ... yet."
  [& args]

  (println (stampAdder Stamps '() '())))

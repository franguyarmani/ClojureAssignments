(ns hw02.core
  (:gen-class))

(defn variable?
  " is e is a variable? "
  [e]
  (symbol? e))

(defn same-variable?
  " are v1 and v2 the same variable? "
  [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn make-sum
  " constructs the sum of a1 and a2 "
  [a1 a2]
  (cond (and (number? a1) (number? a2)) (+ a1 a2)
        (= a1 0) a2
        (= a2 0) a1  
        :else (list '+ a1 a2)))

(defn make-product
  " construct the product of m1 and m2 "
  [m1 m2]
  (cond (and (number? m1) (number? m2)) (* m1 m2)
        (or (= m1 0)(= m2 0)) 0
        (= m1 1) m2
        (= m2 1) m1   
        :else (list '* m1 m2)))

(defn make-exponentiation
  " construct the exponential with base e1 and exponent e2 "
  [e1 e2]
  (cond (= e1 0) 0
        (or (= e2 0)(= e1 1)) 1
        (= e2 1) e1 
        (and (number? e1) (number? e2)) (Math/pow e1 e2)  
        :else (list '** e1 e2)))

(defn sum?
  " is e a sum? "
  [e]
  (and (list? e)
       (= (first e) '+)))

(defn addend
  " gets addend (first term in an addition) of the sum s "
  [s]
  (second s))

(defn augend
  " gets augend (second term in an addition) of the sum s "
  [s]
  (last s))


(defn product?
  " is p a product? "
  [p]
  (and (list? p)
       (= (first p) '*)))

(defn multiplier
  " multiplier of the product p "
  [p]
  (second p))

(defn multiplicand
  " multiplicand of the product p "
  [p]
  (last p))


(defn exponentiation?
  " is e an exponent? "
  [e]
  (and (list? e)
       (= (first e) '**)))
  
(defn exponent
  " exponent of the exponential e "
  [e]
  (last e))
  
(defn base
  " base of the exponential e "
  [e]
  (second e))


(defn deriv
  " takes the derivative of exp with respect to var "
  [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))
        (product? exp) (make-sum
                        (make-product (multiplier exp)
                                      (deriv (multiplicand exp) var))
                        (make-product (deriv (multiplier exp) var)
                                      (multiplicand exp)))
        (exponentiation? exp) (make-product 
                            (exponent exp)(make-exponentiation (base exp) (- (exponent exp) 1))) 
        :else (throw (Exception. "unknown expression type -- DERIV" exp))))

  

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

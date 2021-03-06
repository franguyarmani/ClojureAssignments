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
  (list '+ a1 a2))

(defn make-product
  " construct the product of m1 and m2 "
  [m1 m2]
  (list '* m1 m2))

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
  " is e a product? "
  [e]
  (and (list? e)
       (= (first e) '*)))

(defn multiplier
  " multiplier of the product p "
  [p]
  (second p))


(defn multiplicand
  " multiplicand of the product p "
  [p]
  (last p))

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
        :else (throw (Exception. "unknown expression type -- DERIV" exp))))

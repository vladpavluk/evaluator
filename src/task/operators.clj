(ns task.operators)

(defmulti evaluate-simple-expression first)

(defmethod evaluate-simple-expression '+
  [[_ & args]]
  (apply + args))

(defmethod evaluate-simple-expression '-
  [[_ & args]]
  (apply - args))

(defmethod evaluate-simple-expression '*
  [[_ & args]]
  (apply * args))

(defmethod evaluate-simple-expression '/
  [[_ & args]]
  (apply / args))

(defmethod evaluate-simple-expression 'power
  [[_ & args]]
  (assert (= (count args) 2) "This function takes 2 arguments")
  (Math/pow (first args) (second args)))

(defmethod evaluate-simple-expression 'abs
  [[_ & args]]
  (assert (= (count args) 1) "This function takes 1 argument")
  (Math/abs (first args)))

(defmethod evaluate-simple-expression :default
  [[operand & _]]
  (if operand
    (throw (Exception. (str "Unknown operand - " operand)))
    (throw (Exception. "Unknown form"))))
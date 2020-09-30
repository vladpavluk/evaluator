(ns task.optimizers)

(defmulti optimize-simple-expression first)

(defmethod optimize-simple-expression '+
  [[_ & args]]
  (as-> args $
        (remove (partial = 0) $)
        (condp = (count $)
          0 0
          1 (first $)
          (apply list '+ $))))

(defmethod optimize-simple-expression '-
  [[_ & args]]
  (as-> args $
        (remove (partial = 0) $)
        (cond
          (= (count $) 0) 0
          (= (count $) 1) (first $)
          (and (= (count $) 2) (= (first $) (second $))) 0
          :else (apply list '- $))))

(defmethod optimize-simple-expression '*
  [[_ & args]]
  (if (some (partial = 0) args)
    0
    (as-> args $
          (remove (partial = 1) $)
          (condp = (count $)
            0 1
            1 (first $)
            (apply list '* $)))))

(defmethod optimize-simple-expression '/
  [[_ & args]]
  (as-> args $
        (remove (partial = 1) $)
        (cond
          (= (count $) 0) (apply list '/ args)
          (= (count $) 1) (first $)
          (and (= (count $) 2) (= (first $) (second $))) 1
          :else (apply list '/ $))))
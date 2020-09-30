(ns task.core
  (:require [clojure.walk :as walk]
            [task.operators :as operators]
            [task.optimizers :as optimizers]))

(defn- simple-form? [form]
  (when (list? form)
    (not (some list? (rest form)))))

(defn- extract-value
  "Returns either an arg itself, or, if 'arg' is a symbol,
   returns variable value for that symbol"
  [vars arg]
  (cond
    (symbol? arg)
    (if-let [arg-value (get vars (keyword arg))]
      arg-value
      (throw (Exception. (str "Unknown variable - " (name arg)))))
    :else arg))

(defn- substitute-vars-with-vals
  ;free to destruct here without additional asserts, as this fn
  ;is called against satisfying parameter
  [vars [operator & args]]
  (apply
    list operator
    (map (partial extract-value vars) args)))

(defn optimize
  [form]
  (walk/postwalk
    (fn [sub-form]
      (if (list? sub-form)
        (optimizers/optimize-simple-expression sub-form)
        sub-form))
    form))

(defn evaluate
  "Passed a form, evaluates it and returns value.
  Parameters:
  - environment - a map of keyword to value, where keyword represents variable name
  - form - a Lisp-like form to evaluate"
  [vars form]
  (->> form
       (optimize)
       (walk/postwalk
         (fn [subform]
           (if (simple-form? subform)
             (->> subform
                  (substitute-vars-with-vals vars)
                  (operators/evaluate-simple-expression))
             subform)))))
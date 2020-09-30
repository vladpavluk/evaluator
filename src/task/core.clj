(ns task.core
  (:require [clojure.walk :as walk]
            [task.operators :as operators]
            [task.optimizers :as optimizers]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn- simple-form?
  "Checks if form is simple, e.g. `evaluable`,
  by means it does not contain sub-forms"
  [form]
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
  "Forcibly replaces plain (non-nested) form values with respective variable
  values passed in `vars`. Returns new form"
  ;free to destruct here without additional asserts, as this fn
  ;is called against satisfying parameter
  [vars [operator & args]]
  (apply
    list operator
    (map (partial extract-value vars) args)))

(defn optimize
  "Passed a form, returns another possibly-optimized form"
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
  - vars - a map of keyword to value, where keyword represents variable name
  - form - a Lisp-like form to evaluate"
  [vars form]
  (->> form
       (optimize)
       (walk/postwalk
         (fn [sub-form]
           (if (simple-form? sub-form)
             (->> sub-form
                  (substitute-vars-with-vals vars)
                  (operators/evaluate-simple-expression))
             sub-form)))))

(defn ->javascript
  "Passed a form and fn-name, returns a string
  with JS representative named function.
  All symbols inside `form`, which are not operators, are considered
  to be formal parameters, and collected to the function signature"
  [fn-name form]
  (let [vars (atom #{})
        infix-form
        (as-> form $
              (optimize $)
              (walk/postwalk
                (fn [sub-form]
                  (when (simple-form? sub-form)
                    (swap! vars set/union (set (filter symbol? (rest sub-form)))))
                  (if (list? sub-form)
                    (interpose (first sub-form) (rest sub-form))
                    sub-form)) $))]
    (str "function " fn-name
         "(" (str/join "," @vars) ")"
         " { return " (str (seq infix-form)) "; }")))
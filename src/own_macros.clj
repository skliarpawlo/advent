(ns own-macros)  

(defmacro infix [body]
  (list (second body) (first body) (nth body 2)))

(infix (2 + 2))

(macroexpand-1 `(infix (2 + 2)))
(macroexpand `(when (= 2 1) 666))

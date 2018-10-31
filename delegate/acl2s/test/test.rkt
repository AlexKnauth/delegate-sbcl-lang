#lang delegate/acl2s

(defunc f (x)
  :input-contract (natp x)
  :output-contract (natp (f x))
  (+ x 1))

(check= (f 4) 5)

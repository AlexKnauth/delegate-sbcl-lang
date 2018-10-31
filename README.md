# delegate-sbcl-lang
Racket #langs that delegate to sbcl, acl2, and acl2s

## Installation

```
$ raco pkg install https://github.com/AlexKnauth/delegate-sbcl-lang
```

## Using the `#lang`s

### `#lang delegate/sbcl`

```racket
#lang delegate/sbcl

(defun f (x)
  (+ x 1))

(print (f 4))
;=> 5
```

### `#lang delegate/acl2`

```racket
#lang delegate/acl2

(defun f (x)
  (+ x 1))

(f 5)
;=> 6
```

### `#lang delegate/acl2s`

```racket
#lang delegate/acl2

(defunc f (x)
  :input-contract (natp x)
  :output-contract (natp (f x))
  (+ x 1))

(f 6)
;=> 7
```

# 02 Functions

 데이터로서 함수


## 2.1 Functions as Data

## 2.2 Defining Functions
``` lisp
 > (defun double (x) (* x 2))
DOUBLE

> #'double
#<Interpreted-Function C66ACE>

> (double 3)
6
```

``` lisp
> (eq #'double (car (list #'double)))
T
```

``` lisp
> #'(lambda (x) (* x 2))
#<Interpreted-Function C674CE>

> ((lambda (x) (* x 2)) 3)
6
```

``` lisp
lisp1 / lisp2
> (setq double 2)
2
> (double double)
4
```

``` lisp
> (symbol-value 'double)
2
> (symbol-function 'double)
#<Interpreted-Function C66ACE>

> (setq x #'append)
#<Compiled-Function 46B4BE>
> (eq (symbol-value 'x) (symbol-function 'append))
T
```

``` lisp
(defun double (x) (* x 2))
(setf (symbol-function 'double)
#'(lambda (x) (* x 2)))
```

## 2.3 Functional Arguments

``` lisp


(+ 1 2)
(apply #'+ '(1 2))
(apply (symbol-function '+) '(1 2))
(apply #'(lambda (x y) (+ x y)) '(1 2))

```

``` lisp
(apply #'+ 1 '(2))
(funcall #'+ 1 2)
> (mapcar #'(lambda (x) (+ x 10))
          '(1 2 3))
(11 12 13)
> (mapcar #'+
          '(1 2 3)
          '(10 100 1000))
(11 102 1003)
```

``` lisp
> (sort '(1 4 2 5 6 7 3) #'<)
(1 2 3 4 5 6 7)
> (remove-if #'evenp '(1 2 3 4 5 6 7))
(1 3 5 7)
(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (our-remove-if fn (cdr lst))
          (cons (car lst) (our-remove-if fn (cdr lst))))))


```
## 2.4 Functions as Properties

``` lisp
(defun behave (animal)
  (case animal
    (dog (wag-tail)
      (bark))
    (rat (scurry)
      (squeak))
    (cat (rub-legs)
      (scratch-carpet))))
(defun behave (animal)
(funcall (get animal 'behavior)))


(setf (get ’dog ’behavior)
      #’(lambda ()
          (wag-tail)
          (bark)))
```

## 2.5 Scope

``` lisp
(let ((y 7))
  (defun scope-test (x)
    (list x y)))

> (let ((y 5))
    (scope-test 3))
(3 5)

> (let ((y 5))
    (scope-test 3))
(3 7)
```

## 2.6 Closures

``` lisp
(defun list+ (lst n)
  (mapcar #’(lambda (x) (+ x n))
          lst))

> (list+ ’(1 2 3) 10)
(11 12 13)

(let ((counter 0))
  (defun new-id () (incf counter))
  (defun reset-id () (setq counter 0)))

(defun make-adder (n)
  #’(lambda (x) (+ x n)))


> (setq add2 (make-adder 2)
        add10 (make-adder 10))
#<Interpreted-Function BF162E>
> (funcall add2 5)
7
> (funcall add10 3)
13


(defun make-adderb (n)
  #’(lambda (x &optional change)
       (if change
           (setq n x)
           (+ x n))))

> (setq addx (make-adderb 1))
#<Interpreted-Function BF1C66>
> (funcall addx 3)
4

> (funcall addx 100 t)
100
> (funcall addx 3)
103


> (setq cities (make-dbms ’((boston . us) (paris . france))))
(#<Interpreted-Function 8022E7>
#<Interpreted-Function 802317>
#<Interpreted-Function 802347>)
```

``` lisp
Figure 2.1: Three closures share a list.

(defun make-dbms (db)
  (list
    #’(lambda (key)
        (cdr (assoc key db)))
    #’(lambda (key val)
        (push (cons key val) db)
        key)
    #’(lambda (key)
        (setf db (delete key db :key #’car))
        key)))
```


``` lisp
> (funcall (car cities) ’boston)
US
> (funcall (second cities) ’london ’england)
LONDON
> (funcall (car cities) ’london)
ENGLAND

(defun lookup (key db)
  (funcall (car db) key))

```


## 2.7 Local Functions
``` lisp
> (mapcar #’(lambda (x) (+ 2 x)) ’(2 5 7 3))
(4 7 9 5)
```

> (mapcar #’copy-tree ’((a b) (c d e)))
((A B) (C D E))

(defun list+ (lst n)
(mapcar #’(lambda (x) (+ x n))
lst))

> (labels ((inc (x) (1+ x)))
(inc 3))
4

(let ((x 10) (y x))
y)

(defun count-instances (obj lsts)
(labels ((instances-in (lst)
(if (consp lst)
(+ (if (eq (car lst) obj) 1 0)
(instances-in (cdr lst)))
0)))
(mapcar #’instances-in lsts)))

> (count-instances ’a ’((a b c) (d a r p a) (d a r) (a a)))
(1 2 1 2)


## 2.8 Tail-Recursion


``` lisp
(defun our-length (lst)
(if (null lst)
0
(1+ (our-length (cdr lst)))))
```

``` lisp
(defun our-find-if (fn lst)
(if (funcall fn (car lst))
(car lst)
(our-find-if fn (cdr lst))))


(defun our-length (lst)
(labels ((rec (lst acc)
(if (null lst)
acc
(rec (cdr lst) (1+ acc)))))
(rec lst 0)))

(proclaim ’(optimize speed))

(defun triangle (n)
  (labels ((tri (c n)
  (declare (type fixnum n c))
  (if (zerop n)
  c
  (tri (the fixnum (+ n c))
  (the fixnum (- n 1))))))
  (tri 0 n)))
```

## 2.9 Compilation

``` lisp
> (defun foo (x) (1+ x))
FOO

> (compiled-function-p #’foo)
NIL

> (compile ’foo)
FOO

> (compiled-function-p #’foo)
T
> (compile nil ’(lambda (x) (+ x 2)))
#<Compiled-Function BF55BE>


> (progn (compile ’bar ’(lambda (x) (* x 3)))
(compiled-function-p #’bar))
T


> (let ((y 2))
(defun foo (x) (+ x y)))

> (compile ’make-adder)
MAKE-ADDER
> (compiled-function-p (make-adder 2))
T

(defun 50th (lst) (nth 49 lst))

(proclaim ’(inline 50th))


(defun foo (lst)
(+ (50th lst) 1))

(defun foo (lst)
(+ (nth 49 lst) 1))


```
## 2.10 Functions from Lists


## 짚고 넘어가기

- defun
- eq
- lambda
- symbol-value
- symbol-function
- lisp1 / lisp2
- setq
- mapcar
- funcall
- labels
- let
- proclaim
- optimize
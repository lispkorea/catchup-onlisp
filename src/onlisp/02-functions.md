# 02 함수


## 2.1 데이터로서 함수

- 리스프 자체가 함수 집합 
  - 새로운 오퍼레이터를 추가할 수 있음.

- 리스프 함수는
  - 런타임에 생성하고 반환 가능
  - 인수로 전달가능
  - 변수가 값으로 함수를 가질 수 있다.
  - 함수단위나 파일 단위로 컴파일할 수 있다.


## 2.2 함수 정의

- 함수 정의 방법
  - defun
  - lambda

`defun`으로 함수 정의하기

``` lisp
(defun my-double (x)
  (* x 2))

#'my-double
;;=> #<Interpreted-Function C66ACE>

(my-double 3)
;;=> 6
```

`lambda`로 함수 정의하기

``` lisp
#'(lambda (x) (* x 2))
;;=> #<Interpreted-Function C674CE>

((lambda (x) (* x 2)) 3)
;;=> 6
```

함수/변수는 다른 이름공간을 가지고 있다.

|        | 이름 공간 분리 | ex          |
| ------ | -------------- | ----------- |
| Lisp-1 | 분리를 안함.   | Clojure     |
| Lisp-2 | 분리를 함.     | Common Lisp |

``` lisp
(setq my-double 2)
;;=> 2

(my-double my-double)
;;=> 4

(symbol-value 'my-double)
;;=> 2

(symbol-function 'my-double)
;;=> #<Interpreted-Function C66ACE>
```

``` lisp
(setq x #'append)
;;=> #<Compiled-Function 46B4BE>

(eq (symbol-value 'x)
    (symbol-function 'append))
;;=> T

(eq #'my-double (car (list #'my-double)))
;;=> T
```

`defun`은 심볼을 함수 이름 공간에 추가한다.

``` lisp
(defun my-double (x)
  (* x 2))

(setf (symbol-function 'my-double)
      #'(lambda (x) (* x 2)))
```


## 2.3 인자로서의 함수

`apply` 사용법

``` lisp
(+ 1 2)
;;=> 3

(apply #'+ '(1 2))
;;=> 3

(apply (symbol-function '+) '(1 2))
;;=> 3

(apply #'(lambda (x y) (+ x y)) '(1 2))
;;=> 3
```

`funcall` 사용법

``` lisp
(apply #'+ 1 '(2))
;;=> 3

(funcall #'+ 1 2)
;;=> 3
```

`mapcar` 사용법

``` lisp
(mapcar #'(lambda (x) (+ x 10))
          '(1 2 3))
;;=> (11 12 13)

(mapcar #'+
        '(1 2 3)
        '(10 100 1000))
;;=> (11 102 1003)
```

`sort` 사용법

``` lisp
(sort '(1 4 2 5 6 7 3) #'<)
;;=> (1 2 3 4 5 6 7)
```

`remove-if` 사용법

``` lisp
(remove-if #'evenp '(1 2 3 4 5 6 7))
;;=> (1 3 5 7)

(defun our-remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (our-remove-if fn (cdr lst))
          (cons (car lst) (our-remove-if fn (cdr lst))))))
```


## 2.4 속성으로서의 함수


``` lisp
(defun behave (animal)
  (case animal
    (dog
     (wag-tail)
     (bark))
    (rat
     (scurry)
     (squeak))
    (cat
     (rub-legs)
     (scratch-carpet))
    (human
     (speak))))

(defun wag-tail () (print "wag-tail"))
(defun bark () (print "bark"))
(defun scurry () (print "scurry"))
(defun squeak () (print "squeak"))
(defun rub-legs () (print "rub-legs"))
(defun scratch-carpet () (print "scratch-carpet"))
(defun speak () (print "speak"))

(behave 'dog)
;;>> "wag-tail"
;;>> "bark"

(behave 'rat)
;;>> "scurry"
;;>> "squeak"

(behave 'cat)
;;>> "rub-legs"
;;>> "scratch-carpet"

(behave 'human)
;;>> speak
```

``` lisp
(defun behave2 (animal)
  (funcall (get animal 'behavior)))

(setf (get 'dog 'behavior)
      #'(lambda ()
          (wag-tail)
          (bark)))

(behave2 'dog)
;;>> "wag-tail"
;;>> "bark"

(setf (get 'all 'behavior)
      #'(lambda ()
          (bark)
          (scurry)
          (scratch-carpet)))

(behave2 'all)
;;>> "bark"
;;>> "scurry"
;;>> "scratch-carpet"

```


## 2.5 범위(Scope)

| scope-test함수에서 |     |                              |
| ------------------ | --- | ---------------------------- |
| binding            | x   | 매개변수 x와 바인딩되어있다. |
| free variable      | y   | 스코프 환경에 따라 다르게됨  |

| 스코프            |                                            | y 의 값 |
| ----------------- | ------------------------------------------ | ------- |
| 다이나믹(dynamic) | 함수의 호출 체인을 거슬러 올라감           | 5       |
| 렉시컬(lexical)   | 함수가 정의 된 시점의 환경을 거슬러 올라감 | 7       |

``` lisp
(let ((y 7))
  (defun scope-test (x)
    (list x y)))

;; 다이나믹 스코프
;; - y의 값: 함수 호출을 감싼 let으로 정의한 5.
(let ((y 5))
  (scope-test 3))
;;=> (3 5)

;; 렉시컬 스코프 - Common Lisp 기본 설정
;; - y의 값: 앞서 defun시 감싼 let으로 정의한 7.
(let ((y 5))
  (scope-test 3))
;;=> (3 7)
```


## 2.6 클로져(Closures)

함수에서 binding되지 않는 변수, 즉 free 변수가 있을 때, 그 변수를 포함하는 함수를 클로져라고 한다.

``` lisp
(defun list+ (lst n)
  (mapcar #’(lambda (x) (+ x n))
          lst))

(list+ ’(1 2 3) 10)
;;=> (11 12 13)
```

``` lisp
(let ((counter 0))
  (defun new-id ()
    (incf counter))
  (defun reset-id ()
    (setq counter 0)))

(defun make-adder (n)
  #’(lambda (x) (+ x n)))

(setq add2 (make-adder 2)
      add10 (make-adder 10))

(funcall add2 5)
;;=> 7
(funcall add10 3)
;;=> 13
```

``` lisp
(defun make-adderb (n)
  #’(lambda (x &optional change)
       (if change
           (setq n x)
           (+ x n))))

(setq addx (make-adderb 1))

(funcall addx 3)
;;=> 4

(funcall addx 100 t)
;;=> 100

(funcall addx 3)
;;=> 103
```

``` lisp
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

(setq cities (make-dbms ’((boston . us) (paris . france))))

(funcall (car cities) ’boston)
;;=> US

(funcall (second cities) ’london ’england)
;;=> LONDON

(funcall (car cities) ’london)
;;=> ENGLAND

(defun lookup (key db)
  (funcall (car db) key))
```


## 2.7 지역 함수

`labels` 사용법
``` lisp
(labels ((inc (x) (1+ x)))
  (inc 3))
;;=> 4
```


## 2.8 꼬리 재귀(Tail-Recursion)

`our-length`함수의 끝이 `our-length`으로 끝나는게 아니라 `1+`가 감싸는 형태로 되어있다. 이와 같은 형태는 꼬리 재귀 형태가 될 수 없다.
``` lisp
(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))
```


`our-find-if`는 `our-find-if`로 끝나는 꼬리 재귀 형태이다.

``` lisp
(defun our-find-if (fn lst)
  (if (funcall fn (car lst))
      (car lst)
      (our-find-if fn (cdr lst))))
```

`our-length`는 `labels`를 이용하여 내부에 꼬리 재귀 형태의 `rec` 함수를 정의하였다.

``` lisp
(defun our-length (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))
```

``` lisp
;; Common Lisp에서 꼬리 재귀 최적화를 디폴트가 아닌 경우, 다음과 같이 추가 선언이 필요하다.
(proclaim '(optimize speed))
```

``` lisp
;; 속도 향상을 위한 꼬리 재귀 + 타입 선언 예제.
;; 1 ~ n 까지의 합을 구하는 함수.

(defun triangle (n)
  (labels ((tri (c n)
             (declare (type fixnum n c))
             (if (zerop n)
                 c
                 (tri (the fixnum (+ n c))
                      (the fixnum (- n 1))))))
    (tri 0 n)))
```


## 2.9 컴파일

- `compile`함수 사용하여, 함수를 컴파일
- `compile-file`함수 사용하여, 파일을 컴파일

``` lisp
(defun foo (x)
  (1+ x))
;;=> FOO


;; 컴파일 됐는지 여부 확인
(compiled-function-p #’foo)
;;=> NIL

(compile ’foo)
;;=> FOO

(compiled-function-p #’foo)
;;=> T

;; 익명함수 컴파일
(compile nil '(lambda (x) (+ x 2)))
;;=> #<Compiled-Function BF55BE>

;; 익명함수에 이름을 붙여 컴파일
(progn
  (compile 'bar '(lambda (x) (* x 3)))
  (compiled-function-p #’bar))
;;=> T


;; 렉시컬 환경에서는 컴파일이 안된다.
(let ((y 2))
  (defun foo (x)
    (+ x y)))

;; 함수를 리턴하는 함수를 컴파일하면, 리턴되는 함수도 컴파일이 되는걸 확인 할 수 있다.
(compile 'make-adder)
;;=> MAKE-ADDER
(compiled-function-p (make-adder 2))
;;=> T

;; 인라인 함수의 예제
(proclaim '(inline 50th))
(defun 50th (lst)
  (nth 49 lst))

(defun foo (lst)
  (+ (50th lst) 1) ;; => (+ (nth 49 lst) 1)
  )
```


## 2.10 Functions from Lists

패스


## 짚고 넘어가기

- defun
- eq
- lambda
- symbol-value
- symbol-function
- lisp1 / lisp2
- [setq](https://www.lispworks.com/documentation/lw50/CLHS/Body/s_setq.htm)
- mapcar
- funcall
- labels
- let
- case
- declare
  - type
- proclaim
  - inline
- optimize
- compiled-function-p
- compile
- copmile-file
- progn
# 07. 매크로

`매크로`는 코드를 만드는 함수

## 7.1. 매크로가 동작하는 방식

1. 정의한 표현식을 빌드한 다음
2. 매크로 호출시 대신 해당 표현식을 평가

``` lisp
(defmacro nil! (var) 
  (list 'setq var nil))

(defvar x 10)

;; 새로운 표현식을 빌딩하는 단계를 `매크로 확장(macroexpansion)` 이라고 한다.
;; 아래 매크로 호출은, 매크로 확장을 거쳐, (setq x nil)이 된다.
(nil! x)

x
;;=> NIL
```


## 7.2. 역 따옴표(Backquote)

- 따옴표 `'` 방향을 역으로 한 역 따옴표 `` ` `` 
- 백쿼트(backquote) 혹은 백틱(backtick)라고도 하는데 리스프에서는 백쿼트라는 말을 쓴다.

``` lisp
(defvar a 1)
(defvar b '(2 2))
(defvar c 3)
(defvar d 4)

;; `(a b c) == '(a b c) == (list 'a 'b 'c)
;; `(a ,b c) == '(a (2 2) c)
;; `(a ,@b c) == (list 'a 2 2 'c)

`(a ,b c)
;;=> (A (2 2) C)

`(a ,@b c)
;;=> (A 2 2 C)
```

``` lisp
;; list를 쓴 버전
(defmacro nil! (var) 
  (list 'setq var nil))

;; 백쿼트(`)를 쓴 버전
(defmacro nil! (var) 
  `(setq ,var nil))
```

``` lisp
;; list를 쓴 버전
(defmacro nif (expr pos zero neg) 
  (list 'case 
    (list 'truncate (list 'signum expr))
    (list 1 pos)
    (list 0 zero)
    (list -1 neg)))

;; 백쿼트(`)를 쓴 버전
(defmacro nif (expr pos zero neg) 
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))


(mapcar #'(lambda (x) (nif x 'pos 'zero 'neg))
         '(0 2.5 -8))
;;=> (ZERO POS NEG)
```


## 7.3. 간단한 매크로 정의

``` lisp
(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(memq 'a '(1 2 3))  ; 확장 (member 'a '(1 2 3) :test #'eq)
;;=> NIL

(memq 'a '(a b c))  ; 확장 (member 'a '(a b c) :test #'eq)
;;=> (A B C)
```


## 7.4. 매크로 확장(Macroexpansion) 확인

``` lisp
(defmacro while (test &body body) 
  `(do ()
       ((not ,test))
     ,@body))

(let ((i 0))
    (while (< i 3)
      (print i)
      (incf i)))
;;>> 1
;;>> 2
;;>> 3
;;=> NIL
```

`macroexpand`, `macroexpand-1` 함수를 사용하면 매크로 확장 결과를 확인할 수 있다.

``` lisp
(pprint (macroexpand `(while (able) (laugh))))
;;>> (BLOCK NIL
;;>>   (LET ()
;;>>     (DECLARE (IGNORABLE))
;;>>     (TAGBODY
;;>>       (GO #:G486)
;;>>      #:G485
;;>>       (TAGBODY (LAUGH))
;;>>       (PSETQ)
;;>>      #:G486
;;>>       (UNLESS (NOT (ABLE)) (GO #:G485))
;;>>       (RETURN-FROM NIL (PROGN)))))

(pprint (macroexpand-1 `(while (able) (laugh))))
;;>> (DO () ((NOT (ABLE))) (LAUGH))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(mac (while (able) (laugh)))
;;>> (DO () ((NOT (ABLE))) (LAUGH))
```


## 7.5. 인자 구조화된 할당

`destructuring-bind`

``` lisp
(destructuring-bind (x (y) . z) '(a (b) c d)
  (list x y z))
;;=> (A B (C D))
```

``` lisp
(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))

(our-dolist (x '(a b c))
  (print x))
;;>> A
;;>> B
;;>> C
;;=> NIL
```


``` lisp
(defmacro when-let ((var expr) &body body) 
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(when-let (a 1)
  (1+ a))
;;=> 2

(when-let (a nil)
  (1+ a))
;;=> NIL
```


``` lisp
;; Figure 7.6: A sketch of defmacro.

(defmacro our-expander (name)
  `(get ,name 'expander))

(defmacro our-defmacro (name parms &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name)
             #'(lambda (,g)
                 (block ,name
                   (destructuring-bind ,parms (cdr ,g)
                     ,@body))))
       ',name)))

(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
      expr))

(our-defmacro hello (a)
  `(1+ ,a))

(our-macroexpand-1 '(hello 10))
;;=> (1+ 10)
```


## 7.6. 매크로 모델

패스


## 7.7. 프로그램으로서의 매크로

`setq`와 `psetq`(`p`arallel `setq`)의 차이


``` lisp
 (let ((a 1))
   (setq a 2 b a)
   (list a b))
;;=> (2 2)

(let ((a 1))
  (psetq a 2 b a)
  (list a b))
;;=> (2 1)
```

``` lisp
;; Figure 7.8: Implementing do.

(defmacro our-do (bindforms (test &rest result) &body body)
  (let ((label (gensym)))
    `(prog ,(make-initforms bindforms)
        ,label
        (if ,test
            (return (progn ,@result)))
        ,@body
        (psetq ,@(make-stepforms bindforms))
        (go ,label))))

(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
              (if (consp b)
                  (list (car b) (cadr b))
                  (list b nil)))
          bindforms))

(defun make-stepforms (bindforms)
  (mapcan #'(lambda (b)
              (if (and (consp b) (third b))
                  (list (car b) (third b))
                  nil))
          bindforms))

(do ((w 3)
     (x 1 (1+ x))
     (y 2 (1+ y))
     (z))
    ((> x 10) (princ z) y)
  (princ x)
  (princ y))
;;>> 12233445566778899101011NIL
;;=> 12

(our-do ((w 3)
         (x 1 (1+ x))
         (y 2 (1+ y))
         (z))
    ((> x 10) (princ z) y)
  (princ x)
  (princ y))
;;>> 12233445566778899101011NIL
;;=> 12
```


## 7.8. 매크로 스타일


``` lisp
;; Figure 7.9: Two macros equivalent to and.

(defmacro our-and-1 (&rest args)
  (case (length args)
    (0 t)
    (1 (car args))
    (t `(if ,(car args)
            (our-and-1 ,@(cdr args))))))

(defmacro our-and-2 (&rest args)
  (if (null args)
      t
      (labels ((expander (rest)
                 (if (cdr rest)
                     `(if ,(car rest)
                          ,(expander (cdr rest)))
                     (car rest))))
        (expander args))))
```

매크로는 확장되기 전에 실재 구현이 숨겨져 있으므로, 성능에 영향을 주는 코드의 영향력을 파악하기 어렵다.


## 7.9. 매크로 의존성

```  lisp
(defmacro mac (x)
 `(1+ ,x))

(setq fn (compile nil '(lambda (y) (mac y))))

(defmacro mac (x)
  `(+ ,x 100))

(funcall fn 1)
;;=> 2
```

1. 어떠한 `A` 매크로를 정의한 후, `A`매크로를 호출하는 함수(또는 매크로)를 정의한다.
2. 어떠한 `A` 매크로를 수정했다면, 직접 또는 다른 매크로를 통해 이를 호출하는 모든 함수(또는 매크로)도 다시 컴파일한다.


## 7.10. 함수에서 매크로로

``` lisp
(defun sum (&rest args)
  (apply #'+ args))

(sum 1 2 3)
;;=> 6


(defmacro sum-1 (&rest args)
  `(apply #'+ (list ,@args)))

(macroexpand-1 '(sum-1 1 2 3))
;;=> (APPLY #'+ (LIST 1 2 3))
;;=> T


(defmacro sum-2 (&rest args)
  `(+ ,@args))

(macroexpand-1 '(sum-2 1 2 3))
;;=> (+ 1 2 3)
;;=> T
```

``` lisp
(defun foo (x y z)
  (list x (let ((x y))
            (list x z))))

(foo 1 2 3)
;;=> (1 (2 3))


(defmacro foo-macro (x y z)
  `(list ,x (let ((x ,y))
              (list x ,z))))

(foo-macro 1 2 3)
;;=> (1 (2 3))
```


## 7.11. 심볼 매크로


- CLTL2는 커먼 리스프에 새로운 종류의 매크로인 심볼 매크로(symbol-macro)를 도입했다.

| 종류        | 정의            | 호출   출     |
| ----------- | --------------- | ------------- |
| 일반 매크로 | defmacro        | 함수처럼 호출 |
| 심볼 매크로 | symbol-macrolet | 심볼처럼 호출 |

``` lisp
(symbol-macrolet ((hi (progn (print "Howdy")
                             1)))
  (+ hi 2))
;;>> "Howdy"
;;=> 3
```

18장에서 심볼 매크로를 사용하는 방법을 설명한다.


## 짚고 넘어가기

- defmacro
  - `'`
  - `` ` ``
  - `,`
  - `,@`
- macroexpand
- macroexpand-1
- symbol-macrolet
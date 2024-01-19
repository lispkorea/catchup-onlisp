# 03. 함수형 프로그래밍

## 3.1. 함수형 디자인

- 함수형 프로그래밍은 사이드 이펙트 없이, 값을 반환하는 함수를 조합하여 프로그램을 작성하는 것.
- 사이드 이펙트
  - 사이드 이펙트라 함은 객체의 변경 (ex. `rplaca` ) 및 변수의 할당의 사용(ex. `setq`) 등이 있다.
  - 사이드 이펙트를 지닌 함수의 갯수가 적고 있더라도 그 영향의 범위가 좁아질 수록, 프로그램의 읽기, 테스트, 디버깅은 간단해진다.

| 사이드이펙트를 불러일으키는 함수 |
| -------------------------------- |
| set                              |
| setq                             |
| setf                             |
| psetf                            |
| psetq                            |
| incf                             |
| decf                             |
| push                             |
| pop                              |
| pushnew                          |
| rplaca                           |
| rplacd                           |
| rotatef                          |
| shiftf                           |
| remf                             |
| remprop                          |
| remhash                          |
| let*                             |


``` lisp
;;; Figure 3.1: A function to reverse lists.
;;; O(n^2)
(defun bad-reverse (lst)
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))
         (j (1- len) (1- j)))
        ((>= i ilimit))
      (rotatef (nth i lst) (nth j lst)))))

(setq lst '(abc))
;;=> (ABC)

(bad-reverse lst)
;;=> NIL

lst
;;=> (CBA)
```


``` lisp
;;; Figure 3.2: A function to return reversed lists.
;;; O(n)
(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

> (setq lst '(abc))
(ABC)
> (good-reverse lst)
(CBA)
> lst
(ABC)
```

`nreverse`와 같이 사이드 이펙트가 필요한 경우 반환 값을 `setq` 이용해 대입한다.

``` lisp
> (setq lst '(abc))
(ABC)

> (nreverse lst)
(CBA)

> lst
(A)
```

- 다른 프로그래밍 언어에서 부작용을 사용하는 가장 큰 이유는 다중 값을 반환하는 함수가 필요하다는 것.
  - 언어에서 하나의 값만 반환 할 수 있으면, 다중 값을 반환하기 위해 매개 변수를 활용하여 반환함.
    - ex) c#에서 out 파라미터
  - 다행히 Common Lisp에서는 `values`를 이용, 다중 값을 반환할 수 있음.

``` lisp
(defun powers (x)
  (values x (sqrt x) (expt x 2)))
;;=> POWERS

(multiple-value-bind (base root square) (powers 4)
  (list base root square))
;;=> (4 2.0 16)

(* (powers 4) 2)
;;=> 8
```

``` lisp
(truncate 26.21875)
;;=> 26
;;=> 0.21875

(= (truncate 26.21875) 26)
;;=> T
```

## 3.2. 명령형을 뒤집어보자

`imperative-style`을 임시 변수를 없에면서 역순으로 뒤집어 구현해보면 `funcional-style`이 된다.

``` lisp
(defun imperative-style (x)
  (let (y
        sqr)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))

(defun funtional-style (x)
  (list 'a (expt (car x) 2)))
```

## 3.3. 함수형 인터페이스

함수는 따옴표 붙은 리스트를 반환해서는 안된다.

``` lisp
(defun bad-exclaim (expression)
  (append expression '(oh my)))

(bad-exclaim '(lions and tigers and bears))
;;=> (LIONS AND TIGERS AND BEARS OH MY)

(nconc * '(goodness))
;;=> (LIONS AND TIGERS AND BEARS OH MY GOODNESS)

(bad-exclaim '(fixnums and bignums and floats))
;;=> (FIXNUMS AND BIGNUMS AND FLOATS OH MY GOODNESS)
```

``` lisp
(defun good-exclaim (expression)
  (append expression (list 'oh 'my)))

(good-exclaim '(lions and tigers and bears))
;;=> (LIONS AND TIGERS AND BEARS OH MY)

(nconc * '(goodness))
;;=> (LIONS AND TIGERS AND BEARS OH MY GOODNESS)

(good-exclaim '(fixnums and bignums and floats))
;;=> (FIXNUMS AND BIGNUMS AND FLOATS OH MY)
```

## 3.4. 인터렉티브 프로그래밍

- 숙련 된 Lisp 프로그래머는 테스트하기 쉽도록 프로그램을 디자인한다.
  - 사이드 이펙트를 사용하는 부분을 몇 가지 함수로 분리하고 프로그램의 대부분은 순수한 함수형 프로그래밍 스타일로 쓴다.
  - 사이드 이펙트를 사용하는 것을 피할 수 없다면, 적어도 거기에 함수형 인터페이스를 포함하려고 한다.
  - 하나의 함수에는 하나의 목적만.

- 소프트웨어 개발은 ​​코드 작성과 테스트 사이클로 구성된다
  - 리스프에서는 그 사이클이 매우 짧다.

## 짚고 넘어가기

- rotatef
- nreverse
- values
- multiple-value-bind
- expt
- nconc
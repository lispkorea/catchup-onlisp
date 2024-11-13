# 05. 함수를 반환하기

새로운 함수를 생성하고 반환하는 함수를 정의함으로써, 함수를 인자로 받는 유틸리티의 효과를 증폭시킬 수 있다.


## 5.1. 진화하는 Common Lisp

- CLTL2에서는 `complement`라는 함수가 추가되었다.
  - 함수의 여함수(Complement Function)를 반환하는 함수.
  - 초기 Common Lisp에는 `remove-if`와 `remove-if-not` 같이 쌍을 이루는 함수들이 있었다.
  - CLTL2에 와서는 결과적으로 `-if-not`함수들이 사라지게 되었다.

``` lisp
(defun sample-complement (fn)
  #'(lambda (&rest args)
      (not (apply fn args))))

(remove-if (sample-complement #'oddp) '(1 2 3 4 5 6))
;;=> (1 3 5)
```


## 5.2. 직교성(Orthogonality)

- 직교성(orthogonal)을 지닌 프로그래밍 언어는 적은 수의 오퍼레이터를 다양한 방법으로 결합시킴으로써 여러가지 의미를 표현할 수 있다.

- `setf` 매크로는 리스프의 직교성을 향상시킨다.
  - `get-color`, `set-color`와 같은 함수들이 필요한게 아닌, `get`과 `setf`를 조합하여 동일한 효과를 얻는다.

``` lisp
(setf (get 'ball 'color) 'red)

(symbol-plist 'ball)
;;=> (COLOR RED)

(get 'ball 'color)
;;=> RED
```

새로운 표현만들기 `def!`와 `!`:

``` lisp
(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

(def! #'remove-if #'delete-if)

(delete-if #'oddp '(1 2 3 4))
;;=> (2 4)

(funcall (! #'remove-if) #'oddp '(1 2 3 4))
;;=> (2 4)
```


## 5.3. 메모이징(Memoizing)

캐쉬(cache)를 만들어서, 함수의 결과를 저장해두고, 같은 인자로 호출될 때는 캐쉬에 저장된 값을 반환하는 방식.

``` lisp
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))


(setq slowid (memoize #'(lambda (x) (sleep 5) x)))


(time (funcall slowid 1))
;;>> Elapsed Time = 5.15 seconds
;;=> 1

(time (funcall slowid 1))
;;>> Elapsed Time = 0.00 seconds
;;=> 1
```

## 5.4. 합성 함수(Composing Functions)

- 함수 `f`의 여함수(Complement Function)는 `~f`로 표시.

``` lisp
(funcall (complement #'evenp) 1)
;;=> T
```

- 함수 `f`와 `g`가 있을때 합성함수(Composing Function) `f(g(x))`는 `f ○ g`로 표시.

``` lisp
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))
;;=> 4
```

- `fif`
  - `f`unction `if`
  - `if` 만족시 `then` 아니면 `else` 적용

``` lisp
(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))

(mapcar (fif #'oddp #'1+ #'1-)
        '(1 2 3 4 5))
;;=> (2 1 4 3 6)
```

- `fint`
  - `f`unction `int`ersection
  - 함수들을 모두 만족시

``` lisp
(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x) 
            (and (funcall fn x) (funcall chain x))))))

(mapcar (fint #'oddp #'(lambda (x) (> x 3)))
        '(1 2 3 4 5))
;; (NIL NIL NIL NIL T)
```

- `fun`
  - `f`unction `un`ion
  - 함수들 중 하나라도 만족시

``` lisp
(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))

(mapcar (fun #'oddp #'(lambda (x) (> x 3)))
        '(1 2 3 4 5))
;; (T NIL T T T)
```

## 5.5. cdr을 이용한 재귀

- `cdr`은 리스트의 두번째 요소부터 끝까지를 반환하는 함수.
  - `car`와 `cdr`을 이용하여 전체 리스트를 순회할 수 있다.

``` lisp
(cdr '(1 2 3 4))
;;=> (2 3 4)
```

``` lisp
(defun our-length (lst)
  (if (null lst)
      0
      (1+ (our-length (cdr lst)))))

(our-length '(1 2 3))
;;=> 3
```

``` lisp
(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (our-every fn (cdr lst)))))

(our-every #'evenp '(2 4 6))
;;=> T
```

- lrec
  - `l`ist `rec`urser
  - 리스트에 대한 재귀를 추상화한 함수
  - 꼬리 재귀(tail recursion)를 사용하여 최적화한 버전이 아니지만, 간단히 구현해 본 것.

``` lisp
(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                          #'(lambda ()
                              (self (cdr lst)))))))
    #'self))


;; 리스트 복사
(lrec #'(lambda (x f) (cons x (funcall f))))

;; 중복 삭제
(lrec #'(lambda (x f) (adjoin x (funcall f))))

;; find-if, fn을 만족시키는 x 찾기
(lrec #'(lambda (x f) (if (fn x) x (funcall f))))


; some, fn을 적용시켜 거짓이 아니면 반환
(lrec #'(lambda (x f) (or (fn x) (funcall f))))
```


## 5.6. 서브트리(subtree)와 재귀


| 표현식      | cons cell                           |
| ----------- | ----------------------------------- |
| (a . b)     | (a . b)                             |
| (a b c)     | (a . (b . (c . nil)))               |
| (a b (c d)) | (a . (b . ((c . (d . nil)) . nil))) |

![](../res/fig5_7.svg)

`our-copy-tree` 예제:

``` lisp
(defun our-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (our-copy-tree (car tree))
            (if (cdr tree) (our-copy-tree (cdr tree))))))

(our-copy-tree '((a b (c d)) (e) f))
;;=> ((A B (C D)) (E) F)
```

`count-leaves` 예제:

``` lisp
(defun count-leaves (tree)
  (if (atom tree)
      1
      (+ (count-leaves (car tree))
         (or (if (cdr tree) (count-leaves (cdr tree)))
             1))))

(count-leaves '((a b (c d)) (e) f))
;;=> 10
```

`our-flatten` 예제:

``` lisp
(defun our-flatten (tree)
  (if (atom tree)
      (mklist tree)
      (nconc (flatten (car tree))
             (if (cdr tree) (our-flatten (cdr tree))))))

(our-flatten '((a b (c d)) (e) f))
;;=> (A B C D E F)
```

`rfind-if` 예제:

``` lisp
(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))

(rfind-if (fint #'numberp #'oddp) '(2 (3 4) 5))
;;=> 3
```


## 5.7. 어느 시점에 함수를 만들어야 하는가

- `#'(lambda ... )`는 상수 표현식이나, 함수 호출은 `런타임에 평가`된다.
- `#.( ... )`에서의 `#.`은 리드 매크로(read macro)로 뒷따르는 `표현식을 읽는 시점에 평가`된다.


## 짚고 넘어가기

- [get](https://www.lispworks.com/documentation/lw51/CLHS/Body/f_get.htm)
- [#.](https://www.lispworks.com/documentation/HyperSpec/Body/02_dhf.htm) 리드 매크로
- gethash
- make-hash-table
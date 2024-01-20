# 04. 유틸리티 함수

- 오퍼레이터 종류
  - 함수(function)
  - 매크로(macro)
  - 스페셜 폼(special form)
    - 단, 스페셜 폼은 유저가 만들 수 없다.

## 4.1. 유틸리티의 탄생

- 유틸리티: 프로그램을 쉽게 쓸 수 있게 해주는 연산자.
  - "유틸리티"라는 단어에 정확한 정의가 없음.
  - 어플리케이션이라고 하기엔 작고, 일부분이라고 하기에는 너무 범용적인 경우 "유틸리티"라 칭함.

``` lisp
(defun nicknames (name)
  (list 'nick (concatenate 'string "foo-" name)))

(nicknames "park")
;;=> (NICK "foo-park")

(setq names '("park" "jane" "june"))

;; all-nicknames 함수를 만들어도 되지만
(defun all-nicknames (names)
  (if (null names)
      nil
      (nconc (nicknames (car names))
             (all-nicknames (cdr names)))))
  
;; mapcan을 알고 있다면, all-nicknames 함수를 만들 필요가 없다.
(mapcan #'nicknames names)
;;=> (NICK "foo-park" NICK "foo-jane" NICK "foo-june")

(mapcan #'reverse '((2 1 0) (5 4 3)))
;; (0 1 2 3 4 5)
```


``` lisp
(defun find-books (towns)
  (let ((town (find-if #'bookshops towns))) ;; bookshops 함수 호출
    (values town (bookshops town))))        ;; bookshops 함수 호출

(find-books1 towns)
```

``` lisp
(defun find-books2 (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns)))) ;; bookshops 함수 호출
        (if shops
            (values (car towns) shops)
            (find-books2 (cdr towns))))))

(find-books2 towns)
```

``` lisp
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst)))) ;; fn 함수 호출
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

(find2 #'bookshops towns)
```

## 4.2. 추상화에 투자하라

프로그램을 작성하고 유지하는 데 드는 비용은 프로그램이 길어짐에 따라 증가한다.

- 유틸리티
  - 유틸리티는 당면한 문제뿐만 아니라 일반적인 상황에 대해 작성해야 한다.
  - 서둘러 작성해서는 안 된다.
  - 나중에 필요할지 확실하지 않을 때는, 일단 작성해 본다.
  - 단, 아직은 유틸리티가 아닌 서브 루틴 신분이다.(해당 프로그램에서만 사용)
  - 다른 프로그램에서 그 서브 루틴을 사용할 일이 생기면, 유틸리티로 승격시켜 널리 사용할 수 있도록 한다.

## 4.3. 리스트에 대한 연산

리스프(Lisp)의 이름은 `LIS`t `P`rocessing에서 따왔다.

`longer`함수의 예:

``` lisp
(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(longer '(1 2 3) '(4 5))
;;=> T

(longer '(1 2) '(3 4 5))
;;=> NIL
```

`filter`함수의 예:

``` lisp
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val
            (push val acc))))
    (nreverse acc)))

(filter #'evenp '(1 2 3 4 5))
;;=> (T T)

(filter #'(lambda (x) (if (numberp x) (1+ x)))
        '(a 1 2 b 3 c d 4))
;;=> (2 3 4 5)
```

`group`함수의 예:

``` lisp
(defun group (source n)
  (if (zerop n)
      (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source
        (rec source nil)
        nil)))

(group '(a b c d e f g) 2)
;;=> ((A B) (C D) (E F) (G))
```

`flatten`함수의 예:

``` lisp
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(flatten '(a (b c) ((d e) f)))
;;=> (A B C D E F)
```

`prune`함수의 예:

``` lisp
(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree)
                    (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t
                    (rec (cdr tree)
                         (if (funcall test (car tree))
                             acc
                             (cons (car tree) acc)))))))
    (rec tree nil)))

(prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))
;;=> (1 (3 (5)) 7 (9))
```

## 4.4. 검색

``` lisp

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first)
                nil)
               ((funcall test x first)
                lst)
               (t
                (before x y (cdr lst) :test test))))))

(before 'a 'b '(a))
;;=> (A)
```

``` lisp
(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(after 'a 'b '(b a d))
;;=> (A D)
(after 'a 'b '(a))
;;=> NIL
```

``` lisp
(defun duplicate (obj lst &key (test #'eql))
  (member obj
          (cdr (member obj lst :test test))
          :test test))

(duplicate 'a '(a b c a d))
;;=> (A D)
```

``` lisp
(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(split-if #'(lambda (x) (> x 3))
          '(1 2 3 4 5))
;;=> (1 2 3)
;;=> (4 5)
```


``` lisp
(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(best #'> '(1 2 3 4 5))
;;=> 5
```

``` lisp
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj)
              (setq max score))))
        (values wins max))))

(most #'length '((a b) (a b c) (a) (e f g)))
;;=> (A B C)
;;=> 3
```

``` lisp
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max score)
                   (setq result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))

(mostn #'length '((a b) (a b c) (a) (e f g)))
;;=> ((A B C) (E F G))
;;=> 3
```


## 4.5. 맵핑

``` lisp
(defun mapa-b (fn a b &optional (step 1))
  (map-> fn
         a
         #'(lambda (x) (> x b))
         #'(lambda (x) (+ x step))))


(defun map0-n (fn n)
  (mapa-b fn 0 n))

(map0-n #'1+ 5)
;;=> (1 2 3 4 5 6)


(defun map1-n (fn n)
  (mapa-b fn 1 n))

(mapa-b #'1+ -2 0 .5)
;;=> (-1 -0.5 0.0 0.5 1.0)


(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun recur-mapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'recur-mapcar fn args))
             args)))

(defun our-mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)))

(recur-mapcar #'princ '(1 2 (3 4 (5) 6) 7 (8 9)))
;;>> 123456789
;;=> (1 2 (3 4 (5) 6) 7 (8 9))

(recur-mapcar #'+ '(1 (2 (3) 4)) '(10 (20 (30) 40)))
;;=> (11 (22 (33) 44))
```

## 4.6. I/O

``` lisp
(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string "(" (apply #'read-line args) ")"))))
(readlist)
;;<< Call me "Ed"
;;=> (CALL ME "Ed")
```

``` lisp
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(prompt "Enter a number between ~A and ~A.~%>> " 1 10)
;;>> Enter a number between 1 and 10.
;;<< 3
;;=> 3
```

``` lisp
(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))

(break-loop #'eval #'(lambda (x) (eq x :q)) ">> ")
;;>> Entering break-loop.
;;>> >>
;;<< (+ 2 3)
;;>> 5
;;>> >>
;;<< :q
;;=> NIL
```

## 4.7. 심볼과 문자열

``` lisp
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

(mkstr pi " pieces of " 'pi)
;;=> "3.141592653589793d0 pieces of PI"
```

``` lisp
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(symb 'ar "Madi" #\L #\L 0)
;;=> |ARMadiLL0|
```

``` lisp
(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(reread 'a 'b "c")
;;=> ABC
```

``` lisp
(defun explode (sym)
  (map 'list #'(lambda (c)
                 (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

(explode 'bomb)
;;=> (B O M B)
```

## 4.8. 밀도

- 상향식 프로그램을 읽으려면 정의된 새로운 유틸리티를 모두 이해해야 한다.
  - 이해하는데 걸린 시간은, 유틸리티 없는 경우에 비해 적을것이다.
  - 유틸리티를 사용해서 코드가 읽는 게 어렵다고 말하는 사람들이 있다면, 그 사람들은 유틸리티를 사용하지 않으면 코드가 어떤 식이 될지 이해하지 못하는 사람들이다.

- 의도적으로 유틸리티를 피하는 경우가 하나 있다.
  - 나머지 코드와 독립적으로 배포할 작은 프로그램을 작성해야 하는 경우.
    - 소규모 프로그램에서는 유틸리티로 만들만큼 충분히 사용되지 않을 수 있다.

## 짚고 넘어가기

- find-if
- [mapcan](https://www.lispworks.com/documentation/HyperSpec/Body/f_mapc_.htm)
- push
- zerop
- nreverse
- subseq
- nthcdr
- dolist
- do
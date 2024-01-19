# 01. 확장 가능한 언어

- 리스프의 가장 특징적인 특성 중 하나는 작성 중인 프로그램에 맞게 조정할 수 있다는 점.
- 리스프 자체가 리스프 프로그램이며, 리스프 프로그램은 리스프 데이터 구조인 리스트으로 표현할 수 있다.

## 1.1. 진화에 의한 디자인

- 프로그램을 작성하면서 계획을 세우는 능력은 두 가지 중대한 결과를 가져온다:
  - 프로그램을 작성하는 데 걸리는 시간이 짧아진다. 
    - 계획을 세우면서 동시에 프로그램을 작성하면 주의 집중이 잘된다.
  - 그리고 그렇게 만들어진 프로그램은 더 좋은 프로그램이 된다.
    - 프로그램의 최종 디자인은 진화의 산물이기 때문이다.
    - 최종 목표를 찾는 동안 잘못된 부분을 발견하면 그 자리에서 반드시 다시 작성한다는 원칙을 지키는 한, 최종적으로 완성된 프로그램은 미리 몇 주 동안 계획을 세웠을 때보다 훨씬 더 우아한 프로그램이 될 것이다.

- 리스프의 가장 큰 위험은 리스프 자체가 사용자에게 악영향을 미칠 수 있다는 것
  - 리스프를 한동안 사용하게 되면 프로그래밍 언어와 애플리케이션의 궁합에 너무 민감해져서 원래 사용하던 프로그래밍 언어로 돌아가도, 리스프만큼의 필요한 유연성을 얻지 못한다라는 생각에 갇히게 될 수 있다.

## 1.2. 상향식(Bottom-Up) 프로그래밍

패스

## 1.3. 확장 가능한 소프트웨어

패스

## 1.4. 리스프의 확장

리스프는 함수나 매크로를 정의하는 것만으로도 언어를 확장할 수 있다.

함수를 이용하여 확장하는 예제: `map1-n`

``` lisp
(mapcar #'1+ '(1 2 3 4 5 6 7 8 9 10))
;;=> (2 3 4 5 6 7 8 9 10 11)
```

``` lisp
(map1-n #'1+ 10)
;;=> (2 3 4 5 6 7 8 9 10 11)

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))
```

매크로를 이용하여 확장하는 예제: `for`

``` lisp
(do ((x 1 (1+ x)))
    ((> x 5))
  (print x))
;; >> 1
;; >> 2
;; >> 3
;; >> 4
;; >> 5
;;=> NIL
```

``` lisp
(for (x 1 5)
  (print x))
;; >> 1
;; >> 2
;; >> 3
;; >> 4
;; >> 5
;;=> NIL

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))
```

## 1.5. 왜(혹은 언제) 리스프인가

패스

## 짚고 넘어가기

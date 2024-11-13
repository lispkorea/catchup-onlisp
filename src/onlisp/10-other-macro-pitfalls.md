# 10. 또 다른 매크로의 위험성

## 10.1. 평가 횟수


``` lisp
;;; Figure 10.1: Controlling argument evaluation.

;; 올바른 버전
(defmacro for-correct ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

;; 여러 번 평가:
(defmacro for-eval ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))             ; 올바른 버전에 비해, gensym을 사용하지 않았다.
       ((> ,var ,stop))
     ,@body))

;; 옳지 않은 평가 순서:
(defmacro for-order ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,gstop ,stop)
          (,var ,start (1+ ,var)))          ; 올바른 버전에 비해, 1+ 가 아래에 있다
         ((> ,var ,gstop))
       ,@body)))
```

## 10.2. 평가 순서


``` lisp
(setq x 10)


(+ (setq x 3) x)
;;=> 6
```

``` lisp
(let ((x 1))
  (for-order (i x (setq x 13))
    (princ i)))
;;>> 13
;;=> NIL
```

``` lisp
(let ((x 1))
  (for-correct (i x (setq x 13))
    (princ i)))
;;>> 12345678910111213
;;=> NIL
```


## 10.3. 함수형이 아닌 익스펜더


## 10.4. 재귀

## 짚고 넘어가기
#lang racket
(define (power-set lst)
  (if(empty? lst)
     (cons '() lst)
     (append (map (lambda (x) (cons (car lst) x)) (power-set (cdr lst))) (power-set (cdr lst)))))

(define (remove-p lst pred)
  (myremove-p lst pred '())
 )
(define (myremove-p lst pred newlst)
  (if (null? lst)
      newlst
      (if (pred (car lst))
          (myremove-p(cdr lst) pred newlst)
          (myremove-p(cdr lst) pred (append newlst (list(car lst)))) ; why problem?????
          )
      )
  )

(define (most-movies lst)
  (removedup (longlst lst) 0))

(define (least2greatest lst)
  (sort lst #:key car <))

(define (longlst lst)
  (car(map least2greatest (sort (remove-p (power-set lst) (lambda (x) (< (length x) 2))) #:key length >))))

(define (removedup lst len)
  (if (= (length lst) 1)
      (+ 1 len) 
      (if (= (car (car lst)) (car (car (cdr lst))))
          (removedup (least2greatest (cons (car lst) (cdr (cdr lst)))) len)
          (removedup (cdr lst) (+ len 1)))))
      

;(define (collectshortest


  

(define *testing-flag* #t)

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'most-movies (most-movies '((1 2) (2 3))) 2)
(test 'most-movies (most-movies '((1 2) (2 3) (3 4) (1 5))) 3)
(test 'most-movies (most-movies '((3 4) (1 2) (1 5) (4 5) (0 1))) 4)
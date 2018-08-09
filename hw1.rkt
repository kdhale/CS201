#lang racket

(provide hours
	 bin2dec
	 dec2bin
	 hex2dec
	 dec2hex
	 range
	 remove-p
	 collect-p
	 starts-with?
	 contains?
	 power-set
	 most-movies
	 )

; Please do not modify the lines above this one.

; ********************************************************
; CS 201 HW #1  DUE Wednesday, 9/20/2017 at 11:59 pm
;                via the submit system on the Zoo

; ********************************************************
; Name: Kenia Hale
; Email address: kenia.hale@yale.edu
; ********************************************************

; This file may be opened in DrRacket.
; Lines beginning with semicolons are comments.

; If you are asked to write a procedure, please make sure it has 
; the specified name, and the specified number and order of arguments.  
; The names of the formal arguments need not be the same
; as in the problem specification.

; For each problem, the intended inputs to your procedures
; are specified (for example, "positive integers") 
; and your procedures need not do anything reasonable 
; for other possible inputs.

; You may write auxiliary procedures in addition to
; the requested one(s) -- for each of your auxiliary 
; procedures, please include a comment explaining
; what it does, and giving an example or two.

; You may also use procedures you have written 
; elsewhere in this assignment or previous assignments.
; They only need to be defined once somewhere within this file.

; Please use the predicate equal? to test equality
; of values that may not be numbers.  To test equality
; of numbers, you can use =.

; Reading: Chapters 1 and 2 of the Racket Guide.

; ********************************************************
; ** problem 0 ** (1 easy point) 
; Replace the number 0 in the definition below to indicate
; the number of hours you spent doing this assignment
; Decimal numbers (eg, 6.237) are fine.  Exclude time
; spent reading.

(define hours 5)

; ********************************************************
; ** problem 1 ** (9 points)
; Write a procedure

; (bin2dec '(1 0 0))

; that takes a list of binary digits and returns the
; corresponding decimal number

; Examples
; (bin2dec '(1 0 1 0)) => 10
; (bin2dec '(1 1 1 1 1)) => 31
; (bin2dec '(1)) => 1
; (bin2dec '()) => 0
; (bin2dec '(1 0 0 0 0 0 0 0)) => 128
;
; ********************************************************

(define (bin2dec num)
  (mybin2dec num 0)
 )
(define (mybin2dec lst num) ; function that takes in list and returns length
  (if (null? lst)
      (+ 0 num) ; ends the recursion
      (if (= (car lst) 1) ; if number is 1, take 2 to the pwr of (length - 1)
          (mybin2dec (cdr lst) (+ (expt 2 (- (length lst) 1)) num))
          (mybin2dec (cdr lst) num)) ; else, do it w cadr
      )
 )
 

; ********************************************************
; ** problem 2 ** (10 points)
; Write a procedure

; (dec2bin n)

; that takes a positive integer as input, and returns 
; a list of binary digits
; The following identity should hold:

;; (bin2dec (dec2bin n)) == n

; Examples

; (dec2bin 23) => '(1 0 1 1 1)
; (dec2bin 2) => '(1 0)
; (dec2bin 128) => '(1 0 0 0 0 0 0 0)
; (dec2bin 0) => '(0)
; (dec2bin 127) => '(1 1 1 1 1 1 1)

; ********************************************************

(define (dec2bin n)
  (mydec2bin n '()))
(define (mydec2bin num lst) ; recusrive function, calls number and maintains a list
  (if (= num 1) ; if @ end
      (reverse(append lst '(1)))
      (mydec2bin (quotient num 2)(append lst (list (remainder num 2))))
      )
  )
  


; ********************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (hex2dec n)

; that takes a list of hexadecimal (base 16) digits and returns the
; corresponding decimal number

; Examples

; (hex2dec '(A)) => 10
; (hex2dec '(F F)) => 255
; (hex2dec '(1 0 0)) => 256
; (hex2dec '(d e a d b e e f)) => 3735928559

; ********************************************************

(define (hex2dec num)
  (myhex2dec num 0))
(define (myhex2dec lst num) ; function that takes in list and returns length
  (if (null? lst)
      (+ 0 num) ; ends the recursion
      (if (equal?(car lst) 0) ; if number is 0
          (myhex2dec (cdr lst) num) ;do it w cadr
          (cond
            [(or (equal? 'A (car lst)) (equal? 'a (car lst)))(myhex2dec (cdr lst) (+ (* 10 (expt 16 (- (length lst) 1)))num))]
            [(or (equal? 'B (car lst)) (equal? 'b (car lst)))(myhex2dec (cdr lst) (+ (* 11 (expt 16 (- (length lst) 1)))num))]
            [(or (equal? 'C (car lst)) (equal? 'c (car lst)))(myhex2dec (cdr lst) (+ (* 12 (expt 16 (- (length lst) 1)))num))]
            [(or (equal? 'D (car lst)) (equal? 'd (car lst)))(myhex2dec (cdr lst) (+ (* 13 (expt 16 (- (length lst) 1)))num))]
            [(or (equal? 'E (car lst)) (equal? 'e (car lst)))(myhex2dec (cdr lst) (+ (* 14 (expt 16 (- (length lst) 1)))num))]
            [(or (equal? 'F (car lst)) (equal? 'f (car lst)))(myhex2dec (cdr lst) (+ (* 15 (expt 16 (- (length lst) 1)))num))]
            [else (myhex2dec (cdr lst) (+ (* (car lst) (expt 16 (- (length lst) 1)))num))]
           )
          
      )
  )
 )


; ********************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (dec2hex n)

; that takes a positive integer as input, and returns 
; a list of hexadecimal digits
; The following identity should hold:

;; (hex2dec (dec2hex n)) == n

; Examples

; (dec2hex 255) => '(F F)
; (dec2hex 10) => '(A)
; (dec2hex 256) => '(1 0 0)
; (dec2hex 3735928559) => '(D E A D B E E F)

; ********************************************************

(define (dec2hex n)
  (mydec2hex n '()))
(define (mydec2hex num lst); recusrive function
  (if (= num 0) ; if @ end
      (reverse(append lst '()))
      (cond
            [(equal? (remainder num 16) 10) (mydec2hex (quotient num 16)(append lst '(A)))]
            [(equal? (remainder num 16) 11) (mydec2hex (quotient num 16)(append lst '(B)))]
            [(equal? (remainder num 16) 12) (mydec2hex (quotient num 16)(append lst '(C)))]
            [(equal? (remainder num 16) 13) (mydec2hex (quotient num 16)(append lst '(D)))]
            [(equal? (remainder num 16) 14) (mydec2hex (quotient num 16)(append lst '(E)))]
            [(equal? (remainder num 16) 15) (mydec2hex (quotient num 16)(append lst '(F)))]
            [else (mydec2hex (quotient num 16)(append lst (list(remainder num 16))))]
            );(mydec2bin (quotient num 2)(append lst (list (remainder num 2))))
      )
  )



; ********************************************************
; ** problem 5 ** (10 points)
; Write a procedure

; (range num start diff)

; that generates a list of num elements, starting with
; start and incrementing by diff

; Examples

; (range 10 1 1) => '(1 2 3 4 5 6 7 8 9 10)
; (range 3 12 -6) => '(12 6 0)
; (range 0 1 1) => '()
; (range 10 20 -2) => '(20 18 16 14 12 10 8 6 4 2)

; ********************************************************

(define (range num start diff)
  (myrange num start diff '()))
(define (myrange num start diff lst) ; recursive function, calls all input and maintains list
  (if(= num 0) ; if num = 0, ran out of numbers, end
        lst
        (myrange (- num 1)(+ start diff) diff (append lst (list start))); myrange((num-1), (start + diff), (diff), and ('(start+diff)
   )
 )


; ********************************************************
; ** problem 6 ** (10 points)
; Write a procedure

; (remove-p lst pred)

; that takes a list lst and returns that list minus 
; any elements that satisfy the given predicate pred.

; Examples:

; (remove-p '(1 2 3 4 5 6) even?) => '(1 3 5)
; (remove-p '(1 2 3 4 5 6) odd?) => '(2 4 6)
; (remove-p '(1 2 3 4 5 6) (lambda (a) (> a 3))) => '(1 2 3)
; (remove-p '(1 2 3 4 5 6) (lambda (a) (< a 3))) => '(3 4 5 6)

; ********************************************************

; ask about using pre-defined functions?
(define (remove-p lst pred)
  (myremove-p lst pred '())
 )
(define (myremove-p lst pred newlst) ; recursive function, maintains new list
  (if (null? lst)
      newlst
      (if (pred (car lst))
          (myremove-p(cdr lst) pred newlst)
          (myremove-p(cdr lst) pred (append newlst (list(car lst)))) ; why problem?????
          )
      )
  )
  

; ********************************************************
; ** problem 7 ** (10 points)
; Write a procedure 

; (collect-p lst pred)

; that takes a list lst and returns that list containing only
; the elements that satisfy the given predicate pred.

; Examples:

; (collect-p '(1 2 3 4 5 6) even?) => '(2 4 6)
; (collect-p '(1 2 3 4 5 6) odd?) => '(1 3 5)
; (collect-p '(1 2 3 4 5 6) (lambda (a) (> a 3))) => '(4 5 6)
; (collect-p '(1 2 3 4 5 6) (lambda (a) (< a 3))) => '(1 2)


(define (collect-p lst pred)
  (mycollect-p lst pred '())
 )
(define (mycollect-p lst pred newlst) ; recursive function
  (if (null? lst)
      newlst
      (if (pred (car lst))
          (mycollect-p(cdr lst) pred (append newlst (list(car lst))))
          (mycollect-p(cdr lst) pred newlst)
          )
      )
  )


; ********************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (starts-with? lst1 lst2)
; (contains? lst1 lst2)

; starts-with? returns #t if lst1 matches the beginning
; of lst2.  That is, if by deleting elements off the end of
; lst2, you can end up with lst1.

; contains? returns #t if lst1 is inside lst2.  That is, if
; by deleting elements from either or both ends of lst2,
; you can end up with lst1.

; Examples:

; (starts-with? '(1 2 3) '(1 2 3 4 5)) => #t
; (starts-with? '(1 2 3 4 5) '(1 2 3)) => #f
; (starts-with? '() '(1 2 3)) => #t
; (starts-with? '(1) '(1)) => #t

; (contains? '(3 4 5) '(1 2 3 4 5 6)) => #t
; (contains? '() '(1 2 3)) => #t
; (contains? '(4 5 6) '(1 2 3 4 5)) => #f
; (contains? '(5) '(1 2 3 4 5 6)) => #t

; ********************************************************


(define (starts-with? lst1 lst2)
  (if (empty? lst1) ; if null
      #t
      (mystarts-with? lst1 lst2)))
(define (mystarts-with? lst1 lst2)
      (if(< (length lst2) (length lst1)) ;if lst2 < lst1, lst2 can't contain
         #f
         (if (equal? (first lst1) (first lst2))
             (starts-with? (rest lst1) (rest lst2)) ; keep checking
             #f
             )
         )
  )

       
(define (contains? lst1 lst2)
  (if (null? lst1) ; if null
      #t
      (mycontains? lst1 lst2)))
(define (mycontains? lst1 lst2)
     (if(< (length lst2) (length lst1)) ;if lst2 < lst1, lst2 can't contain
         #f
         (if (equal? (first lst1) (first lst2))
             (starts-with? (rest lst1) (rest lst2)) ; keep checking if cdr starts with ehat we're looking for
             (contains? lst1 (rest lst2)) ;else run again
          )
         )
  )
         
      

; ********************************************************
; ** problem 9 (10 points)
; Write 

; (power-set lst)

; which treats the lst as a set and returns a list of all possible
; subsets

; Examples:
; (power-set '()) => '(())
; (power-set '(1)) => '(() (1))
; (power-set '(1 2)) => '(() (2) (1) (1 2))
; (power-set '(1 2 3)) => '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
; (power-set '(1 2 3 4)) => 
; '(() (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4) (1) (1 4) (1 3) (1 3 4) 
;   (1 2) (1 2 4) (1 2 3) (1 2 3 4))

; (define toppings '(onion peppers bacon sausage mushroom))
; (power-set toppings)
; '(() (mushroom) (sausage) (sausage mushroom) (bacon) (bacon mushroom)
; (bacon sausage) (bacon sausage mushroom) (peppers) (peppers mushroom)
; (peppers sausage) (peppers sausage mushroom) (peppers bacon) (peppers
; bacon mushroom) (peppers bacon sausage) (peppers bacon sausage
; mushroom) (onion) (onion mushroom) (onion sausage) (onion sausage
; mushroom) (onion bacon) (onion bacon mushroom) (onion bacon sausage)
; (onion bacon sausage mushroom) (onion peppers) (onion peppers
; mushroom) (onion peppers sausage) (onion peppers sausage mushroom)
; (onion peppers bacon) (onion peppers bacon mushroom) (onion peppers
; bacon sausage) (onion peppers bacon sausage mushroom))

(define (power-set lst)
  (if(empty? lst)
     (cons '() lst); add empty at end
     (append (map (lambda (x) (cons (car lst) x)) (power-set (cdr lst))) (power-set (cdr lst))))); append the car mapped to powerset of cdr to powerset of cdr
 
              
      
       
; ********************************************************
; ** problem 10 (10 points)
; 
; You want to automate your binge watching of movies.
; You do not care about quality of the movies, just quantity.
; Given a list of movie start and end times, figure out 
; the largest number of movies you can watch in their entirety.
; You may assume that if movie A ends at time N and movie B
; starts at time N, you can watch both A and B.

; Write

; (most-movies lst)

; where lst is a list of movie (starttime endtime) pairs.
; most-movies will return a number indicating the most movies
; you can watch from that list.


; Examples:

; (most-movies '((1 2) (2 3)) => 2
; (most-movies '((1 2) (2 3) (3 4) (1 5))) => 3
; (most-movies '((3 4) (1 2) (1 5) (4 5) (0 1)) => 4

; There are many ways to approach this problem.  I have primed you
; by giving you the power-set problem above, which can be used
; in a brute force approach.

; However, for long lists, the brute force approach can run out of 
; memory and time. There is a very nice solution that is made
; even more efficient using memoization. I'll let you think about it first.
; I'll award an extra 5 points to anyone who comes up with the
; efficient memoization solution.


; ********************************************************

(define (most-movies lst)
  (removedup (longlst lst) 0))

(define (least2greatest lst) ; sorts list by car
  (sort lst #:key car <))

(define (longlst lst) ; returns longest possible powerset of list
  (car(map least2greatest (sort (remove-p (power-set lst) (lambda (x) (< (length x) 2))) #:key length >))))

(define (removedup lst len) ; removes pairs that have same car, because aleady sorted, automatically keeps shortest movies
  (if (= (length lst) 1)
      (+ 1 len) 
      (if (= (car (car lst)) (car (car (cdr lst)))) ; if beginning of one pair and another match
          (removedup (least2greatest (cons (car lst) (cdr (cdr lst)))) len) ; run again, add car back on, sort, and ncheck if beginning of same pair and another match
          (removedup (cdr lst) (+ len 1))))) ; else, if they don't match, just run with cdr



; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

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
	
(test 'hours hours (lambda (x) (> x 0)))
	
(test 'bin2dec (bin2dec '(1 0 1 0)) 10)
(test 'bin2dec (bin2dec '(1 1 1 1 1)) 31)

(test 'dec2bin (dec2bin 10) '(1 0 1 0))
(test 'dec2bin (dec2bin 31) '(1 1 1 1 1))

(test 'hex2dec (hex2dec '(A)) 10)
(test 'hex2dec (hex2dec '(F F)) 255)
(test 'hex2dec (hex2dec '(1 0 0)) 256)
(test 'hex2dec (hex2dec '(d e a d b e e f)) 3735928559)

(test 'dec2hex (dec2hex 10) '(A))
(test 'dec2hex (dec2hex 255) '(F F))
(test 'dec2hex (dec2hex 256) '(1 0 0))
(test 'dec2hex (dec2hex 3735928559) '(D E A D B E E F))

(test 'range (range 10 1 1) '(1 2 3 4 5 6 7 8 9 10))
(test 'range (range 3 12 -6) '(12 6 0))
(test 'range (range 0 1 1) '())
(test 'range (range 10 20 -2) '(20 18 16 14 12 10 8 6 4 2))

(test 'remove-p (remove-p '(1 2 3 4 5 6) even?) '(1 3 5))
(test 'remove-p (remove-p '(1 2 3 4 5 6) odd?) '(2 4 6))
(test 'remove-p (remove-p '(1 2 3 4 5 6) (lambda (a) (> a 3))) '(1 2 3))
(test 'remove-p (remove-p '(1 2 3 4 5 6) (lambda (a) (< a 3))) '(3 4 5 6))

(test 'collect-p (collect-p '(1 2 3 4 5 6) even?) '(2 4 6))
(test 'collect-p (collect-p '(1 2 3 4 5 6) odd?) '(1 3 5))
(test 'collect-p (collect-p '(1 2 3 4 5 6) (lambda (a) (> a 3))) '(4 5 6))
(test 'collect-p (collect-p '(1 2 3 4 5 6) (lambda (a) (< a 3))) '(1 2))


(test 'starts-with? (starts-with? '(1 2 3) '(1 2 3 4 5)) #t)
(test 'starts-with? (starts-with? '(1 2 3 4 5) '(1 2 3)) #f)
(test 'starts-with? (starts-with? '() '(1 2 3)) #t)
(test 'starts-with? (starts-with? '(1) '(1)) #t)

(test 'contains? (contains? '(3 4 5) '(1 2 3 4 5 6)) #t)
(test 'contains? (contains? '() '(1 2 3)) #t)
(test 'contains? (contains? '(4 5 6) '(1 2 3 4 5)) #f)
(test 'contains? (contains? '(5) '(1 2 3 4 5 6)) #t)

(test 'power-set (power-set '()) '(()))
(test 'power-set (power-set '(1)) '(() (1)))
(test 'power-set (power-set '(1 2)) '(() (2) (1) (1 2)))
(test 'power-set (power-set '(1 2 3)) '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))

(test 'most-movies (most-movies '((1 2) (2 3))) 2)
(test 'most-movies (most-movies '((1 2) (2 3) (3 4) (1 5) (1 4))) 3)
(test 'most-movies (most-movies '((3 4) (1 2) (1 5) (4 5) (0 1))) 4)


;*********************************************************
;***** end of hw #1
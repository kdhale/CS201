#lang racket

(require racket/trace)

; ********************************************************
; CS 201 HW #1  DUE Wednesday, 9/27/2017 at 11:59 pm
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

; Also, most of these procedures involve car/cdr recursion.
; In that case, your code needs to implement the recursion, not
; simply invoke a racket procedure that allows you to avoid the problem.

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

; (depth tree)

; which takes a nest list tree as input and return an integer 
; indicating the maximum level of the tree.

; Examples

; (depth '()) => 0
; (depth '(1 2 3)) => 1
; (depth '(a (b (c (d))))) => 4
; (depth '((((((0))))))) => 6

; ********************************************************
(define (depth tree)
  (cond
    [(empty? tree)0] ; end condition
    [(not (list? (car tree))) ; if first element of list not list
     (if(equal? (cdr tree) empty) ; if next element empty, muct be end of list, so add 1
      (+ 1 (depth (cdr tree))) 
      (depth (cdr tree)))] ; else, just run for rest of tree
    [else(+ 1 (depth (car tree)) (depth (cdr tree)))])) ; if car = tree, add one, depth(car), and continue running depth for rest of list as well
  
; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 2 ** (10 points)
; Write a procedure

; (count-odd tree)

; that takes a nest list tree and returns another the number of 
; elements which are odd

; Examples

; (count-odd '(1 2 3)) => 2
; (count-odd '(1 (2 (3)))) => 2
; (count-odd '()) => 0
; (count-odd '((((((8 8 8))))))) => 0
; (count-odd '((((((9 9 9))))))) => 3


; ********************************************************
(define (count-odd tree)
  (if(empty? tree)
     0 ; end condition
    (if(not (list? (car tree))) ; if first element of list not a list
     (if (odd? (car tree)) ; if car = odd, add 1 and call on cdr
         (+ 1 (count-odd (cdr tree)))
         (count-odd (cdr tree))) ; else, just call on cdr
    (+ (count-odd (car tree)) (count-odd (cdr tree)))))); if (car is list), add c-o of car + c-o cdr 

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (count-pred pred tree)

; that takes a nest list tree and returns another the number of 
; elements which satisfy the given predicate


; Examples

; (count-pred odd? '(1 2 3)) => 2
; (count-pred even? '(1 2 3)) => 1
; (count-pred integer? '(1 (2 (3)))) => 3
; (count-pred string? '()) => 0
; (count-pred even? '((((((8 8 8))))))) => 3
; (count-pred (lambda (x) (> x 5)) '((((((9 9 9))))))) => 3

; ********************************************************

(define (count-pred pred tree)
  (if(empty? tree)
     0 ; end condition
    (if(not (list? (car tree))) ; if first element of list not a list
     (if (pred (car tree)) ; if (pred car == true), add 1 and call on cdr
         (+ 1 (count-pred pred (cdr tree)))
         (count-pred pred (cdr tree))) ; else, just call on cdr
    (+ (count-pred pred (car tree)) (count-pred pred (cdr tree)))))); if (car is list), add c-o of car + c-o cdr 

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (find-type type tree)

; that takes a data type and a nested list tree and returns #t if
; at least one of the leaf nodes is of the given type

; Examples:

; (find-type 'integer '((1 2 3))) => #t
; (find-type 'flonum '((1 2 3))) => #f
; (find-type 'flonum '((1 2 1.3))) => #t
; (find-type 'symbol '((1 2 3))) => #f
; (find-type 'symbol '((a b c))) => #t
; (find-type 'string '((a ("b") c))) => #t
; (find-type 'character '((a ("b") #\c))) => #t

;; boolean types are legal
; ********************************************************

(define (find-type type tree)
  (if(empty? tree)
     #f ; end condition
    (if(not (list? (car tree))) ; if first element of list not a list
     (if (determine-type (car tree) type) ; if type matches car, end
         #t
         (find-type type (cdr tree))) ; else, just call on cdr
    (or (find-type type (car tree)) (find-type type (cdr tree)))))); run on (car), and if nothing found, run on cdr

(define (determine-type n type) ; determines what data type 'type' is
  (cond
    [(equal? type 'integer) (integer? n)] ; if type = integer, return if n = intege
    [(equal? type 'flonum) (flonum? n)] ; if type = flonum, return if n = flonum
    [(equal? type 'symbol) (symbol? n)] 
    [(equal? type 'string) (string? n)] 
    [(equal? type 'character) (char? n)] 
    [(equal? type 'boolean) (boolean? n)]
    [else false]))

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 5 ** (10 points)
; Write a procedure

; (types tree)

; that takes a nested list tree and returns a list of the data
; types in the tree in alphbetical order

; Examples

; (types '(1 1 1 1)) => '(integer)
; (types '(1 ((a)))) => '(integer symbol)
; (types '(1 (("a")))) => '(integer string)
; (types '(1 ((#\a)))) => '(character integer)
; (types '(1.0 ((#\a)))) => '(character flonum)
;; boolean types are legal

; ********************************************************
(define (types tree)
  (sort (remove-duplicates (mytypes tree '())) symbol<?))
  

(define (mytypes tree lst)
  (if(empty? tree)
    lst ; end condition
    (if(not (list? (car tree))) ; if first element of list not a list ; if type matches car, end
       (mytypes (cdr tree) (append (id-type (car tree)) lst)) ; else, just call on cdr
    (append lst (mytypes (car tree) lst)(mytypes (cdr tree) lst))))); run on (car), and if nothing found, run on cdr

(define (id-type n) ; determines what data type 'type' is
  (cond
    [(flonum? n) (list 'flonum)] ; if type = flonum, return if n = flonum
    [(integer? n) (list 'integer)] ; if type = integer, return if n = intege
    [(symbol? n) (list 'symbol)] 
    [(string? n) (list 'string)] 
    [(char? n) (list 'character)] 
    [(boolean? n) (list 'boolean)]
    [else false]))

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 6 ** (10 points)
; Write a procedure

; (cull pred lst)

; that takes a predicate pred and a list lst and returns a new list
; of two lists: the first list comprises the elements that satisfy
; pred, and the second list comprises elements that do not.

; Examples:

; (cull even? '(1 2 3 4 5 6)) => '((2 4 6) (1 3 5))
; (cull symbol? '(1 2 a b 3 3 d e)) => '((a b d e) (1 2 3 3))
; (cull even? '(2 4 6)) => '((2 4 6) ())
; (cull odd? '(2 4 6)) => '(() (2 4 6))
; (cull odd? '()) => '(() ())

; ********************************************************
(define (cull pred lst)
  (append  (list (culltrue pred lst)) (list(cullfalse pred lst)))) ; add lists together

(define (culltrue pred lst); for list that is pred
  (if (empty? lst)
      lst
      (if (pred (car lst))
          (append (list (car lst))(culltrue pred (cdr lst)))
          (culltrue pred (cdr lst)))))

(define (cullfalse pred lst); for list the is anti pred
  (if (empty? lst)
      lst
      (if (not (pred (car lst)))
          (append (list (car lst))(cullfalse pred (cdr lst)))
          (cullfalse pred (cdr lst)))))
          

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 7 ** (10 points)
; Write a procedure 

; (tree-min tree)

; that takes a nested list tre whose leaf nodes are integers 
; and returns the minimum of those integers.

; Examples:

; (tree-min '(1 2 3)) => 1
; (tree-min '(1 (2 (-3)))) => -3
; (tree-min '()) => '()
; (tree-min '(((((((((7)))))))))) => 7
; (tree-min '((((((((()))))))))) => '()

; ********************************************************
(define (tree-min tree)
  (cond
    [(empty? tree)empty] ; base case
    [(list? (car tree))(my-min (tree-min(car tree))(tree-min(cdr tree)))]; recusive call, if car = list, return min of recursive call of car vs cdr
    [else (my-min(car tree)(tree-min(cdr tree)))] ; if car = number, return min of car vs recursive call of rest
   ))
(define (my-min n1 n2); mbasically, if gets down to empty list and number, choose number, othervise return minimum 
  (cond
   [(empty? n1) n2]
   [(empty? n2) n1]
   [else (min n1 n2)]))
   

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 8 ** (10 points)
; Write the procedure

; (count-leaves tre)

; count-leaves takes a nest list tree as an argument and returns
; an integer which is the number of leaves in the tree.

; Examples:

; (count-leaves '(1 2 3)) => 3
; (count-leaves '()) => 0
; (count-leaves '(1 (2 (3 (4))))) => 4
; (count-leaves '(((((((7)))))))) => 1

; ********************************************************

(define (count-leaves tree)
  (cond
    [(empty? tree)0] ; end condition
    [(not (list? (car tree)))(+ 1 (count-leaves (cdr tree)))] ; else, just run for rest of tree
    [else (+ (count-leaves (car tree)) (count-leaves (cdr tree)))])) ; if car = tree, add one, depth(car), and continue running depth for rest of list as well

; (Replace this comment with your procedures.)

; ********************************************************
; ** problem 9 ** (10 points)
; Write a procedure

; (map-tree proc tree)

; which takes two arguments, a procedure proc and a nested list tree,
; and returns a copy of tree with each leaf node replaced by
; the result of applying proc to that leaf.

; Examples:

; (map-tree even? '(1 2 3 4)) => '(#f #t #f #t)
; (map-tree even? '(1 (2 (3 (4))))) => '(#f (#t (#f (#t))))
; (map-tree (lambda (x) (+ x 1)) '(1 (2 (3 (4 5 6))))) => '(2 (3 (4 (5 6 7))))
; (map-tree odd? '()) => '()

; ********************************************************
(define (map-tree proc tree)
  (cond
    [(empty? tree) empty] ; if empty, return empty tree
    [(not (list? (car tree)))(cons (proc (car tree)) (map-tree proc (cdr tree)))] ; if car isn't list, cons the (proc car) and recursive call of cdr
    [else (cons (map-tree proc (car tree)) (map-tree proc (cdr tree)))])) ; else, cons recursive call of car and recursive call of cdr

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 10 (10 points)
; Write a procedure

;; (quadratic-roots a b c)

;; which calculates the roots of a quadratic equation

;;  a x^2 + b x + c

;; using the quadratic formula

;; see https://en.wikipedia.org/wiki/Quadratic_formula

;; The answer can be either a single root or two complex roots

;; The discriminant is (b^2 - 4 a c)
;; if the discriminant is 0, there is only one root
;; otherwise there are two.

;; Examples:

;; (quadratic-roots 1 -14 49) => 7
;; (quadratic-roots 1 4 -5) => '(1 -5)
;; (quadratic-roots 1 6 9) => -3
;; (quadratic-roots 1 4 4) => -2 
;; (quadratic-roots 1 0 -4)=>  '(2 -2)
;; (quadratic-roots 1 0 -4)=>  '(0+2i 0-2i)


; ********************************************************
(define (quadratic-roots a b c)
  (let ([discrim (- (expt b 2) (* 4 a c))]) ; defining descriminant so don't have to keep reassigning
    (if (equal? discrim 0); if d = 0, only have to worry about dividing -b by 2a
        (/ (- 0 b) (* 2 a))
        (list(/ (+ (- 0 b) (sqrt discrim)) (* 2 a))(/ (- (- 0 b) (sqrt discrim)) (* 2 a)))))) ; else, list of b+ and b-

; (Replace this comment with your procedure(s).)

; ********************************************************
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

(test 'depth (depth '())  0)
(test 'depth (depth '(1 2 3)) 1)
(test 'depth (depth '(a (b (c (d))))) 4)
(test 'depth (depth '((((((0))))))) 6)
	
(test 'count-odd (count-odd '(1 2 3)) 2)
(test 'count-odd (count-odd '(1 (2 (3)))) 2)
(test 'count-odd (count-odd '()) 0)
(test 'count-odd (count-odd '((((((8 8 8))))))) 0)
(test 'count-odd (count-odd '((((((9 9 9))))))) 3)

(test 'count-pred (count-pred odd? '(1 2 3)) 2)
(test 'count-pred (count-pred even? '(1 2 3)) 1)
(test 'count-pred (count-pred integer? '(1 (2 (3)))) 3)
(test 'count-pred (count-pred string? '()) 0)
(test 'count-pred (count-pred even? '((((((8 8 8))))))) 3)
(test 'count-pred (count-pred (lambda (x) (> x 5)) '((((((9 9 9))))))) 3)

(test 'find-type (find-type 'integer '((1 2 3))) #t)
(test 'find-type (find-type 'flonum '((1 2 3))) #f)
(test 'find-type (find-type 'flonum '((1 2 0.3 3))) #t)
(test 'find-type (find-type 'symbol '((1 2 3))) #f)
(test 'find-type (find-type 'symbol '((a b c))) #t)
(test 'find-type (find-type 'string '((a ("b") c))) #t)
(test 'find-type (find-type 'character '((a ("b") #\c))) #t)
(test 'find-type (find-type 'character '((a ("b") "c"))) #f)

(test 'types (types '(1 1 1 1)) '(integer))
(test 'types (types '(1 ((a)))) '(integer symbol))
(test 'types (types '(1 (("a")))) '(integer string))
(test 'types (types '(1 ((#\a)))) '(character integer))
(test 'types (types '(1.0 (("a")))) '(flonum string))

(test 'cull (cull even? '(1 2 3 4 5 6)) '((2 4 6) (1 3 5)))
(test 'cull (cull symbol? '(1 2 a b 3 3 d e)) '((a b d e) (1 2 3 3)))
(test 'cull (cull even? '(2 4 6)) '((2 4 6) ()))
(test 'cull (cull odd? '(2 4 6)) '(() (2 4 6)))
(test 'cull (cull odd? '()) '(() ()))

(test 'tree-min (tree-min '(1 2 3)) 1)
(test 'tree-min (tree-min '(1 (2 (-3)))) -3)
(test 'tree-min (tree-min '()) '())
(test 'tree-min (tree-min '(((((((((7)))))))))) 7)
(test 'tree-min (tree-min '((((((((()))))))))) '())

(test 'count-leaves (count-leaves '(1 2 3)) 3)
(test 'count-leaves (count-leaves '()) 0)
(test 'count-leaves (count-leaves '(1 (2 (3 (4))))) 4)
(test 'count-leaves (count-leaves '(((((((7)))))))) 1)

(test 'map-tree (map-tree even? '(1 2 3 4)) '(#f #t #f #t))
(test 'map-tree (map-tree even? '(1 (2 (3 (4))))) '(#f (#t (#f (#t)))))
(test 'map-tree (map-tree (lambda (x) (+ x 1)) '(1 (2 (3 (4 5 6))))) '(2 (3 (4 (5 6 7)))))
(test 'map-tree (map-tree odd? '()) '())

(test 'quadratic-roots (quadratic-roots 1 -14 49) 7)
(test 'quadratic-roots (quadratic-roots 1 4 -5) '(1 -5))
(test 'quadratic-roots (quadratic-roots 1 6 9) -3)
(test 'quadratic-roots (quadratic-roots 1 4 4) -2)
(test 'quadratic-roots (quadratic-roots 1 0 -4) '(2 -2))
(test 'quadratic-roots (quadratic-roots 1 0 4) '(0+2i 0-2i))


; ========================================================
; **** end of hw #2
; ========================================================

#lang racket

(provide 
 lorint time-calls
 total-order?
 sorted? insert merge
 isort msort
 count-compares)

; Please do not change lines above this one.

;************************************************************
; CS 201a HW #8  DUE Sunday December 10th at 11:59 pm, 
; via the submit system on the Zoo.  This assignment is worth 90 points.
;************************************************************
; Name: Kenia Hale
; Email address: kenia.hale@yale.edu
;************************************************************

; Computer science topics: running times of programs, insertion sort,
; merge sort.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************

; Timing procedures.
; The Racket procedure (current-inexact-milliseconds)
; returns a real (in the sense of Racket) number representing
; the current time in milliseconds since midnight UTC, January 1, 1970.

;************************************************************
; ** problem 1 ** (10 points)

; Write two procedures

; (lorint count bound)
; (time-calls reps proc args)

; (lorint count bound) takes a nonnegative
; integer count and a positive integer bound
; and returns a list of count randomly chosen integers 
; in the range from 0 through bound - 1.

; (time-calls reps proc args) takes 
; a nonnegative integer, reps,
; a procedure, proc,
; a list of arguments for the procedure, args,
; and returns the amount of time in SECONDS elapsed
; in calling proc on args a number of times equal to reps.

; Recall that we can apply a proc to args with (apply proc args).
; Note that a millisecond is 1/1000 of a second.

; Examples of lorint
;> (lorint 10 100)
;'(49 14 28 15 12 80 33 69 18 57)
;> (lorint 10 3)
;'(0 0 2 1 0 0 1 2 0 1)

; The following examples of time-calls were run on my workstation and
; show that calling the built-in plus procedure 10,000 times on
; the arguments 13 and 14 took somewhat more than 0.001 seconds,
; while doing the same thing 100,000 times took somewhat more
; than 0.01 seconds, and a million times took somewhat more than 0.1
; seconds.  The first two runs show random variation in the measured times.

; When the number of repetitions is multiplied by 10, the time is
; also (approximately) multiplied by 10.

;> (time-calls 10000 + (list 13 14))
;0.00168701171875
;> (time-calls 10000 + (list 13 14))
;0.00122412109375
;> (time-calls 100000 + (list 13 14))
;0.012380859375
;> (time-calls 1000000 + (list 13 14))
;0.12706494140625

; The following examples show timings (on my workstation)
; for creating lists of 100,000 or 200,000 or 300,000
; random numbers in the range 0 to 9 inclusive.
; About a third of a second suffices in the last case.

;> (time-calls 1 lorint (list 100000 10))
;0.074503173828125
;> (time-calls 1 lorint (list 200000 10))
;0.19560009765625
;> (time-calls 1 lorint (list 300000 10))
;0.33381982421875
;******************************(******************************

(define (lorint count bound)
  (if (equal? count 0)
      '()
      (cons (random 0 bound) (lorint (- count 1) bound))))

(define (time-calls reps proc args)
  (let ([x (current-inexact-milliseconds)])
  (/ (- (time-calls-rec reps proc args) x) 1000)))

(define (time-calls-rec reps proc args)
  (cond
    [(equal? 0 reps)(current-inexact-milliseconds)]
    [else (apply proc args) (time-calls-rec (- reps 1) proc args)]))

;(current-inexact-milliseconds)

;************************************************************
; ** problem 2 ** (15 points)
; For this problem, use your procedure time-calls
; to time the built-in Racket procedures:

; length, take, drop

; and report the following measurements, and answer the following questions.
; Comment out your responses with semicolons.

; For length, report measurements of 100 repetitions of calling length
; on a list of length k * 100,000 for k = 1,2,3,4.

; For take and drop, report measurements of 100 repetitions of calling take (or drop)
; on a list of length k * 100,000 for k = 1,2,3,4, with the number
; of elements to take (or drop) being half the length of the list.

; You may want to do several measurements because of random variation.

; For each procedure, is O(1) or O(n) a better description of the running
; time as a function of the length n of the list argument?  Compare the
; times taken by the three procedures on comparable inputs -- which is 
; fastest? slowest?  Can you explain *why* on the basis of how lists and
; their operations are implemented?  (Complex statistical analysis is not
; necessary.)
;************************************************************

; LENGTH
; k = 1 : 0.07564788818359375
; k = 2 : 0.1545704345703125
; k = 3 : 0.22802099609374998
; k = 4 : 0.30173828125
; O(n) is best to to represent length

; TAKE
;k = 1 : 1.6206527099609376
;k = 2 : 4.006476745605469
;k = 3 : 4.1586673583984375
;k = 4 : 4.566901550292969
; O(1) is the best to represent length
; Slowest, because has to keep track of index and create "fresh" list, has to run through again

; DROP
;k = 1 : 0.050645812988281254
;k = 2 : 0.11458038330078124
;k = 3 : 0.16971801757812502
;k = 4 : 0.22688824462890625
; O(n) is the best to represent length
; Fastest, because only have to go through half of list, then can return everything after that



;************************************************************
; We represent a total ordering on a set X of values via a predicate
; (compare? x y), that returns #t or #f.  The results must
; satisfy the following properties for all values x, y, z from the set X:
; (1) if (equal? x y) => #t then (compare? x y) => #t,
;* (1) (if (equal? x y) 
;           (compare? x y) 
;           #t)
; (2) if (and (compare? x y) (compare? y x)) => #t, then (equal? x y) => #t,
;* (2) (if (and (compare? x y) (compare? y x)) 
;           (equal? x y) 
;           #t)
; (3) if (and (compare? x y)(compare? y z)) => #t, then (compare? x z) => #t,
;* (3) (if (and (compare? x y)(compare? y z)) 
;           (compare? x z) 
;           #t)
; (4) (or (compare? x y) (compare? y x)) => #t.
;* (4) (or (compare? x y) (compare? y x))


; If the set X is finite, then we can write a procedure to test
; whether all these properties hold of a proposed total ordering compare? 
; on the set X.  This is what the next problem asks you to do.
; Note that you do NOT need to complete this problem before doing
; the subsequent ones.

;************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (total-order? compare? domain)

; that takes a predicate (compare? x y) and a list of values domain
; such that whenever x and y are values from domain, (compare? x y)
; returns either #t or #f.
; The procedure returns #t if compare? is a total order on domain
; (that is, satisfies the four properties above for all x, y, z from domain),
; and #f otherwise.

; Hint: it might be helpful to write a procedure to check these conditions
; one pair x, y at a time.

; QUESTION: What is the running time of your procedure in terms of n,
; the number of elements in the domain.  Assume compare? takes time O(1).
; Give your answer in terms of O, Theta, or Omega, as appropriate and
; explain why it is correct.  Comment out your answer with semicolons.

; The running time is O(1), because every time I ran it on increasingly larger lists, the running time stayed the same

; Examples
;> (total-order? <= '(1 3 5 4))
;#t
;> (total-order? < '(1 3 5 4))
;#f
;> (total-order? >= '(3 2 4 5 1))
;#t
;> (total-order? string<=? (list "hi" "hey" "hello"))
;#t
;> (total-order? equal? (list "hi" "hey" "hello"))
;#f
;************************************************************

(define (compare-two? x y pred)
  (cond
    [(and (equal? x y) (apply pred (list x y))) #t]
    [(and (and (apply pred (list x y)) (apply pred (list y x))) (equal? x y)) #t]
    [(or (apply pred (list x y)) (apply pred (list y x))) #t]
    [else #f]))

(define (total-order? compare? domain)
  (cond
    [(empty? domain)#t]
    [(equal? 1 (length domain))(compare-two? (car domain) (car domain) compare?)]
    [else (and (compare-two? (car domain) (car domain) compare?) (and (compare-two? (car domain) (car (cdr domain)) compare?) (total-order? compare? (cdr domain))))]))

;************************************************************

; Now we turn to sorting a list of elements with respect to a given
; comparison operator.  You don't need to have done problem 3 to
; do the following problems.

;************************************************************
; ** problem 4 ** (15 points)
; Write three procedures

; (sorted? compare? lst)
; (insert compare? item lst)
; (merge compare? lst1 lst2)

; For each of these procedures, you may assume that
; compare? is a total order on the elements of lst,
; item and the elements of lst, and the elements of lst1 and lst2,
; respectively.

; (sorted? compare? lst)
; takes a list of items and returns #t or #f
; depending on whether the items of lst are
; sorted with respect to the comparison predicate
; compare?
; In other words, the result should be #f if and only if
; there are two consecutive elements of lst for
; which compare? returns #f.

; (insert compare? item lst)
; inserts an item into a list lst of items
; which is sorted with respect to the compare?
; predicate, so that the resulting list of
; items is also sorted with respect to the
; compare? predicate.

; (merge compare? lst1 lst2)
; takes two lists of elements lst1 and lst2, each of which is sorted with
; respect to the compare? predicate, and produces as its result a list
; of all the items in lst1 and lst2 (preserving duplicates) that is
; sorted with respect to compare?

; Examples
;> (sorted? <= '(1 4 5 8 10))
;#t
;> (sorted? >= '(10 9 4 7 6))
;#f
;> (insert <= 3 '(1 2 4 5))
;'(1 2 3 4 5)
;> (insert string>=? "hello" (list "the" "best" "arrangment"))
;'("the" "hello" "best" "arrangment")
;> (merge >= '(10 7 4 2 1) '(22 9 5))
;'(22 10 9 7 5 4 2 1)
;> (merge string<=? (list "a" "novel" "thought") (list "more" "predictive"))
;'("a" "more" "novel" "predictive" "thought")
;************************************************************

(define (sorted? compare? lst)
  (cond
    [(equal? (length lst) 2)(apply compare? lst)]
    [(apply compare? (list (car lst) (car (cdr lst))))(sorted? compare? (cdr lst))]
    [else #f]))

(define (insert compare? item lst)
 (cond
   [(empty? lst) (list item)]
   [(apply compare? (list item (car lst)))(append (list item) lst)] ; basically keep trying to put it in every pace till it's sorted
   [else (cons (car lst) (insert compare? item (cdr lst)))]))

(define (merge compare? lst1 lst2)
 (cond
   [(empty? lst1)lst2] ; if either runs out, add the rest ot the other
   [(empty? lst2)lst1]
   [(apply compare? (list (car lst1) (car lst2)))(cons (car lst1) (merge compare? (cdr lst1) lst2))] ; if car lst1 show go before car lst2, cons car lst1, run again on cdr lst1 and lst2 
   [else (cons (car lst2) (merge compare? (cdr lst2) lst1))])); 

;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (isort compare? lst)
; (msort compare? lst)

; Each takes a total order comparison predicate compare? and a list
; lst of items, and returns a list of all the elements in lst (duplicates
; preserved) arranged so that they are sorted with respect to compare?

; (isort compare? lst) should use (insert compare? item lst) and
; should implement insertion sort.

; (msort compare? lst) should use (merge lst1 lst2) and should
; implement merge sort.

; Examples
;> (isort string<=? (list "predictive" "novel" "a" "more" "thought"))
;'("a" "more" "novel" "predictive" "thought")
;> (msort string>=? (list "predictive" "novel" "a" "more" "thought"))
;'("thought" "predictive" "novel" "more" "a")
;************************************************************

(define (isort compare? lst)
 (cond
   [(sorted? compare? lst) lst]
   [else (build-sorted-lst compare? lst '())]))

(define (build-sorted-lst compare? lst beglst) ; constantly building up a sorted list at the beginning
  (cond
    [(empty? lst) beglst]
    [else (build-sorted-lst compare? (cdr lst) (insert compare? (car lst) beglst))]))

(define (msort compare? lst) ; keep splitting in half, then marge all the halfs together
  (cond
    [(equal? 1 (length lst)) lst]
    [(equal? 2 (length lst)) (merge compare? (list (car lst)) (cdr lst))]
    [else (merge compare? (msort compare? (take lst (quotient (length lst) 2))) (msort compare? (drop lst (quotient (length lst) 2))))]))

;************************************************************
; ** problem 6 ** (20 points)
; By using sufficiently long lists of integers,
; and possibly repeating and averaging measurements, 
; give empirical evidence for the claims that:
; (1) your implementation of insertion sort (isort, above) has best
; case time Theta(n) and worst case time of Theta(n^2).
; (2) your implementation of merge sort (msort, above) has best case and
; worst case times of Theta(n log n).

; Please identify inputs that give best and worst cases for your
; implementations of isort and msort.

; QUESTION: Empirically, what do the average running times of isort and
; msort seem to be?  Be sure that you use sufficiently long lists of
; randomly chosen integers in a range larger than the length of the list,
; so that there are unlikely to be many duplicate values.

; QUESTION: Roughly what is the longest list of random integers that your isort
; procedure can sort in 10 seconds?  Same question for your msort procedure?

; Because of memory caching and other effects, the timing behaviors will not
; necessarily uniform over the whole range of feasible input lengths.
;************************************************************

(define (one-to-thousand a num)
  (if (equal? num (* a 1000))
      '()
      (cons num (one-to-thousand a (+ num 1)))))
;(1)
; isort <= on list of x already sorted numbers
; x = 1000 : 0.0029999186197916666
; x = 2000 : 0.01066650390625
; x = 3000 : 0.024347086588541664
; x = 4000 : 0.04150830078125

; isort >= on list of x number sorted backwards
; x = 1000 : 1.7130730794270832
; x = 2000 : 13.765224039713543
; x = 3000 : 31.453740804036457
; x = 4000 : 65.44350878906249

; 

;(2)
; merge sort <= on list of x already sorted numbers
; x = 1000 : 0.0029995930989583333
; x = 2000 : 0.006666097005208333
; x = 3000 : 0.012040201822916666
; x = 4000 : 0.0219951171875

; merge sort >= on list of x number sorted backwards
; x = 1000 : 0.0030000000000000005
; x = 2000 : 0.007000325520833334
; x = 3000 : 0.012672037760416666
; x = 4000 : 0.02266748046875

; Average Time of isort O(n^2), msort O(n)

; QUESTION: Roughly what is the longest list of random integers that your isort
; procedure can sort in 10 seconds?  Same question for your msort procedure?
; isort : 10500
; msort : 490000

;(/ (+ (time-calls 1 msort (list < (one-to-thousand 4 0))) (+ (time-calls 1 msort (list < (one-to-thousand 4 0))) (+ (time-calls 1 msort (list < (one-to-thousand 4 0)))))) 3)

;************************************************************
; ** problem 7 ** (10 points)
; Write one procedure

; (count-compares sort compare? lst)

; that returns the number of calls to the compare? procedure
; when we apply the procedure sort to the arguments compare? and lst.
; Think of sort as a procedure like msort or isort, taking a comparison
; predicate and a list as its arguments, though sort could
; be some other sorting procedure devised for testing purposes.

; The trick here is to take the compare? procedure and "wrap" it
; in another procedure that will count the number of times it
; is called.  Then call sort with the "wrapped" compare? and lst
; as inputs.  Finally, return the final count from the "wrapped"
; compare? as the value from count-compares.

; Please read about the mutator set! to help you keep the count.

; Examples (yours may randomly vary.)
;> (count-compares msort <= (lorint 10 100))
;23
;> (count-compares msort <= (lorint 10 100))
;22
;> (count-compares isort <= (lorint 10 100))
;34
;************************************************************
  
(define (count-compares sort compare? lst)
  (let* ([count 0]
         [oldc? compare?])
  (set! compare? (lambda (a b) (set! count (+ 1 count)) (oldc? a b))) ; will increment count and run compare using the old compare?
  (apply sort (list compare? lst))
  count)); output count

;************************************************************
; ** problem 8 ** (10 points)
; This is where the test code normally appears.
; For this assignment, write your own tests.  

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    '---X---)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'lorint (lorint 10 100) '(49 14 28 15 12 80 33 69 18 57))
(test 'lorint (lorint 10 3) '(0 0 2 1 0 0 1 2 0 1))
(test 'time-calls (time-calls 10000 + (list 13 14)) 0.00168701171875)
(test 'time-calls (time-calls 100000 + (list 13 14)) 0.012380859375)
(test 'time-calls (time-calls 1000000 + (list 13 14)) 0.12706494140625)
(test 'time-calls (time-calls 1 lorint (list 100000 10)) 0.074503173828125)
(test 'time-calls (time-calls 1 lorint (list 200000 10)) 0.19560009765625)
(test 'time-calls (time-calls 1 lorint (list 300000 10)) 0.33381982421875)
(test 'time-calls (time-calls 1 isort (list <= (lorint 1000 100000))) 0.117006591796875)
(test 'time-calls (time-calls 1 isort (list <= (lorint 2000 100000))) 0.36501953125)
(test 'time-calls (time-calls 1 isort (list <= (lorint 3000 100000))) 0.86504736328125)

;********* end of hw8, end of hws! **************************

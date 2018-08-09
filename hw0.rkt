#lang racket

(provide hours
	 hypotenuse
	 favorite)

; Please do not modify the lines above this one.

; ********************************************************
; CS 201 HW #0  DUE Wednesday 9/13/17, 11:59 pm
; ** using the Zoo submit system **

; ********************************************************
; Name: Kenia Hale
; Email address: kenia.hale@yale.edu
; ********************************************************

; This file may be loaded into Racket.  
; Lines beginning with semicolons are comments.

; Homework #0 will be worth 20 points -- other
; homeworks will be worth 100 points.
; One purpose of homework #0 is to make sure 
; you can use the submit system on the Zoo.  
; You will receive no credit for this assignment until
; you successfully use the submit system to submit it.

; You will be submitting *two* files for homework #0.
; Please name them:
; hw0.rkt (for the Racket definitions and procedures)
; response.pdf (for the reading response)

; ********************************************************
; ** problem 0 ** (1 easy point) 
; Replace the number 0 in the definition below to
; indicate how many hours you spent doing this assignment.
; Fractions are fine, eg, 3.14159.
; You will receive no credit for this problem if
; you leave the number as 0.

(define hours 1) ; DON'T FORGET TO CHANGE

; ********************************************************
; ** problem 1 ** (5 points)

; Write a procedure (hypotenuse x y)
; that takes as arguments positive integers x and y
; and uses the Pythagorean theorem to calculate the 
; hypotenuse of the right triangle with sides of length
; x and y

; Examples

; (hypotenuse 3 4)  => 5
; (hypotenuse 5 12) => 13
; (hypotenuse 1 1)  => 1.4142135623730951

; ********************************************************

; Replace this comment and the code below
; with your procedure.
; Make sure it is named hypotenuse
; and has two arguments.  The names of
; the arguments are not important.

; Your procedure will
; be tested automatically, but it will
; be called only on positive integer arguments.

(define (hypotenuse x y)
	(sqrt (+ (* x x) (* y y))))

; ********************************************************
; ** problem 2 ** (4 points)

; Write a procedure (favorite)
; that takes no arguments and
; returns a *string* indicating
; something you like to eat.

; Please remember the difference between
; a procedure call and the evaluation
; of a variable!

; Example (yours will likely be different)

; (favorite) => "pizza with artichoke hearts"

; ********************************************************

; Replace this comment and the code below
; with your procedure.
; Make sure it is named favorite and takes
; no arguments.  Your procedure will be
; tested automatically, and will be called
; only with no arguments.

(define (favorite) "Solange")

; ********************************************************
; ** problem 3 ** (10 points)

; For this problem, you are asked to find one
; article (of 2 pages or more) in
; the magazine "Communications of the ACM",
; in one of the four issues:
; June, July, August, September 2017.
; See http://cacm.acm.org/

; read the article and answer 
; the following three questions:

;   a. What did you know about the topic
;      prior to reading the article?
;   b. What did you learn from reading the
;      article?
;   c. What more would you like to know
;      about the topic?

; Your answer should be AT MOST 2 pages
; saved in pdf format, and submitted as
; the file response.pdf for assignment 0.
; Please include in your file 
; your name and email address
; and the title and author(s) of the article
; you are responding to.

; Your grade for this problem will be 10 if I can
; open, print and read your submitted pdf file.
; It is to help me get acquainted with you and
; your interests -- you won't receive feedback
; on your answers.

; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (if (expected got) 'OK-TEST
				  'FAILED-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'hours hours (lambda (x) (> x 0)))
(test 'hypotenuse (hypotenuse 3 4) 5)
(test 'hypotenuse (hypotenuse 5 12) 13)
(test 'hypotenuse (hypotenuse 6 8) 10)
(test 'favorite (favorite) (lambda (x) (string? x)))


; ********************************************************
; ********  end of homework #0
; ********************************************************

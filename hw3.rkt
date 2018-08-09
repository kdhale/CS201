#lang racket

(provide hours
         ins ins-c-state ins-c-symbol ins-n-state ins-n-symbol ins-dir
         tm-reverse
         i-match? i-lookup
         conf conf-state conf-ltape conf-symbol conf-rtape
         halted? change-state write-symbol
	 normalize
         shift-head-left shift-head-right
         next-config
         tm-convert
	 tm-sort)

; Please do not modify the lines above this comment.
; ********************************************************
; CS 201 HW #3  DUE Sunday, 10/8/2017 at 11:59 pm
;                via the submit system on the Zoo
; ****************************************************************
; Name: Kenia Hale
; Email address: kenia.hale@yale.edu
; ****************************************************************

; Unless the problem specifies otherwise:
; (a) You may solve the problem using any method and any Racket constructs 
; (*except* mutators, that is, set! and its relatives.)
; (b) You may write auxiliary procedure(s) in addition to the one(s) specified in the problem.  
; Please include a comment for each one specifying what it does and giving one or more examples of it.
; (c) Please make your code as clear and readable as possible.

; The topics of this assignment are:
; a simulator for Turing machines and writing Turing machine programs.

; ****************************************************************
; ** problem 0 ** (1 easy point)
; Modify the following definition to reflect the number of hours you spent on this assignment.

(define hours 5)

; ****************************************************************
; Turing machines were described in the lectures; see also the lecture notes on the course web page.
; Here is a top-level procedure to simulate a Turing machine starting from a given configuration until 
; either it halts or it has executed n steps.
; The procedure returns the list of the successive configurations of the computation,
; starting with the initial one.
; The length of the list of configurations is one more than 
; the number of steps taken by the machine.

(define (simulate mach config n) 
  (cond
    ((<= n 0) (list config))
    ((halted? mach config) (list config))
    (else
     (cons config
           (simulate 
            mach (next-config mach config) (- n 1))))))

; mach is a representation of a Turing machine
; config is a representation of a configuration of the machine
; n is the maximum number of steps to simulate

; The procedures halted? and next-config will be
; written by you in the problems below; you will then
; have a complete Turing machine simulator.

; ****************************************************************
; Turing machine representation.

; A Turing machine is represented as a list of instructions, 
; where each instruction is a 5-tuple, represented as a struct
; defined as follows:

(struct ins (c-state c-symbol n-state n-symbol dir) #:transparent) 

; The fields represent the following components of an instruction:
; current state, current symbol, new state, new symbol, and move direction

; The current state and new state are Racket symbols,
; the current symbol and new symbol are Racket symbols or non-negative integers
; and the move direction must be either the symbol 'L or the symbol 'R,
; representing a move to the left or right, respectively.

; Example
; > (define i1 (ins 'q1 0 'q3 1 'L))
; creates an instruction with with current state 'q1, current symbol 0,
; new state 'q3, new symbol 1, and move direction 'L,
; and names it i1.

; Because we've made ins "transparent", its field values
; will be printed out.
; > i1
; (ins 'q1 0 'q3 1 'L)

; We can access the components of i1 via the structure selectors:
; (ins-c-state i1) => 'q1
; (ins-c-symbol i1) => 0
; (ins-n-state i1) => 'q3
; (ins-n-symbol i1) => 1
; (ins-dir i1) => 'L

; Example (from lecture):
; a Turing machine that when started in state 'q1 on the leftmost of a string of 0's and 1's 
; changes all the 0's to 1's and all the 1's to 0's 
; and then returns the head to the leftmost symbol and halts.

(define tm1 
  (list
   (ins 'q1 0 'q1 1 'R)
   (ins 'q1 1 'q1 0 'R)
   (ins 'q1 'b 'q2 'b 'L)
   (ins 'q2 0 'q2 0 'L)
   (ins 'q2 1 'q2 1 'L)
   (ins 'q2 'b 'q3 'b 'R)))

; ****************************************************************
; ** problem 1 (15 points)
; Define (in the format just given) a Turing machine named

; tm-reverse

; that takes an input string of 0's and 1's 
; and produces an output string equal to the reverse of the input string.
; When the machine halts, the head should be scanning the leftmost symbol 
; of the output.

; That is, when started in state q1 with the head on the leftmost of a
; string of 0's and 1's, it halts with the head on the leftmost of a
; string of 0's and 1's, and the output string is obtained from
; the input string by reversing it.

; Your machine *may* use additional tape symbols but the output should contain no
; symbols other than 0, 1 and blank.
; When the machine halts, symbols other than the output should be blank.

; Examples of the behavior of tm-reverse
; 1            =>  1
; 110          =>  011
; 0001         =>  1000
; 101011       =>  110101

; (It may help to review ideas from the machine to make a copy of its input,
; described in lectures and in the online lecture notes.)

; The initial state of your machine should be q1 -- other states may be named with
; Racket symbols of your choice.

; IMPORTANT: please describe how your Turing machine works.
; You'll be able to run it once you get the procedures for the simulator working.

;(simulate tm-reverse (conf 'q1 '() 1 '(1 0)) 100)

; ****************************************************************
(define tm-reverse
  (list
   (ins 'q1 0 'q1 0 'R)
   (ins 'q1 1 'q1 1 'R)
   (ins 'q1 'b 'q2 'c 'L)

   ; q2 calls q3 and q4 on 1 and 0, but will ove left otherwise, passing over everything else
   (ins 'q2 0 'q3 'e 'R) ; if 0, go to q3, change 0 to 'e
   (ins 'q2 1 'q4 'e 'R) ; if 1, go to q4, chenge 1 to 'e
   (ins 'q2 'e 'q2 'e 'L)
   (ins 'q2 'c 'q2 'c 'L)
   (ins 'q2 'z 'q2 'z 'L)
   (ins 'q2 'n 'q2 'n 'L)
   (ins 'q2 'b 'q99 'b 'R)

   ; q3 skips over everything but b, which it changes to z = zero
   (ins 'q3 'e 'q3 'e 'R) 
   (ins 'q3 'c 'q3 'c 'R)
   (ins 'q3 'n 'q3 'n 'R)
   (ins 'q3 'z 'q3 'z 'R)
   (ins 'q3 'b 'q2 'z 'L) ; if blank, changes to z(ero) then returns to q2

   ; q4 skips over everything but b, which it changes to n = one
   (ins 'q4 'e 'q4 'e 'R)
   (ins 'q4 'c 'q4 'c 'R)
   (ins 'q4 'n 'q4 'n 'R)
   (ins 'q4 'z 'q4 'z 'R)
   (ins 'q4 'b 'q2 'n 'L) ; if blank, change to n (one), then return to q2

   ; q99 means there are no ore numbers to be copied, and will start to change e's back to b's, and turns z's to 0 and n's to 1 till reach b
   (ins 'q99 'e 'q99 'b 'R)
   (ins 'q99 'c 'q99 'b 'R)
   (ins 'q99 'z 'q99 0 'R) ; z -> 0
   (ins 'q99 'n 'q99 1 'R) ; n -> 1
   (ins 'q99 'b 'q100 'b 'L)

   ; q100 returns to left most number 
   (ins 'q100 1 'q100 1 'L)
   (ins 'q100 0 'q100 0 'L)
   (ins 'q100 'b 'q101 'b 'R)))


; ****************************************************************
; ** problem 2 (10 points)
; Write the following two procedures.
; Remember to use the instruction selectors:
; ins-c-state, ins-c-symbol, ins-n-state, ins-n-symbol, ins-dir

; (i-match? state symbol inst)
; returns #t if state and symbol are equal to 
; the state and symbol of instruction inst
; otherwise returns #f

; (i-lookup state symbol mach)
; returns #f if no instruction of Turing machine mach 
; has state and symbol equal to state and symbol
; otherwise returns the instruction in mach that matches.
; You may assume that at most one instruction will match.

; Examples
; (i-match? 'q1 'b (ins 'q1 'b 'q3 'b 'L)) => #t
; (i-match? 'q1  0  (ins 'q1 1 'q4 1 'L)) => #f
; (i-match? 'q2 1 (ins 'q2 1 'q2 1 'L)) => #t
; (equal? (i-lookup 'q1 1 tm1) (ins 'q1 1 'q1 0 'R)) => #t
; (equal? (i-lookup 'q2 'b tm1) (ins 'q2 'b 'q3 'b 'R)) => #t
; (i-lookup 'q3 1 tm1) => #f
; ****************************************************************

(define (i-match? state symbol inst)
  (if (and (equal? state (ins-c-state inst)) (equal? symbol (ins-c-symbol inst))) ; if the states match and the symbols match, true
           #t
           #f))

(define (i-lookup state symbol mach)
  (if (empty? mach) ; if reaches empty and haven't found, return false
      #f
      (if (i-match? state symbol (car mach)) ; if found , return that instruction (car mach)
          (car mach)
          (i-lookup state symbol (cdr mach)))))


; ****************************************************************
; Representation of a Turing machine configuration.
; We represent a Turing machine configuration using the following structure:

(struct conf (state ltape symbol rtape) #:transparent)

; where state is the current state of the machine,
; ltape is a list of symbols to the left of the currently scanned symbol,
; symbol is the currently scanned symbol,
; rtape is a list of symbols to the right of the currently scanned symbol.

; We reserve the symbol 'b for the blank.

; For example, we define the following two configurations:

(define config1 (conf 'q3 '(0 0) 1 '(1)))
(define config2 (conf 'q6 '(1 b) 0 '(b b)))

; Note that the selectors are
; conf-state, conf-ltape, conf-symbol, conf-rtape

; config1 represents the Turing machine configuration

;   --------------------------
;   .. | 0 | 0 | 1 | 1 |  | ..
;   --------------------------
;                ^
;                q3

; in which the non-blank symbols on the tape are 0011,
; and the machine is in state q3 with the read/write head
; scanning the leftmost 1.

; config2 represents the Turing machine configuration

;   ------------------------------
;   .. |   | 1 |  | 0 |   |   | ..
;   ------------------------------
;                   ^
;                   q6

; in which the symbols 1, blank, 0, are on the tape, surrounded
; by blanks, and the machine is in state q6 with the read/write
; head scanning the 0.

; A configuration is *normalized* if neither the first symbol of
; ltape nor the last symbol of rtape is the symbol 'b.
; Of the two configurations above, config1 is normalized, 
; but config2 is not (the last element of its rtape list is 'b.)

; Note that tape squares not explicitly represented are
; assumed to contain blanks.  A normalized configuration
; to represent the machine in state q1 with all tape squares
; blank is thus (conf 'q1 '() 'b '())).

; ****************************************************************
; ** problem 3 (9 points)
; Write the following three procedures.

; (halted? mach config)
; returns #t if the Turing machine mach is halted in machine configuration config 
; (ie, no instruction of the machine matches the current state and symbol 
; in configuration config) and returns #f otherwise.

; (change-state new-state config)
; takes a configuration config and returns a configuration
; in which the state of the machine is changed to new-state.

; (write-symbol new-symbol config) takes a configuration config and
; returns a configuration in which the symbol scanned by 
; the read/write head has been replaced by new-symbol.

; Examples
; (halted? tm1 (conf 'q1 '(1 1 0) 'b '())) => #f
; (halted? (list (ins 'q1 'b 'q2 'b 'R)) (conf 'q2 '() 'b '())) => #t
; (change-state 'q2 (conf 'q1 '(0) 1 '())) => (conf 'q2 '(0) 1 '())
; (change-state 'q13 (conf 'q4 '(0 1 1) 'b '())) => (conf 'q13 '(0 1 1) 'b '())
; (write-symbol 1 (conf 'q5 '(0) 0 '(1 1))) => (conf 'q5 '(0) 1 '(1 1))
; (write-symbol 'c (conf 'q2 '(0 0 1) 1 '(1 1))) => (conf 'q2 '(0 0 1) 'c '(1 1))
; (write-symbol 'b (conf 'q3 '(1) 0 '())) => (conf 'q3 '(1) 'b '())
; ****************************************************************

(define (halted? mach config) ; look up state
  (if (equal? #f (i-lookup (conf-state config) (conf-symbol config) mach)) ; if no instruction for specific state, then halted
      #t
      #f))

(define (change-state state config)
  (conf state (conf-ltape config) (conf-symbol config) (conf-rtape config))) ; keep everything same but new state

(define (write-symbol new-symbol config)
  (conf (conf-state config) (conf-ltape config) new-symbol (conf-rtape config))); keep everything same except new symbol

; ****************************************************************
; ** problem 4 ** (10 points)
; Write one procedure

; (normalize config)
; takes a Turing machine configuration config and returns an equivalent 
; *normalized* configuration. That is, the same Turing machine configuration is
; represented by the input configuration and the output configuration, 
; and the output configuration does not have a 'b as the first element 
; of its ltape list or the last element of its rtape list.

; Examples
; (normalize config1) => (conf 'q3 '(0 0) 1 '(1))
; (normalize config2) => (conf 'q6 '(1 b) 0 '())
; (normalize (conf 'q3 '(b 0) 'b '(1 1 0 b b))) => (conf 'q3 '(0) 'b '(1 1 0))
; (normalize (conf 'q6 '(b 0 b 0) 1 '(0 b 0 b))) => (conf 'q6 '(0 b 0) 1 '(0 b 0))
; (normalize (conf 'q4 '(b b) 'b '(b b b))) => (conf 'q4 '() 'b '())
; ****************************************************************


(define (normalize config)
  (if (and (not (empty? (conf-ltape config)))(equal? 'b (car (conf-ltape config)))) ; if l-tape not empty and car = b
      (normalize (conf (conf-state config) (cdr (conf-ltape config)) (conf-symbol config) (conf-rtape config))) ; call recursively till front of ltape isn't b
      (if (and (not (empty? (conf-rtape config)))(equal? 'b (last (conf-rtape config)))) ; if rtape not empty end of rtape = b
          (normalize (conf (conf-state config) (conf-ltape config) (conf-symbol config) (reverse (cdr (reverse (conf-rtape config)))))) ; remove last element of r-tape
          config)))
      

; ****************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (shift-head-left config)
; takes a normalized configuration config and returns a normalized configuration 
; in which the position of the read/write head has been moved one tape square 
; to the left.

; (shift-head-right config)
; takes a normalized configuration config and returns a normalized configuration 
; in which the position of the read/write head has been moved one tape square 
; to the right.

; Examples
; (shift-head-left (conf 'q5 '() 'b '())) => (conf 'q5 '() 'b '())
; (shift-head-left (conf 'q6 '(0 0) 1 '(1 1))) => (conf 'q6 '(0) 0 '(1 1 1))
; (shift-head-left (conf 'q7 '() 0 '(1 1 0))) => (conf 'q7 '() 'b '(0 1 1 0))
; (shift-head-right (conf 'q2 '() 'b '())) => (conf 'q2 '() 'b '())
; (shift-head-right (conf 'q9 '() 0 '(1 1 1))) => (conf 'q9 '(0) 1 '(1 1))
; (shift-head-right (conf 'q8 '(1 0 1 1) 'b '())) => (conf 'q8 '(1 0 1 1 b) 'b '())
; ****************************************************************
;(struct conf (state ltape symbol rtape) #:transparent)
; last element of a list is built in to Racket (last lst)

; all but last element of a list -- uses Racket's drop-right


(define (shift-head-left config)
  (if (empty? (conf-ltape config))
      (normalize (conf (conf-state config) '() 'b (append (list (conf-symbol config)) (conf-rtape config)))) ; normalize empty list, new symbol b, append symbol to front of rtape
      (normalize (conf (conf-state config) (reverse (cdr (reverse (conf-ltape config)))) (last (conf-ltape config)) (append (list (conf-symbol config)) (conf-rtape config))))))
; fancy reverse thing = just cutting last element off left, new symbol = last of left, then adding old symbol to front of right

(define (shift-head-right config)
  (if (empty? (conf-rtape config))
      (normalize (conf (conf-state config) (append (conf-ltape config) (list (conf-symbol config))) 'b '())) ; add current symbol to end of left, then new symbol = b and rtape just empty
      (normalize (conf (conf-state config) (append (conf-ltape config) (list (conf-symbol config))) (car (conf-rtape config)) (cdr (conf-rtape config)))))) ; same as above, but new symbol = car of rtape

; ****************************************************************
; ** problem 6 ** (15 points)
; Write a procedure 

; (next-config mach config)
; takes a Turing machine mach and a normalized configuration config
; and returns the normalized next configuration 
; for the Turing machine mach in the configuration config.
; If there is no applicable instruction, the configuration
; returned should be just the input configuration.

; Hint: get your procedures
; halted?, i-lookup, write-symbol, shift-head-left, shift-head-right
; working and combine them appropriately.

; Examples
; (next-config tm1 (conf 'q1 '() 0 '(0 1))) => (conf 'q1 '(1) 0 '(1))
; (next-config tm1 (conf 'q1 '(1) 0 '(1))) => (conf 'q1 '(1 1) 1 '())
; (next-config tm1 (conf 'q1 '(1 1 0) 'b '())) => (conf 'q2 '(1 1) 0 '())
; (next-config tm1 (conf 'q2 '() 'b '(1 1 0))) => (conf 'q3 '() 1 '(1 0))
; (next-config tm1 (conf 'q3 '() 1 '(1 0))) => (conf 'q3 '() 1 '(1 0))
; ****************************************************************
(define (next-config mach config)
  (if (halted? mach config) ; if halted, then return config
      config
      (let* ([newins (i-lookup (conf-state config) (conf-symbol config) mach)] ; new instruction, basically the instruction with same state and symbol in machine
             [newconf (write-symbol (ins-n-symbol newins) (change-state (ins-n-state newins) config))]) ; new configuration, change state to new state of newins, and change symbol to new symbol of newins
        (if (equal? 'R (ins-dir newins)) ; if instruction says go right, go right 
            (shift-head-right newconf)
            (shift-head-left newconf))))); else go left
      
;(struct ins (c-state c-symbol n-state n-symbol dir) #:transparent) 

#|(define tm1 
  (list
   (ins 'q1 0 'q1 1 'R)
   (ins 'q1 1 'q1 0 'R)
   (ins 'q1 'b 'q2 'b 'L)
   (ins 'q2 0 'q2 0 'L)
   (ins 'q2 1 'q2 1 'L)
   (ins 'q2 'b 'q3 'b 'R)))|#

; ****************************************************************
; If your procedures are working, then you should
; be able to run the following example, which
; shows the successive normalized configurations 
; of Turing machine tm1 when run from the given configuration.

;> (simulate tm1 (conf 'q1 '() 1 '(1 0 1 0)) 20)
;(list
; (conf 'q1 '() 1 '(1 0 1 0))
; (conf 'q1 '(0) 1 '(0 1 0))
; (conf 'q1 '(0 0) 0 '(1 0))
; (conf 'q1 '(0 0 1) 1 '(0))
; (conf 'q1 '(0 0 1 0) 0 '())
; (conf 'q1 '(0 0 1 0 1) 'b '())
; (conf 'q2 '(0 0 1 0) 1 '())
; (conf 'q2 '(0 0 1) 0 '(1))
; (conf 'q2 '(0 0) 1 '(0 1))
; (conf 'q2 '(0) 0 '(1 0 1))
; (conf 'q2 '() 0 '(0 1 0 1))
; (conf 'q2 '() 'b '(0 0 1 0 1))
; (conf 'q3 '() 0 '(0 1 0 1)))

; ****************************************************************
; ** problem 7 ** (15 points)
; Define (in the given representation) a Turing machine named

; tm-convert

; that takes as input a positive integer n represented in binary
; and produces as output a string of n x's.  When the machine
; halts, the read/write head should be positioned over the
; leftmost x in the output string.  The start state should be named
; q1 -- other states may be named by any other Racket symbols.

; You *may* use additional tape symbols.  When the machine halts,
; there should be just n x's, surrounded by blanks, on the tape.

; IMPORTANT: Give a clear overview description of how your Turing machine works.

; NOTE: you can still do this problem if your simulator is not working, 
; assuming you understand Turing machines and the representation of them 
; defined above.

; Examples of the behavior of tm-convert
; 1            => x
; 110          => xxxxxx
; 1111         => xxxxxxxxxxxxxxx

; Here are input configurations if you want to simulate your tm-convert on
; these inputs.

(define conv1 (conf 'q1 '() 1 '()))
(define conv6 (conf 'q1 '() 1 '(1 0)))
(define conv15 (conf 'q1 '() 1 '(1 1 1)))
;(simulate tm-convert conv1 20)

; ****************************************************************

(define tm-convert
  (list
    ;q1 places c
   (ins 'q1 0 'q1 0 'R)
   (ins 'q1 1 'q1 1 'R)
   (ins 'q1 'b 'q2 'c 'L)

   ;q2 go left till finds numbers
   (ins 'q2 'c 'q2 'c 'L) ; skip c 
   (ins 'q2 'x 'q2 'x 'L) ; skip x
   (ins 'q2 1 'q3 0 'R) ; if one, change to 0, then q3
   (ins 'q2 0 'q4 'z 'L) ; if x, change to z, then q4

   ;q3 goes right till finding b, then place x
   (ins 'q3 1 'q3 1 'R)
   (ins 'q3 0 'q3 0 'R)
   (ins 'q3 'x 'q3 'x 'R)
   (ins 'q3 'c 'q3 'c 'R)
   (ins 'q3 'b 'q2 'x 'L) ; place x then return to q2 to find more numbers
   
   ;q4 basically keeps left till finding a 1 or blank bc will be passed a 0, and needs to find closest one to subtract one from
   (ins 'q4 0 'q4 0 'L)
   (ins 'q4 1 'q5 0 'R) ; this one becomes a 0, then q5
   (ins 'q4 'b 'q99 'b 'R) ; if find blank, means no more 1's, and therefore no more x's
   
   ;q5 goes right till finding the z placed before, changing all 0's to 1's on it's way
   (ins 'q5 0 'q5 1 'R) ; change 0's to 1's because binary subtraction, got rid of 1 so have to redistribute
   (ins 'q5 'z 'q3 1 'R) ; once finding z, change to 1 (binary subtract 1) and go back to q3

   ;q99 means finished, will be at left most b, so keep going right turning everything to b till reaching an x (halting)
   (ins 'q99 0 'q99 'b 'R)
   (ins 'q99 'z 'q99 'b 'R)
   (ins 'q99 'c 'q99 'b 'R)))

; ****************************************************************
; ** problem 8 ** (15 points)
; Define (in the given representation) a Turing machine named

; tm-sort

; that takes as input a non-empty string of 0's and 1's
; and produces as output a string of 0's and 1's equal to the input
; string rearranged to have all the 0's before all the 1's.
; When the machine halts, the read/write head should be positioned over the
; leftmost 0 or 1 in the output string.  The start state should be named
; q1 -- other states may be named by any other Racket symbols.

; You *may* use additional tape symbols.  When the machine halts,
; the only non-blank symbols on the tape should be the output string.

; IMPORTANT: Give a clear overview description of how your Turing machine works.

; NOTE: you can still do this problem if your simulator is not working, 
; assuming you understand Turing machines and the representation of them 
; defined above.

; Examples of the behavior of tm-sort
; 0          => 0  
; 1          => 1
; 00         => 00
; 110        => 011
; 1011011    => 0011111

; Here are some input configurations if you want to simulate your tm-sort on
; these inputs.

(define sort0 (conf 'q1 '() 0 '()))
(define sort1 (conf 'q1 '() 1 '()))
(define sort00 (conf 'q1 '() 0 '(0)))
(define sort110 (conf 'q1 '() 1 '(1 0)))
(define sort-long (conf 'q1 '() 1 '(0 1 1 0 1 1)))
; ****************************************************************

(define tm-sort
  (list
   (ins 'q1 0 'q1 0 'R)
   (ins 'q1 1 'q1 1 'R)
   (ins 'q1 'b 'q2 'c 'L)

   ;q2 keeps going left till finding 0 or b
   (ins 'q2 1 'q2 1 'L)
   (ins 'q2 'c 'q2 'c 'L)
   (ins 'q2 'e 'q2 'e 'L)
   (ins 'q2 'z 'q2 'z 'L)
   (ins 'q2 'n 'q2 'n 'L)
   (ins 'q2 'b 'q4 'b 'R) ; if reaches blank, all zeros gone, start looking for 1's
   (ins 'q2 0 'q3 'e 'R) ; is reaches 0, set as e and go to q3

   ;q3 moves right till finding the rightmost 'b, then places 'z (for 0) there
   (ins 'q3 1 'q3 1 'R)
   (ins 'q3 'c 'q3 'c 'R)
   (ins 'q3 'e 'q3 'e 'R)
   (ins 'q3 'z 'q3 'z 'R)
   (ins 'q3 'b 'q2 'z 'L) ; once places 0, go left back to q2

   ;q4 starts finding leftmost 1, or if reaches b (no more ones), goes to q99, all zeros have been placed
   (ins 'q4 'e 'q4 'e 'R)
   (ins 'q4 'z 'q4 'z 'R)
   (ins 'q4 'n 'q4 'n 'R)
   (ins 'q4 'c 'q4 'c 'R)
   (ins 'q4 'b 'q99 'b 'L) ; if reach b (rightmost), head back left, no more numbers to convert
   (ins 'q4 1 'q5 'e 'R) ; if find 1, change to e and keep heading right

   ;q5 places 1's as n's
   (ins 'q5 'c 'q5 'c 'R)
   (ins 'q5 'e 'q5 'e 'R)
   (ins 'q5 'z 'q5 'z 'R)
   (ins 'q5 'n 'q5 'n 'R)
   (ins 'q5 1 'q5 1 'R)
   (ins 'q5 'b 'q2 'n 'L) ; once finished placine n, do q2 again and head back to left of list

   ;q99 starts rightmost, and heads left converting symbols to numbers or blanks
   (ins 'q99 'e 'q99 'b 'L)
   (ins 'q99 'c 'q99 'b 'L)
   (ins 'q99 'n 'q99 1 'L)
   (ins 'q99 'z 'q99 0 'L)
   (ins 'q99 'b 'q100 'b 'R)

   ;q100 heads back till reaches left most number, then halts!
   (ins 'q100 'b 'q100 'b 'R)))

;(simulate tm-reverse (conf 'q1 '() 1 '(1 0)) 100)

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

(test 'i-match? (i-match? 'q1 'b (ins 'q1 'b 'q3 'b 'L)) #t)
(test 'i-match? (i-match? 'q1  0  (ins 'q1 1 'q4 1 'L)) #f)
(test 'i-match? (i-match? 'q2 1 (ins 'q2 1 'q2 1 'L)) #t)
(test 'i-lookup (i-lookup 'q1 1 tm1) (ins 'q1 1 'q1 0 'R))
(test 'i-lookup (i-lookup 'q2 'b tm1) (ins 'q2 'b 'q3 'b 'R))
(test 'i-lookup (i-lookup 'q3 1 tm1) #f)

(test 'halted? (halted? tm1 (conf 'q1 '(1 1 0) 'b '())) #f)
(test 'halted? (halted? (list (ins 'q1 'b 'q2 'b 'R)) (conf 'q2 '() 'b '())) #t)
(test 'change-state (change-state 'q2 (conf 'q1 '(0) 1 '())) (conf 'q2 '(0) 1 '()))
(test 'change-state (change-state 'q13 (conf 'q4 '(0 1 1) 'b '())) (conf 'q13 '(0 1 1) 'b '()))
(test 'write-symbol (write-symbol 1 (conf 'q5 '(0) 0 '(1 1))) (conf 'q5 '(0) 1 '(1 1)))
(test 'write-symbol (write-symbol 'c (conf 'q2 '(0 0 1) 1 '(1 1))) (conf 'q2 '(0 0 1) 'c '(1 1)))
(test 'write-symbol (write-symbol 'b (conf 'q3 '(1) 0 '())) (conf 'q3 '(1) 'b '()))

(test 'normalize (normalize config1) (conf 'q3 '(0 0) 1 '(1)))
(test 'normalize (normalize config2) (conf 'q6 '(1 b) 0 '()))
(test 'normalize (normalize (conf 'q3 '(b 0) 'b '(1 1 0 b b))) (conf 'q3 '(0) 'b '(1 1 0)))
(test 'normalize (normalize (conf 'q6 '(b 0 b 0) 1 '(0 b 0 b))) (conf 'q6 '(0 b 0) 1 '(0 b 0)))
(test 'normalize (normalize (conf 'q4 '(b b) 'b '(b b b))) (conf 'q4 '() 'b '()))


(test 'shift-head-left (shift-head-left (conf 'q5 '() 'b '())) (conf 'q5 '() 'b '()))
(test 'shift-head-left (shift-head-left (conf 'q6 '(0 0) 1 '(1 1))) (conf 'q6 '(0) 0 '(1 1 1)))
(test 'shift-head-left (shift-head-left (conf 'q7 '() 0 '(1 1 0))) (conf 'q7 '() 'b '(0 1 1 0)))
(test 'shift-head-right (shift-head-right (conf 'q2 '() 'b '())) (conf 'q2 '() 'b '()))
(test 'shift-head-right (shift-head-right (conf 'q9 '() 0 '(1 1 1))) (conf 'q9 '(0) 1 '(1 1)))
(test 'shift-head-right (shift-head-right (conf 'q8 '(1 0 1 1) 'b '())) (conf 'q8 '(1 0 1 1 b) 'b '()))


(test 'next-config (next-config tm1 (conf 'q1 '() 0 '(0 1))) (conf 'q1 '(1) 0 '(1)))
(test 'next-config (next-config tm1 (conf 'q1 '(1) 0 '(1))) (conf 'q1 '(1 1) 1 '()))
(test 'next-config (next-config tm1 (conf 'q1 '(1 1 0) 'b '())) (conf 'q2 '(1 1) 0 '()))
(test 'next-config (next-config tm1 (conf 'q2 '() 'b '(1 1 0))) (conf 'q3 '() 1 '(1 0)))
(test 'next-config (next-config tm1 (conf 'q3 '() 1 '(1 0))) (conf 'q3 '() 1 '(1 0)))


; *************** end of hw3.rkt *********************************


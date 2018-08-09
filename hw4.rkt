#lang racket

(provide 
         entry entry-key entry-value entry?
         bnot bnot-arg bnot?
         band band-arg1 band-arg2 band?
         bor bor-arg1 bor-arg2 bor?
         tt tt-vars tt-rows tt?
	 hours
         lookup unique-keys?
         boolean-exp? type-of
	 all-vars
	 eval-in-env
	 all-combs
	 truth-table	 	 	 
	 satisfiable? equivalent?
	 find-exp
         substitute-in
         match)

; Please do not modify lines above this one.

; ****************************************************************
; CS 201b HW #4  DUE 11:59 pm Sunday, October 22, 2017
; using the submit command on the Zoo.
; ****************************************************************
; Name: Kenia Hale
; Email address: kenia.hale@yale.edu
; ****************************************************************
; ** problem 0 ** (1 easy point)
; Please modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 10)

; ****************************************************************
; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Racket constructs, except mutators (set! and its relatives.)
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

; The topics of this assignment are as follows.
; Racket: deep recursion on a recursively defined data structure.
; Computer Science: Boolean functions, expressions, environments,
; truth tables, satisfiability, equivalence.

; You might also find uses for the special form: case.

; ****************************************************************
; We define a table as a list of entries,
; where each entry is given by the following structure.

(struct entry (key value) #:transparent); to call, (entry-key name)

; Recall that a struct defines a constructor, selector(s), and a type predicate.
; In this case, the constructor is entry, the selectors are
; entry-key and entry-value, and the type predicate is entry?.

; Here are two examples of tables.

(define test1
  (list
   (entry "second" 2) ; key = second, value = 2
   (entry "first" 1)
   (entry "fifth" 5)))

(define test2
  (list
   (entry 'x 0)
   (entry 'z 1)
   (entry 'y 1)
   (entry 'z 0)))

; ****************************************************************
; ** problem 1 ** (9 points)
; Write two procedures to deal with tables as follows.

; (lookup key table)
; (unique-keys? table)

; (lookup key table)
; returns #f if no entry in the table has a key equal? to key
; otherwise, returns the value of the first entry whose key is equal? to key.

; (unique-keys? table)
; returns #t if all the entries of table have keys that are pairwise
; not equal? and #f otherwise.

; Examples
;> (lookup "first" test1)
;1
;> (lookup "third" test1)
;#f
;> (lookup 'z test2)
;1
;> (unique-keys? test1)
;#t
;> (unique-keys? test2)
;#f
;>
; ****************************************************************

(define (lookup key table)
  (cond
    [(empty? table) #f]
    [(equal? key (entry-key (car table)))(entry-value (car table))] ; will return that value if keys equal
    [else (lookup key (cdr table))])); else, run on cdr

(define (unique-keys? table)
  (if (not (check-duplicates (list-keys table '()))) ; checks if any duplicates in a list of the keys
      #t
      #f))

(define (list-keys table newtable); returns a list of keys within a table
  (if (empty? table)
      newtable
      (list-keys (cdr table) (cons (entry-key (car table)) newtable))))

; ****************************************************************
; Our representation of Boolean expressions will use the following
; struct definitions.  Note that the transparent attribute allows
; the contents of a structure to be printed out, and allows two
; structures to be compared for content using equal?

(struct bnot (arg) #:transparent)
(struct band (arg1 arg2) #:transparent)
(struct bor (arg1 arg2) #:transparent)

; We recursively define a representation of Boolean expressions as follows.

; 1) 0 and 1 represent the constants 0 and 1
; 2) Racket symbols represent variables (for example, 'x, 'y, 'z2)
; 3) If exp1 and exp2 represent Boolean expressions, then
;    (bnot exp1) represents the NOT of exp1
;    (band exp1 exp2) represents the AND of exp1 and exp2
;    (bor exp1 exp2) represents the OR of exp1 and exp2

; Some examples of Boolean expressions follow.

(define exp0 (bnot 0))
(define exp1 (bor 'x 'y))
(define exp2 (band 'y 'z))
(define exp3 (band 'w (bor (bnot 'x) 0)))
(define exp4 (bor 'x (bnot 'x)))
(define exp5 (band (bor 'x (bnot 'y)) (bnot (band 0 'z))))

; ****************************************************************
; ** problem 2 ** (10 points)
; Write two procedures

; (boolean-exp? exp)
; (type-of exp)

; (boolean-exp? exp) takes an arbitrary Racket value exp
; and tests to see whether it is a Boolean expression according
; to the definition above, returning #t if so and #f if not.

; (type-of exp)
; that takes a Boolean expression as defined above
; and returns its type as one of the symbols:
;   constant, variable, not, or, and
; Note that the type is determined by the top-level
; operation in case the expression is not a constant or variable.

; Recall that (number? exp) tests whether exp is a number,
; and (symbol? exp) tests whether exp is a symbol, and the
; structs bnot, band, and bor have type predicates bnot?, band?, and bor?

; Examples
;> (boolean-exp? 0)
;#t
;> (boolean-exp? 2)
;#f
;> (boolean-exp? exp0)
;#t
;> (boolean-exp? (band "hi" 'c))
;#f
;> (boolean-exp? (band 'x (bor 0 (bnot 1))))
;#t
;> (boolean-exp? (band 'x (bor 0 (bnot #t))))
;#f
;> (type-of 0)
;'constant
;> (type-of 'hi)
;'variable
;> (type-of (bnot (band 'x 0)))
;'not
;> (type-of (bor (band 'x 'y) (band 'x 'z)))
;'or
;> (type-of (band (bor 0 1) (bnot 'x)))
;'and
; ****************************************************************

(define (boolean-exp? exp)
  (cond
    [(not (bool-var? exp))#f] ; if doesn't abide by types allowed as bool, then false
    [(bnot? exp)(boolean-exp? (bnot-arg exp))] ; if type bnot, check if the arg of bnot is also boolean exp
    [(bor? exp)(and (boolean-exp? (bor-arg1 exp))(boolean-exp? (bor-arg2 exp)))] ; if type bor, check if both arguments also count as bool exps
    [(band? exp)(and (boolean-exp? (band-arg1 exp))(boolean-exp? (band-arg2 exp)))] ; same as above
    [else #t]))

(define (bool-var? v) ; determines if v is any of the acceptable types within a boolean-function
  (cond
    [(and (number? v)(or (equal? v 1)(equal? v 0))) #t] ; number and equals 1 or 0
    [(symbol? v) #t]
    [(or (bor? v)(band? v)(bnot? v)) #t]
    [else #f]))

(define (type-of exp)
  (cond
    [(number? exp) 'constant] ; number and equals 1 or 0
    [(symbol? exp) 'variable]
    [(bor? exp) 'or]
    [(band? exp) 'and]
    [else 'not]))
;constant, variable, not, or, and

; ****************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (all-vars exp)

; that takes a Boolean expression exp 
; and makes a list containing all the variables
; that occur in exp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in exp (scanning left to right.)

; Hint: selectors, type-of and deep recursion on the structure 
; of Boolean expressions.  Note that there is a Racket procedure
; remove-duplicates.

; Examples
;> (all-vars 0)
;'()
;> (all-vars (bnot (band 'x (band 'y (bor 'x 'z)))))
;'(x y z)
;> (all-vars (band 1 (bor 0 (bnot 'u))))
;'(u)
;> (all-vars (band (band 'x 'y) (band 'y 'x)))
;'(x y)
;> (all-vars (bor (bor (bor 'c 'b) (bor 'a 'b)) 'c))
;'(c b a)
;> 
; ****************************************************************

(define (all-vars exp)
  (remove-duplicates (all-vars-lst exp '())))

(define (all-vars-lst exp lst); recursively parses through and returns a list of symbols
  (cond
    [(bnot? exp) ; if not
     (append (all-vars (bnot-arg exp)) lst)] ; run again, but append the arg into lst
    [(band? exp)
     (append (all-vars (band-arg1 exp))(all-vars (band-arg2 exp)) lst)] ; append arg1 arg2 and lst
    [(bor? exp)
     (append (all-vars (bor-arg1 exp))(all-vars (bor-arg2 exp)) lst)]; same as above
    [(symbol? exp) (list exp)]; if a symbol, return a list of it so can be appended
    [else lst])); if at end, return the lst

; ****************************************************************
; We represent an environment as a table each entry of which
; has a key that is a Racket symbol and a value that is 0 or 1,
; which specifies the truth value of that variable in the environment.
; For example:

(define environ1
  (list
   (entry 'x 0) (entry 'y 1) (entry 'z 0)))
  
(define environ2
  (list
   (entry 'u 0) (entry 'x 1) (entry 'w 1) (entry 'y 0) (entry 'z 1)))

; ****************************************************************
; ** problem 4 ** (10 points)
; Write a procedure 

; (eval-in-env exp env)

; that takes a Boolean expression exp and an environment env
; (represented as described above) and returns 0 or 1 giving 
; the value of the expression in the environment.

; If the Boolean expression contains variables that do not
; occur in the environment, (eval-in-env exp env) should
; return the symbol: 'unspecified-variable.
; (You may want to check for this condition first.)

; Hint: deep recursion on the structure of Boolean expressions
; along with argument selectors and type-of.

; Examples
;> (eval-in-env 1 environ1)
;1
;> (eval-in-env (bor 0 0) '())
;0
;> (eval-in-env 'x environ1)
;0
;> (eval-in-env 'x environ2)
;1
;> (eval-in-env (bnot 'z) environ1)
;1
;> (eval-in-env (bor 'y (bnot 'x)) environ2)
;0
;> (eval-in-env (band (band (bor 'u 'x) (bor 'w 0)) (bnot (band 'y 'z))) environ2)
;1
;> (eval-in-env exp5 environ1)
;0
;> (eval-in-env (band 'y (bor 'x 'u)) (list (entry 'x 0) (entry 'y 1)))
;'unspecified-variable
;>
; ****************************************************************

(define (eval-in-env exp env)
  (cond
    [(not(contains-vars? (all-vars exp) env))'unspecified-variable]; if all vars in exp aren't in environment, then show error
    [(symbol? exp)(lookup exp env)] ; if it's a symbol, return the value within the environment
    [(number? exp) exp]; if number, return it's self
    [(bor? exp)(bool-translate(or (bool-translate (eval-in-env (bor-arg1 exp) env))(bool-translate (eval-in-env (bor-arg2 exp) env))))];recusrive call, translate to #t/#f to racket 
    [(band? exp)(bool-translate(and (bool-translate (eval-in-env (band-arg1 exp) env))(bool-translate (eval-in-env (band-arg2 exp) env))))];understands, then to  #t/#f so boolean exp
    [(bnot? exp)(bool-translate(not (bool-translate (eval-in-env (bnot-arg exp) env))))]
    [else 'unspecified-variable])) ; if environment is blank, won't have in it)); not
    

(define (bool-translate b) ; converts between one's and zero's and #t and #f
  (cond
    [(equal? #t b) 1]
    [(equal? #f b) 0]
    [(equal? 1 b) #t]
    [else #f])) ; if 0

(define (contains-vars? expvarlst env) ; tells if all the variables in exp are within env
  (cond
    [(empty? expvarlst)#t] ; if reaches the end, then contains all
    [(boolean? (lookup (car expvarlst) env))#f] ; 
    [else (contains-vars? (cdr expvarlst) env)]))
; ****************************************************************
; We define a truth table as represented by the following struct

(struct tt (vars rows) #:transparent)

; whose fields contain the following
; (1) a (possibly empty) list of n distinct variables, and
; (2) a table containing an entry for each combination of n 0's and 1's,
; whose key is a list of n 0's and 1's and whose value is the value (0 or 1)
; of the function represented by the truth table.

; Note that the entries in a truth table should be in increasing order of
; their keys, considered as binary numbers.

; Examples of truth tables for the expressions
; (bnot 'x), (band 'x 'y), (bor (bnot 'a) 'b) 
; and the function that is the XOR of 'u and 'v.

(define tt-not (tt '(x)
                   (list
                    (entry '(0) 1)
                    (entry '(1) 0))))

(define tt-and (tt '(x y)
                   (list 
                    (entry '(0 0) 0)
                    (entry '(0 1) 0)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
                    
 
(define tt-imp (tt '(a b)
                   (list
                    (entry '(0 0) 1)
                    (entry '(0 1) 1)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
  
(define tt-xor (tt '(u v)
                   (list
                    (entry '(0 0) 0)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))

; Here is a truth table for a function of three arguments a, b, c.

(define tt-f1 (tt '(a b c)
                  (list
                   (entry '(0 0 0) 0)
                   (entry '(0 0 1) 0)
                   (entry '(0 1 0) 1)
                   (entry '(0 1 1) 1)
                   (entry '(1 0 0) 0)
                   (entry '(1 0 1) 1)
                   (entry '(1 1 0) 0)
                   (entry '(1 1 1) 1))))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (all-combs n)

; that takes a non-negative integer n and creates the list of all 
; lists of n 0's or 1's in the *specific order* required for
; a truth table.  In other words, the lists, interpreted as binary numbers, 
; should be in increasing order.

; Hint: if a recursive call gives the correct answer
; for (all-combs 2), what needs to happen to it
; to give the correct answer for (all-combs 3)?
; (This may remind you of power-set from assignment #1.)

; Use let or let* to avoid recomputing the recursive call!

; Examples
;> (all-combs 0)
;'(())
;> (all-combs 1) <-- 2 values, 2^1 (0-1)
;'((0) (1))
;> (all-combs 2) <-- 4 values, 2^2 (0-3)
;'((0 0) (0 1) (1 0) (1 1))
;> (all-combs 3)
;'((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1)) <-- 8 values, (0-7)
;> 
; ****************************************************************

(define (all-combs n)
  (let ([x (all-combs-lst (- (expt 2 n) 1))]) ; so don't have to write it twice, basically listing all binary numbers 0 to (2^n -1)
      (if (equal? 0 n)
          (list '())
          (add-zeros (length (last x)) x))))

(define (all-combs-lst n) ; returns a list of all the binary reprisentations needed
  (if (equal? n 0)
      (dec2bin 0)
      (append (all-combs-lst (- n 1))(list (dec2bin n)))))

(define (add-zeros longlen lst) ; will add the zeros if not long enough, basically checking if length of car = longest length in list
    (cond
      [(empty? lst)'()]
      [(equal? longlen (length (car lst)))(cons (car lst)(add-zeros longlen (cdr lst)))]; if car has enough zeros, cons to recursive call on rest of list
      [else (add-zeros longlen (cons (cons 0 (car lst)) (cdr lst)))])) ;keep caling self again, till car has enough zeros at front

(define (dec2bin n)
  (mydec2bin n '()))

(define (mydec2bin num lst) ; returns single number represented by binary list
  (cond
    [(= num 0)(cons (list 0) lst)]
    [(= num 1)(append '(1) lst)]
    [else (mydec2bin (quotient num 2)(append (list (remainder num 2)) lst))]
      ))


; ****************************************************************
; ** problem 6 ** (10 points)
; Write a procedure

; (truth-table exp)

; that takes a Boolean expression exp and returns the truth table for exp
; where the variables for the table are extracted from exp using all-vars, 
; and the function value for each row is obtained by evaluating exp 
; in the corresponding environment.  Notice that all-vars specifies
; the order of variables for the truth table.

; Examples:
;> (truth-table exp0)
;(tt '() (list (entry '() 1)))
;> (truth-table exp1)
;(tt
; '(x y)
; (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1)))
;> (truth-table exp5)
;(tt
; '(x y z)
; (list
;  (entry '(0 0 0) 1)
;  (entry '(0 0 1) 1)
;  (entry '(0 1 0) 0)
;  (entry '(0 1 1) 0)
;  (entry '(1 0 0) 1)
;  (entry '(1 0 1) 1)
;  (entry '(1 1 0) 1)
;  (entry '(1 1 1) 1)))
;> 
; ****************************************************************

(define (truth-table exp)
  (let* ([vars (all-vars exp)]
         [combs (all-combs (length vars))]) 
    (tt vars (find-rows vars exp combs))))

(define (find-rows vars exp combs); vars stays same, exp stays same, append entrys with car combs being taken away, stop when combs empty 
  (if (empty? combs)
      '()
      (cons (new-entry exp vars (car combs)) (find-rows vars exp (cdr combs))))) ; basically building rows by out of entries

(define (new-entry exp vars comb) ; comb = one combination = key in entry, creates a new entry that will be added to list of rows
  (entry comb (eval-in-env exp (new-env vars comb))))

(define (new-env vars comb) ; creates a new environment(list of entries) that will be used to find value within entry
  (if (empty? vars)
      '()
      (cons (entry (car vars) (car comb))(new-env (cdr vars)(cdr comb))))) ; creates a list of entries, for each, connects car of comb (boolean values) to car of variables, 
  
    

; ****************************************************************
; ** problem 7 ** (10 points)
; Write two procedures

; (satisfiable? exp)
; (equivalent? exp1 exp2)

; (satisfiable? exp)
; takes one Boolean expression exp and
; returns #t if exp is satisfiable and #f otherwise.

; (equivalent? exp1 exp2)
; takes two Boolean expressions exp1 and exp2
; and returns #t if they are equivalent and #f if they
; are not equivalent.

; A Boolean expression is satisfiable if there exists an environment
; in which its value is 1.  Two Boolean expressions are equivalent
; if for every environment which assigns values to all the variables
; in either expression, they have the same value in that environment.

; One possibility might be to use satisfiable? to help implement equivalent?

; These procedures will be tested on expressions with few enough
; variables that generating truth tables will be a feasible approach.

; Examples:
;>  (satisfiable? 0)
;#f
;> (satisfiable? 1)
;#t
;>  (satisfiable? (band 'x (band 'y 'z)))
;#t
;> (satisfiable? (band 'x (band 'y (bnot 'y))))
;#f
;>  (satisfiable? (band (bor 'x (bnot 'y)) 0))
;#f
;>  (equivalent? 0 (band 'a (bnot 'a)))
;#t
;>  (equivalent? 0 'a) 
;#f
;>  (equivalent? (bor 'x (bor 'y 'z)) (bor 0 (bor 'z (bor 'x 'y))))
;#t
;> (equivalent? (bor 'x (band 'y 'z)) (band (bor 'x 'y) (bor 'x 'z)))
;#t
;> 
; ****************************************************************

(define (satisfiable? exp)
  (let ([truth (truth-table exp)])
    (if (empty? (remove* '(0) (map (lambda (x) (entry-value x))(tt-rows truth))))
        #f
        #t)))

(define (equivalent? exp1 exp2)
  (let* ([truth1 (map(lambda (x) (entry-value x)) (tt-rows (truth-table-eq exp1 exp2)))] ; truth table using exp1 as main expression
         [truth2 (map(lambda (x) (entry-value x)) (tt-rows (truth-table-eq exp2 exp1)))]) ; truth table using exp2
    (equal-lists truth1 truth2))); if these aren't equal, then aren't equivalent

(define (equal-lists lst1 lst2) ; checks if the lists are equal in every way, if so, true
  (cond
    [(and (empty? lst1)(empty? lst2))#t]
    [(or (empty? lst1)(empty? lst2))#f]
    [(equal? (car lst1)(car lst2))(equal-lists (cdr lst1)(cdr lst2))]
    [else #f]))
               
(define (truth-table-eq exp1 exp2) ; creates truth table with all variables in vars, but only uses exp1 as rows
  (let* ([vars (append (all-vars exp1)(all-vars exp2))]
         [combs (all-combs (length vars))]) 
    (tt vars (find-rows vars exp1 combs))))

; ****************************************************************
; ** problem 8 ** (10 points)
; Write a procedure

; (find-exp tt)

; This procedure takes a truth table
; and returns a Boolean expression 
; for the given truth table.

; You may choose to use the sum-of-products algorithm
; from lecture, or some other method.
; Please include comments explaining your method
; in either case.

;Examples
;>  (boolean-exp? (find-exp tt-and))
;#t
;>  (equivalent? (find-exp tt-and) (band 'x 'y))
;#t
;>  (equivalent? (find-exp tt-imp) (bor (bnot 'a) 'b)) 
;#t
;>  (equivalent? (find-exp tt-xor) (bor (band 'u (bnot 'v)) (band (bnot 'u) 'v)))
;#t
;>  (boolean-exp? (find-exp tt-f1))
;#t
;> 
; ****************************************************************
;(struct tt (vars rows) #:transparent)

(define (find-exp tt)
  (construct-sum-lst (tt-vars tt)(find-truth-list (tt-rows tt) '()))) ; construct-sum-lst (all vars in tt) (list of all entrys that result in 1)

(define (find-truth-list rows lst) ; first goes through tt-rows and cons any who's value is 1 (aka true)
  (cond
    [(empty? rows)lst]
    [(equal? 1 (entry-value (first rows)))(find-truth-list (cdr rows)(cons (car rows) lst))] ; if equals 1, add to list
    [else (find-truth-list (cdr rows) lst)]))

(define (construct-sum-lst vars entry-lst); next connect list with bor's at each new value in entry-lst
  (if (empty? (cdr entry-lst))
      (construct-sum vars (entry-key (car entry-lst)))
      (bor (construct-sum vars (entry-key (car entry-lst)))(construct-sum-lst vars (cdr entry-lst)))))

(define (construct-sum vars key) ; constructs each individual part of the sum based on that entry-key with bands
  (cond
    [(and (equal? 0 (car key))(empty? (cdr key)))(bnot (car vars))]; if at and and 0, say not value
    [(empty? (cdr key))(car vars)]; if just at end, say value
    [(equal? 0 (car key))(band (bnot (car vars))(construct-sum (cdr vars)(cdr key)))] ; if not value, band (bnot) to cdr
    [else (band (car vars)(construct-sum (cdr vars)(cdr key)))])) ; else, just band to cdr

; ****************************************************************
; ** problem 9 ** (10 points)
; Write a procedure

; (substitute-in exp sub-table)

; that takes a Boolean expression exp and a table sub-table each of whose
; entries has a key that is a variable and a value that is a
; Boolean expression, and returns the Boolean expression obtained
; by replacing every occurrence in exp of a variable in the sub-table with
; the corresponding Boolean expression.

; Hint: type-of and deep recursion on the structure of Boolean expressions.

; Examples
;> (substitute-in 0 (list (entry 'x 1)))
;0
;> (substitute-in 'x (list (entry 'x 1)))
;1
;> (substitute-in (band 'x 'y) (list (entry 'x (bnot 'z)) (entry 'y 0)))
;(band (bnot 'z) 0)
;> (substitute-in (band (bor 'x 'y) (bor (bnot 'x) 'y)) (list (entry 'x (bnot 1))))
;(band (bor (bnot 1) 'y) (bor (bnot (bnot 1)) 'y))
;> 
; ****************************************************************

(define (substitute-in exp sub-table)
  (cond
    [(number? exp)exp]
    [(and (symbol? exp)(not (equal? #f (lookup exp sub-table))))(lookup exp sub-table)]; symbols in exp that are in table
    [(symbol? exp)exp]; refers to symbols in exp that aren't in table
    [(bor? exp)(bor (substitute-in (bor-arg1 exp) sub-table)(substitute-in (bor-arg2 exp) sub-table))]
    [(band? exp)(band (substitute-in (band-arg1 exp) sub-table)(substitute-in (band-arg2 exp) sub-table))]
    [else (bnot (substitute-in (bnot-arg exp) sub-table))]))

; ****************************************************************
; ** problem 10 ** (10 points)
; Write a procedure

; (match exp pat)

; that takes two Boolean expressions exp and pat and
; attempts to "match" the expression exp to the pattern pat.
; That is, it tries to find a table sub-table (as in the preceding
; problem) such that (substitute-in pat sub-table) returns
; the expression exp.  If this is not possible, it should return #f.

; Examples
;> (match 1 1)
;'()
;> (match 0 'x)
;(list (entry 'x 0))
;> (match 'z 0)
;#f
;>  (match 'z 'x)
;(list (entry 'x 'z))
;> (match (band 'y 'x) 'x)
;(list (entry 'x (band 'y 'x)))
;>  (match (bnot (band 'a 'b)) (bnot 'z))
;(list (entry 'z (band 'a 'b)))



;> (match (bor (band 'x 'y) 0) (bor 'a 0)) if you can get pat down to symbol, if or, take anything thart osn't 
;(list (entry 'a (band 'x 'y)))


;> (match (band (bor (bnot 'z) 0) (bor (bnot 'z) 'y)) (band (bor 'a 'b) (bor 'a 'c)))
;(list (entry 'a (bnot 'z)) (entry 'b 0) (entry 'c 'y))

;> (match (band (band 'x 'y) 'z) (band (bor 'a 'b) 'c)) ; if all vars completely different to all vars of other  
;#f

;> (match (band 'x 'y) (band 'a 'a))
;#f
;> (substitute-in (band 'a 'a) (match (band (bor 0 1) (bor 0 1)) (band 'a 'a)))
;(band (bor 0 1) (bor 0 1))
;>

;(match-r (bor-arg2 exp) (bor-arg1 pat))
;(match-r (bor-arg1 exp)(bor-arg2 pat))

;(match-r (band-arg1 exp)(band-arg2 pat))(match-r (band-arg2 exp) (band-arg1 pat))
; ****************************************************************

(define (match exp pat)
  (let* ([recur (match-r exp pat)]
         [rem-recur (remove-duplicates recur)]); after removing duplicates
    (cond
      [(empty? recur)'()]
      [(or (boolean? (car rem-recur)) (not (unique-keys? rem-recur))) #f]; if multiple entries with same keys, then false. Also if there's a #f at the beginning of the list then false
      [else(remove-duplicates recur)])))

(define (match-r exp pat) ; if begins with #f, whole thing = #f, else if any #f's inside, cut list off there
  (cond
    [(and (number? exp)(number? pat)) '()] ; if both numbers, '()
    [(number? pat)'(#f)] ; if pat is ever number, #f, has to be list so can append
    [(symbol? pat)(list (entry pat exp))] ; if pat is a symbol, then can finally add to list
    [(and (bor? exp)(bor? pat))(append (match-r (bor-arg1 exp)(bor-arg1 pat))(match-r (bor-arg2 exp) (bor-arg2 pat)))] ; call recursively, matching arg1 of exp to arg1 of pat, same with arg2
    [(and (band? exp)(band? pat))(append (match-r (band-arg1 exp)(band-arg1 pat))(match-r (band-arg2 exp) (band-arg2 pat)))]; same as above but with band
    [(and (bnot? exp) (bnot? pat))(append (match-r (bnot-arg exp)(bnot-arg pat)))]
    [else '(#f)]))
                  
; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

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


(test 'lookup  (lookup "first" test1) 1)
(test 'lookup  (lookup "third" test1) #f)
(test 'lookup  (lookup 'z test2) 1)
(test 'unique-keys?  (unique-keys? test1) #t)
(test 'unique-keys?  (unique-keys? test2) #f)


(test 'boolean-exp?  (boolean-exp? 0) #t)
(test 'boolean-exp?  (boolean-exp? 2) #f)
(test 'boolean-exp?  (boolean-exp? exp0) #t)
(test 'boolean-exp?  (boolean-exp? (band "hi" 'c)) #f)
(test 'boolean-exp?  (boolean-exp? (band 'x (bor 0 (bnot 1)))) #t)
(test 'boolean-exp?  (boolean-exp? (band 'x (bor 0 (bnot #t)))) #f)
(test 'type-of  (type-of 0) 'constant)
(test 'type-of  (type-of 'hi) 'variable)
(test 'type-of  (type-of (bnot (band 'x 0))) 'not)
(test 'type-of  (type-of (bor (band 'x 'y) (band 'x 'z))) 'or)
(test 'type-of  (type-of (band (bor 0 1) (bnot 'x))) 'and)

(test 'all-vars (all-vars 0) '())
(test 'all-vars (all-vars (bnot (band 'x (band 'y (bor 'x 'z))))) '(x y z))
(test 'all-vars (all-vars (band 1 (bor 0 (bnot 'u)))) '(u))
(test 'all-vars (all-vars (band (band 'x 'y) (band 'y 'x))) '(x y))
(test 'all-vars (all-vars (bor (bor (bor 'c 'b) (bor 'a 'b)) 'c)) '(c b a))


(test 'eval-in-env (eval-in-env 1 environ1) 1)
(test 'eval-in-env (eval-in-env (bor 0 0) '()) 0)
(test 'eval-in-env (eval-in-env 'x environ1) 0)
(test 'eval-in-env (eval-in-env 'x environ2) 1)
(test 'eval-in-env (eval-in-env (bnot 'z) environ1) 1)
(test 'eval-in-env (eval-in-env (bor 'y (bnot 'x)) environ2) 0)
(test 'eval-in-env (eval-in-env (band (band (bor 'u 'x) (bor 'w 0)) (bnot (band 'y 'z))) environ2) 1)
(test 'eval-in-env (eval-in-env exp5 environ1) 0)
(test 'eval-in-env (eval-in-env (band 'y (bor 'x 'u)) (list (entry 'x 0) (entry 'y 1))) 'unspecified-variable)


(test 'all-combs (all-combs 0) '(()))
(test 'all-combs (all-combs 1) '((0) (1)))
(test 'all-combs (all-combs 2) '((0 0) (0 1) (1 0) (1 1)))
(test 'all-combs (all-combs 3) '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1)))


(test 'truth-table (truth-table exp0) (tt '() (list (entry '() 1))))
(test 'truth-table (truth-table exp1) 
      (tt '(x y)
	  (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1))))
(test 'truth-table (truth-table exp5) (tt
				       '(x y z)
				       (list
					(entry '(0 0 0) 1)
					(entry '(0 0 1) 1)
					(entry '(0 1 0) 0)
					(entry '(0 1 1) 0)
					(entry '(1 0 0) 1)
					(entry '(1 0 1) 1)
					(entry '(1 1 0) 1)
					(entry '(1 1 1) 1))))



(test satisfiable?  (satisfiable? 0) #f)
(test satisfiable? (satisfiable? 1) #t)
(test satisfiable?  (satisfiable? (band 'x (band 'y 'z))) #t)
(test satisfiable? (satisfiable? (band 'x (band 'y (bnot 'y)))) #f)
(test satisfiable?  (satisfiable? (band (bor 'x (bnot 'y)) 0)) #f)
(test equivalent?  (equivalent? 0 (band 'a (bnot 'a))) #t)
(test equivalent?  (equivalent? 0 'a) #f)
(test equivalent?  (equivalent? (bor 'x (bor 'y 'z)) (bor 0 (bor 'z (bor 'x 'y)))) #t)
(test equivalent? (equivalent? (bor 'x (band 'y 'z)) (band (bor 'x 'y) (bor 'x 'z))) #t)


(test 'find-exp  (boolean-exp? (find-exp tt-and)) #t)
(test 'find-exp  (equivalent? (find-exp tt-and) (band 'x 'y)) #t)
(test 'find-exp  (equivalent? (find-exp tt-imp) (bor (bnot 'a) 'b)) #t)
(test 'find-exp  (equivalent? (find-exp tt-xor) (bor (band 'u (bnot 'v)) (band (bnot 'u) 'v))) #t)
(test 'find-exp  (boolean-exp? (find-exp tt-f1)) #t)

(test 'substitute-in (substitute-in 0 (list (entry 'x 1))) 0)
(test 'substitute-in (substitute-in 'x (list (entry 'x 1))) 1)
(test 'substitute-in (substitute-in (band 'x 'y) (list (entry 'x (bnot 'z)) (entry 'y 0))) (band (bnot 'z) 0))
(test 'substitute-in (substitute-in (band (bor 'x 'y) (bor (bnot 'x) 'y)) (list (entry 'x (bnot 1)))) (band (bor (bnot 1) 'y) (bor (bnot (bnot 1)) 'y)))


(test 'match (match 1 1) '())
(test 'match (match 0 'x) (list (entry 'x 0)))
(test 'match (match 'z 0) #f)
(test 'match  (match 'z 'x) (list (entry 'x 'z)))
(test 'match (match (band 'y 'x) 'x) (list (entry 'x (band 'y 'x))))
(test 'match  (match (bnot (band 'a 'b)) (bnot 'z)) (list (entry 'z (band 'a 'b))))
(test 'match (match (bor (band 'x 'y) 0) (bor 'a 0)) (list (entry 'a (band 'x 'y))))
(test 'match (match (band (bor (bnot 'z) 0) (bor (bnot 'z) 'y)) (band (bor 'a 'b) (bor 'a 'c))) (list (entry 'a (bnot 'z)) (entry 'b 0) (entry 'c 'y)))
(test 'match (match (band (band 'x 'y) 'z) (band (bor 'a 'b) 'c)) #f)
(test 'match (match (band 'x 'y) (band 'a 'a)) #f)
(test 'match (substitute-in (band 'a 'a) (match (band (bor 0 1) (bor 0 1)) (band 'a 'a))) (band (bor 0 1) (bor 0 1)))



; **************** end of hw #4 *********************************
#lang racket

(provide hours
         gate gate? gate-type gate-inputs gate-output
         ckt ckt? ckt-inputs ckt-outputs ckt-gates
         good-gate? good-circuit?
         all-wires find-gate
         ha-ckt fa-ckt
         entry entry? entry-key entry-value
         next-value
         next-config
         stable? all-stable-configs output-values init-config
         simulate
         final-config
         add-ckt
         dff-ckt
         timing-ckt)


; Please do not edit lines above this one

;**********************************************************
; CS 201a HW #5, due 11:59 pm Thursday, November 2, 2017
; using the submit command.
;**********************************************************
; Name: Kenia Hale
; Email address: kenia.hale@yale.edu
;**********************************************************

; Computer science topics: gates and circuits

; Unless the problem specifies otherwise:
; * You may solve the problem using any method 
; and any Racket constructs, except mutators (set! and its relatives.)
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;**********************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 8)

;**********************************************************

; Wires and gates.

; A wire is identified by a Racket symbol, for example 'x, 'y0, or 'next.
; Strings (eg, "x" or "next") are not permitted wire names.

; A gate is a struct with three fields: 
; 1) a symbol indicating the type of gate, one of:
;       not, and, or, xor, nand, nor
; 2) a list of the wire identifiers of the inputs
; 3) the output wire identifier

(struct gate (type inputs output) #:transparent)

; Examples of gates:

(define gate1 (gate 'and '(x y) 'z))
(define gate2 (gate 'or '(x v) 'v))
(define gate3 (gate 'not '(s) 't))
(define gate4 (gate 'nand '(x x) 'z))

;**********************************************************

; Circuits.

; A circuit is a struct with three fields
; (1) a list of input wire identifiers
; (2) a list of output wire identifiers
; (3) a list of gates

(struct ckt (inputs outputs gates) #:transparent)

;**********************************************************
; Examples of circuits

; Here is a circuit to compare the values of its two inputs
; and output 1 if they are equal, 0 if they are unequal.
; This computes one-bit compare for equality, and implements
; the sum-of-products representation.  This is a combinational
; circuit (no cycle of wires and gates.)

(define eq1-ckt
  (ckt
   '(x y)
   '(z)
   (list
    (gate 'not '(x) 'cx)
    (gate 'not '(y) 'cy)
    (gate 'and '(x y) 't1)
    (gate 'and '(cx cy) 't2)
    (gate 'or '(t1 t2) 'z))))
 
; This is interpreted as follows:
; the inputs of the circuit are the wires x and y,
; the outputs of the circuit consist of just the wire z,
; there are five gates specified as follows:
; wire cx is the output of a NOT gate with input x,
; wire cy is the output of a NOT gate with input y,
; wire t1 is the output of an AND gate with inputs x and y,
; wire t2 is the output of an AND gate with inputs cx and cy,
; wire z is the output of an OR gate with inputs t1 and t2.

; Here is another implementation of comparing two bits for equality.
; This uses the implementation as the NOT of (x XOR y).
; This is also a combinational circuit.
; The inputs and output of this circuit are named as in eq1-ckt.

(define eq2-ckt
  (ckt
   '(x y)
   '(z)
   (list
    (gate 'xor '(x y) 'w)
    (gate 'not '(w) 'z))))

; Here is a two-bit selector whose Boolean expressions are as follows.

; z_1 = x_1 * s' + y_1 * s
; z_0 = x_0 * s' + y_0 * s

; For this circuit, z_1 and z_0 are
; equal to x_1 and x_0 if s = 0, and
; z_1 and z_0 are equal to y_1 and y_0
; if s = 1.

; This is also a combinational circuit.

(define sel-ckt
  (ckt
   '(x1 x0 y1 y0 s)
   '(z1 z0)
   (list
    (gate 'not '(s) 'sc)
    (gate 'and '(x1 sc) 'u1)
    (gate 'and '(y1 s) 'v1)
    (gate 'or '(u1 v1) 'z1)
    (gate 'and '(x0 sc) 'u0)
    (gate 'and '(y0 s) 'v0)
    (gate 'or '(u0 v0) 'z0))))

; This is a NAND latch, used to store one bit.
; It is a sequential (not combinational) circuit,
; because it has a loop from a wire to itself through
; other wires and gates.

(define latch-ckt
  (ckt
   '(x y)
   '(q u)
   (list
    (gate 'nand '(x u) 'q)
    (gate 'nand '(y q) 'u))))

; The following is also a sequential circuit, with
; an OR gate one of whose inputs is its output.
; (The "Garden of Eden" circuit.)

(define seq-or-ckt
  (ckt
   '(x)
   '(z)
   (list
    (gate 'or '(x z) 'z))))

; The next is also a sequential circuit.
; It could serve as a clock.
; Note that this circuit has *no* inputs, but
; does have an output.

(define clock-ckt
  (ckt
   '()
   '(z)
   (list
    (gate 'not '(z) 'z))))

;**********************************************************
; ** problem 1 ** (9 points)
; Write two procedures:

; (good-gate? value)
; (good-circuit? value)

; (good-gate? value) takes an arbitrary value
; and returns #t if it is a well-formed gate,
; and #f otherwise.
; To be a well-formed gate, the value must be
; a gate struct whose three fields satisfy
; (1) the type field is one of the gate symbols:
;     'not, 'and, 'or, 'xor, 'nand, 'nor,
; (2) the inputs field is a list of wire identifiers
; (3) the output field is a single wire identifier

; In addition, the number of inputs should be correct for each gate type.
; A gate of type 'not has 1 input, while
; gates of types 'and, 'or, 'xor, 'nand, 'nor have 2 inputs.

; (good-circuit? value) takes an arbitrary value and
; returns #t if value is a well-formed circuit, 
; and returns #f otherwise.

; To be a well-formed circuit, it must be a ckt struct
; and its inputs field must be a list of wires,
; its outputs field must be a list of wires, and
; its gates field must be a list of gates that
; are well-formed according to the good-gate? procedure.

; In addition, the circuit must satisfy the conditions:
; (1) no input of the circuit is the output of a gate,
; (2) every input of a gate is either 
; an input of the circuit or the output of a gate,
; (3) no wire is the output of two or more gates,
; (4) every output of the circuit is either an input
; of the circuit or the output of a gate.

; Examples
; (good-gate? gate1) => #t
; (.. and similarly for gate2, gate3, gate4)
; (good-gate? (gate 'not 'x 'y)) => #f
; (good-gate? (gate 'nor '("x" "y") "z")) => #f
; (good-gate? (gate 'and '(1 2) 3)) => #f
; (good-gate? (gate 'equal '(x y) 'z)) => #f
; (good-gate? (gate 'or '(w x y) 'z)) => #f

; (good-circuit? sel-ckt) => #t
; (.. and similarly for eq1-ckt, eq2-ckt, latch-ckt, seq-or-ckt, clock-ckt)
; (good-circuit? 'hi) => #f
; (good-circuit? (ckt '() '() '())) => #t
; (good-circuit? (ckt '(x y) '(z) (list (gate 'and '(x y) 'x) (gate 'or '(x y) 'z)))) => #f
; (good-circuit? (ckt '(x y) '(z) (list (gate 'nor '(x y) 'z) (gate 'nand '(x y) 'z)))) => #f
; (good-circuit? ((ckt '(x y) '(u z) (list (gate 'or '(x y) 'z)))) => #f
;**********************************************************
;'not, 'and, 'or, 'xor, 'nand, 'nor,
;(struct gate (type inputs output) #:transparent)
(define (good-gate? value)
  (and (gate? value)(list? (gate-inputs value))(gate-type? value)(symbol?(gate-output value)))); if inputs = list, gate-typs is valid, has appropriate number of inputs/are all symbols,
           
(define (gate-type? v)
  (cond
    [(equal? (gate-type v) 'not)(and (equal? (length (gate-inputs v)) 1) (wires? (gate-inputs v)))] ; checks gate type, as well as length of inputs and if all of them are symbols
    [(equal? (gate-type v) 'and)(and (equal? (length (gate-inputs v)) 2) (wires? (gate-inputs v)))]
    [(equal? (gate-type v) 'or) (and (equal? (length (gate-inputs v)) 2) (wires? (gate-inputs v)))]
    [(equal? (gate-type v) 'xor)(and (equal? (length (gate-inputs v)) 2) (wires? (gate-inputs v)))]
    [(equal? (gate-type v) 'nand)(and (equal? (length (gate-inputs v)) 2) (wires? (gate-inputs v)))]
    [(equal? (gate-type v) 'nor) (and (equal? (length (gate-inputs v)) 2) (wires? (gate-inputs v)))]
    [else #f]))

(define (wires? v); checks if list, and if everything in list is a symbol
  (cond
    [(not (list? v)) #f]
    [(empty? v) #t]
    [(not (symbol? (car v))) #f]
    [else (wires? (cdr v))]))
      
      

(define (good-circuit? value)
  (if (not (ckt? value)) #f
      (let* ([i (ckt-inputs value)]
             [o (ckt-outputs value)]
             [g (ckt-gates value)])
        (and 
             (wires? i) ; if inputs = list of wires
             (wires? o) ; if outputs - list of wires
             (list? g) ; if gates = list
             (gates? g)
             (not (check-duplicates (append i (all-gates-outputs value)))) ; if no overlapping bewteen original inputs and outputs of all gates
             (recur-contains (all-gates-inputs value) (append i (all-gates-outputs value))) ; every input of gates = either original input or output of a gate
             (not (check-duplicates (all-gates-outputs value))) ; not multiples in output of gates
             (recur-contains o (append i (all-gates-outputs value))))))) ; every output of circut = either original input or output of a gate
         
(define (gates? v) ; if list of good gates
  (cond
    [(empty? v) #t]
    [(not (good-gate? (car v)))#f] ; checks if good-gate
    [else (gates? (cdr v))]))

(define (all-gates-inputs circut) ; list of all original inputs of all gates
  (flatten (map gate-inputs (ckt-gates circut))))

(define (all-gates-outputs circut); list of every original output of each gate
  (map gate-output (ckt-gates circut)))
  
(define (recur-contains vars all-vars) ; vars = list, all-vars = list of all of some type of variables, checking to see if everyhting in vars is in all-vars
  (cond
    [(and (empty? all-vars) (empty? vars)) #t]
    [(empty? all-vars) #f]
    [(empty? vars)#t]
    [(boolean? (member (car vars) all-vars))#f] ; will return that value if keys equal
    [else (recur-contains (cdr vars) all-vars)])); else, run on cdr
  

;**********************************************************
; ** problem 2 ** (10 points)
; Write two procedures.

; (all-wires circuit) to return the list of all the wire names that appear
;      in the circuit, as circuit inputs, circuit outputs, gate
;      inputs or gate outputs, in that order, with duplicates removed.
; (find-gate wire circuit) to return the gate in the circuit with the given
;      output wire, or #f if there is no such gate.

; You may assume that circuit is a well-formed circuit; in particular,
; a wire is the output of *at most one* gate.

; Examples:

; (all-wires eq1-ckt) => '(x y z cx cy t1 t2)
; (all-wires sel-ckt) => '(x1 x0 y1 y0 s z1 z0 sc u1 v1 u0 v0)
; (find-gate 't2 eq1-ckt) => (gate 'and '(cx cy) 't2)
; (find-gate 'w eq2-ckt) => (gate 'xor '(x y) 'w)
; (find-gate 'y sel-ckt) => #f
;**********************************************************

(define (all-wires circuit) 
  (remove-duplicates (append (ckt-inputs circuit)(ckt-outputs circuit)(all-gates-inputs circuit)(all-gates-outputs circuit))))

(define (find-gate wire circuit)
  (if (recur-contains (list wire) (all-gates-outputs circuit))
      (find-gate-r wire (ckt-gates circuit))
      #f)); if in there
(define (find-gate-r wire gate-list)
  (if (equal? wire (gate-output (car gate-list)))
      (car gate-list)
      (find-gate-r wire (cdr gate-list))))
      


;**********************************************************
; ** problem 3 ** (10 points)
; Define circuits for a half-adder and a full-adder in the representation described above.

; Your half-adder should be called ha-ckt
; and should have input wires: x and y and output wires: z and co, 
; where z is the exclusive or of x and y, and co is 1 if both x and y are 1.

; Your full-adder should be called fa-ckt 
; and should have input wires: x, y, and ci and output wires: z and co,
; where the value of z is 1 if the sum of x, y, and ci is odd,
; and the value of co is 1 if and only if at least two of x, y, and ci are 1.

; The order and names of the circuit input and output wires should be as specified above,
; but the number and names of internal wires (wires that are neither circuit inputs 
; nor circuit outputs) are up to you.

; Examples
; (good-circuit? ha-ckt) => #t
; (good-circuit? fa-ckt) => #t
; (ckt-inputs ha-ckt) => '(x y)
; (ckt-outputs ha-ckt) => '(z co)
; (ckt-inputs fa-ckt) => '(x y ci)
; (ckt-outputs fa-ckt) => '(z co)

; (output-values ha-ckt (final-config ha-ckt (init-config ha-ckt '(1 1)))) => (0 1)
; (output-values fa-ckt (final-config fa-ckt (init-config fa-ckt '(1 1 1)))) => (1 1)

; For the last two tests, your procedures output-values, final-config, init-config must be working.
;**********************************************************

(define ha-ckt
	(ckt '(x y) '(z co) (list (gate 'xor '(x y) 'z)
                                  (gate 'and '(x y) 'co))))


(define fa-ckt
	(ckt '(x y ci) '(z co) (list (gate 'xor '(x y) 'xyz);xor returns odd? sum of three inputs
                                     (gate 'xor '(ci xyz) 'z)
                                     (gate 'and '(x y) 'xy)
                                     (gate 'and '(y ci) 'yci)
                                     (gate 'and '(ci x) 'cix)
                                     (gate 'or '(xy yci) 'yy)
                                     (gate 'or '(yy cix) 'co))))



;**********************************************************
; A configuration of a circuit isa table giving a value (0 or 1) for each wire in the circuit.  
; A table is a list of entries, each entry containing a key (the wire name) and a value (0 or 1).

(struct entry (key value) #:transparent)

; Examples
; Two configurations of the wires of the eq1-ckt
; Note that the order of wires in the configuration is that returned by (all-wires eq1-ckt).

(define eq1-config1
  (list
   (entry 'x 0)
   (entry 'y 1)
   (entry 'z 0)
   (entry 'cx 0)
   (entry 'cy 0)
   (entry 't1 0)
   (entry 't2 0)))

(define eq1-config2 
  (list
   (entry 'x 0)
   (entry 'y 0)
   (entry 'z 0)
   (entry 'cx 1)
   (entry 'cy 1)
   (entry 't1 0)
   (entry 't2 0)))

; Two configurations of the wires of the sel-ckt

(define sel-config1
  (list
   (entry 'x1 0)
   (entry 'x0 1)
   (entry 'y1 1)
   (entry 'y0 0)
   (entry 's 1)
   (entry 'z1 0)
   (entry 'z0 0)
   (entry 'sc 0)
   (entry 'u1 0)
   (entry 'v1 0)
   (entry 'u0 0)
   (entry 'v0 0)))


(define sel-config2
  (list
   (entry 'x1 1)
   (entry 'x0 1)
   (entry 'y1 0)
   (entry 'y0 0)
   (entry 's 0)
   (entry 'z1 0)
   (entry 'z0 0)
   (entry 'sc 1)
   (entry 'u1 0)
   (entry 'v1 0)
   (entry 'u0 0)
   (entry 'v0 0)))

; Two configurations of the wires of the latch-ckt

(define latch-config1
  (list
   (entry 'x 0)
   (entry 'y 0)
   (entry 'q 0)
   (entry 'u 0)))

(define latch-config2
  (list
   (entry 'x 0)
   (entry 'y 1)
   (entry 'q 1)
   (entry 'u 0)))

;**********************************************************
; ** problem 4 ** (10 points)
; Write a procedure 

;(next-value wire circuit config)

; that returns the value on the given wire of the given circuit, *after one gate delay*
; starting with the given configuration config of the circuit.

; You may assume that
; (1) circuit is a well-formed circuit, according to the specifications in problem 1,
; (2) the given wire is one of the wires of circuit, and
; (3) the given configuration config specifies a value for every wire in the circuit.

; If the given wire is an input wire of the circuit,
; its next value is just its value in the configuration config.

; If the given wire is the output wire of a gate,
; its next value is obtained by finding the gate of circuit for which it is the output wire, 
; looking up the values of the input wires of the gate in the configuration config,
; and applying the appropriate function of the gate to the input values.

; Note that this doesn't compute the *eventual* value (if any) of the wire, 
; just the *next* value of the wire, after one gate delay.

; You may want to write an auxiliary procedure to look up the value of a wire in a configuration.

; Examples
; (next-value 'cx eq1-ckt eq1-config1) => 1
; (next-value 't2 eq1-ckt eq1-config1) => 0
; (next-value 'z eq1-ckt eq1-config2) => 0
; (next-value 'x0 sel-ckt sel-config1) => 1
; (next-value 'v1 sel-ckt sel-config1) => 1
; (next-value 'v0 sel-ckt sel-config2) => 0
;**********************************************************

(define (next-value wire circuit config)
  (if (recur-contains (list wire) (ckt-inputs circuit))
      (lookup wire config)
      (let* ([input-list (gate-inputs (find-gate wire circuit))]
             [type (gate-type (find-gate wire circuit))])
        (if (equal? 'not type)
            (bool-convert(not (bool-convert(lookup (car input-list) config))))
            (let* ([arg1 (car input-list)]
                   [arg2 (car (cdr input-list))])
                 (gate-type-convert type (lookup arg1 config) (lookup arg2 config)))))))
        
(define (lookup key table)
  (cond
    [(empty? table) #f]
    [(string? table) #f]
    [(equal? key (entry-key (car table)))(entry-value (car table))] ; will return that value if keys equal
    [else (lookup key (cdr table))])); else, run on cdr

(define (gate-type-convert v a b)
  (let* ([a-c (bool-convert a)]
         [b-c (bool-convert b)])
  (cond
    [(equal? v 'and)(bool-convert (and a-c b-c))]
    [(equal? v 'or)(bool-convert (or a-c b-c))]
    [(equal? v 'xor)(bool-convert (xor a-c b-c))]
    [(equal? v 'nand)(bool-convert (nand a-c b-c))]
    [(equal? v 'nor)(bool-convert (nor a-c b-c))]
    [else #f])))
        
(define (bool-convert b) ; converts between one's and zero's and #t and #f
  (cond
    [(equal? #t b) 1]
    [(equal? #f b) 0]
    [(equal? 1 b) #t]
    [else #f])) ; if 0

;**********************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (next-config circuit config)

; that takes a circuit and a current configuration config
; and returns the "next" configuration of the circuit, after *one gate delay* has elapsed.

; In the "next" configuration of the circuit the value of each wire is the result
; of applying the next-value procedure to the wire, circuit and the configuration config.
; Note that only the values of wires in config are used for inputs, not the new values.

; Thus, values on the input wires do not change, and each wire that is the output
; of a gate has the value determined by its gate function applied the values
; of its input wires in the configuration config.

; This is a rather simplified model of the time-varying behavior of wires and gates.

; Examples
;> (next-config eq1-ckt eq1-config1)
;(list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'cx 1) (entry 'cy 0) (entry 't1 0) (entry 't2 0))

;> (next-config eq1-ckt eq1-config2)
;(list (entry 'x 0) (entry 'y 0) (entry 'z 0) (entry 'cx 1) (entry 'cy 1) (entry 't1 0) (entry 't2 1))

;> (next-config sel-ckt sel-config1)
;(list
; (entry 'x1 0)
; (entry 'x0 1)
; (entry 'y1 1)
; (entry 'y0 0)
; (entry 's 1)
; (entry 'z1 0)
; (entry 'z0 0)
; (entry 'sc 0)
; (entry 'u1 0)
; (entry 'v1 1)
; (entry 'u0 0)
; (entry 'v0 0))

;> (next-config sel-ckt (next-config sel-ckt sel-config1))
;(list
; (entry 'x1 0)
; (entry 'x0 1)
; (entry 'y1 1)
; (entry 'y0 0)
; (entry 's 1)
; (entry 'z1 1)
; (entry 'z0 0)
; (entry 'sc 0)
; (entry 'u1 0)
; (entry 'v1 1)
; (entry 'u0 0)
; (entry 'v0 0))

;> (next-config latch-ckt latch-config1)
;(list (entry 'x 0) (entry 'y 0) (entry 'q 1) (entry 'u 1))

;> (next-config latch-ckt latch-config2) 
;(list (entry 'x 0) (entry 'y 1) (entry 'q 1) (entry 'u 0))

;**********************************************************

(define (next-config circuit config)
      (map (lambda (x) (entry (entry-key x)(next-value (entry-key x) circuit config))) config));create new entry with next-value for each entry key
  ;(map (next-value (entry-key(car config)) circuit config) config)) 

;**********************************************************
; ** problem 6 ** (10 points)
; Write four procedures

; (stable? circuit config)
; (all-stable-configs circuit)
; (output-values circuit config)
; (init-config circuit input-values)

; (stable? circuit config)
; returns #t if the next configuration of the circuit after the configuration config
; is the same as config, ie, this configuration is stable for the circuit.

; (all-stable-configs circuit)
; returns a list of all the stable configurations of the circuit.
; The wires in the configurations should be listed in the same order as (all-wires circuit),
; and the values in the configurations list should be in increasing order, considered as
; binary numbers.

; (output-values circuit config)
; returns a list giving the Boolean values of each of the output wires of
; the circuit in the configuration config.
; The order is the same as the list of output wires of the circuit.

; (init-config circuit input-values)
; takes a circuit and a list input-values of Boolean values
; which has the same length as the number of inputs of the circuit
; and returns a configuration in which the circuit input wires have the values 
; specified (in order) by the list inputs, and all other wires have the value 0.

; Examples
; (stable? eq1-ckt (list (entry 'x 0) (entry 'y 0) (entry 'z 1) (entry 'cx 1) (entry 'cy 1) (entry 't1 0) (entry 't2 1))) => #t
; (stable? eq1-ckt (list (entry 'x 0) (entry 'y 0) (entry 'z 0) (entry 'cx 1) (entry 'cy 0) (entry 't1 1) (entry 't2 0))) => #f

;> (all-stable-configs eq2-ckt)
;(list
; (list (entry 'x 0) (entry 'y 0) (entry 'z 1) (entry 'w 0))
; (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'w 1))
; (list (entry 'x 1) (entry 'y 0) (entry 'z 0) (entry 'w 1))
; (list (entry 'x 1) (entry 'y 1) (entry 'z 1) (entry 'w 0)))

;> (all-stable-configs seq-or-ckt)
;(list (list (entry 'x 0) (entry 'z 0)) (list (entry 'x 0) (entry 'z 1)) (list (entry 'x 1) (entry 'z 1)))

; (output-values eq1-ckt eq1-config2) => '(0)
; (output-values latch-ckt latch-config2) => '(1 0)
; (init-config eq1-ckt '(1 0)) => (list (entry 'x 1) (entry 'y 0) (entry 'z 0) (entry 'cx 0) (entry 'cy 0) (entry 't1 0) (entry 't2 0))
; (init-config clock-ckt '()) => (list (entry 'z 0))

;**********************************************************

(define (stable? circuit config)
  (equal? config (next-config circuit config)))

;****************************888 ALL Stable configs

(define (all-stable-configs circuit)
  (let* ([wires (all-wires circuit)]
         [combs (all-combs(length wires))])
    (all-stable-configs-recurse combs circuit))) ; combs = allcombs, meanining all binary combiniations for the number of wires

(define (all-stable-configs-recurse c circuit); recurses through all possible configurations and selects any that are stable
  (let ([wires (all-wires circuit)])
  (cond [(empty? c)'()]
        [(stable? circuit (build-config wires (car c)))(cons (build-config wires (car c))(all-stable-configs-recurse (cdr c) circuit))]; if it's stable, cons to next bc making list of lists of entries
        [else (all-stable-configs-recurse (cdr c) circuit)])))
  
 
(define (build-config wires vals); all-wires and one value from all combs
  (if (empty? wires)
      '()
      (cons (entry (car wires) (car vals))(build-config (cdr wires) (cdr vals)))))

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
    [else (mydec2bin (quotient num 2)(append (list (remainder num 2)) lst))]))
;*****&****************************************************************************** end all stable configs

(define (output-values circuit config)
    (map (lambda (x) (lookup x config)) (ckt-outputs circuit)))



(define (init-config circuit input-values) ; whil
  (let* ([vals (all-wires circuit)]
        [num (- (length vals) (length input-values))])
    (cond
      [(> num 0)(init-config circuit (append input-values (list 0)))]; while more wires than input-values, add zero's on end
      [else (build-config vals input-values)]))); build config will match up every wire to an input
    

; *********************************************************
; ** problem 7 ** (10 points)
; Write a procedure 

; (simulate circuit config n)

; which simulates the given circuit from the given configuration 
; by repeatedly calling next-config until either
; the configuration reached is stable, or
; next-config has been called n times, whichever occurs first.

; Examples
;> (simulate clock-ckt (list (entry 'z 0)) 4)
;(list
; (list (entry 'z 0))
; (list (entry 'z 1))
; (list (entry 'z 0))
; (list (entry 'z 1))
; (list (entry 'z 0)))

;> (simulate eq1-ckt eq1-config1 5)
;(list
; (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'cx 0) (entry 'cy 0) (entry 't1 0) (entry 't2 0))
; (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'cx 1) (entry 'cy 0) (entry 't1 0) (entry 't2 0)))

;> (simulate sel-ckt sel-config1 5)
;(list
; (list
;  (entry 'x1 0)
;  (entry 'x0 1)
;  (entry 'y1 1)
;  (entry 'y0 0)
;  (entry 's 1)
;  (entry 'z1 0)
;  (entry 'z0 0)
;  (entry 'sc 0)
;  (entry 'u1 0)
;  (entry 'v1 0)
;  (entry 'u0 0)
;  (entry 'v0 0))
; (list
;  (entry 'x1 0)
;  (entry 'x0 1)
;  (entry 'y1 1)
;  (entry 'y0 0)
;  (entry 's 1)
;  (entry 'z1 0)
;  (entry 'z0 0)
;  (entry 'sc 0)
;  (entry 'u1 0)
;  (entry 'v1 1)
;  (entry 'u0 0)
;  (entry 'v0 0))
; (list
;  (entry 'x1 0)
;  (entry 'x0 1)
;  (entry 'y1 1)
;  (entry 'y0 0)
;  (entry 's 1)
;  (entry 'z1 1)
;  (entry 'z0 0)
;  (entry 'sc 0)
;  (entry 'u1 0)
;  (entry 'v1 1)
;  (entry 'u0 0)
;  (entry 'v0 0)))

;> (simulate latch-ckt latch-config2 3)
;(list (list (entry 'x 0) (entry 'y 1) (entry 'q 1) (entry 'u 0)))

;> (simulate eq2-ckt (init-config eq2-ckt '(0 1)) 5)
;(list
; (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'w 0))
; (list (entry 'x 0) (entry 'y 1) (entry 'z 1) (entry 'w 1))
; (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'w 1)))

;**********************************************************

(define (simulate circuit config n)
  (cond
    [(stable? circuit config)(list config)]
    [(equal? 0 n)(list config)]; counting down, so eventually will reach 0
    [else (cons config (simulate circuit (next-config circuit config) (- n 1)))])); else, add present config on to next

;**********************************************************
; ** problem 8 ** (10 points)
; Write a procedure

; (final-config circuit config)

; that takes a circuit and a configuration config for the circuit.
; If the circuit would eventually reach a stable configuration 
; from config, then (final-config circuit config) returns the
; stable configuration of the circuit that would be reached.

; Otherwise, (final-config circuit config) returns the symbol 'none.

; Examples
;> (final-config clock-ckt (list (entry 'z 0)))
;'none

;> (final-config eq1-ckt (list (entry 'x 1) (entry 'y 1) (entry 'z 0) (entry 'cx 0) (entry 'cy 0) (entry 't1 0) (entry 't2 0)))
;(list (entry 'x 1) (entry 'y 1) (entry 'z 1) (entry 'cx 0) (entry 'cy 0) (entry 't1 1) (entry 't2 0))

;> (final-config sel-ckt (list (entry 'x1 0) (entry 'x0 0) (entry 'y1 1) (entry 'y0 0) (entry 's 0) (entry 'z1 1) (entry 'z0 1) (entry 'sc 0) (entry 'u1 1) (entry 'v1 1) (entry 'u0 0) (entry 'v0 1)))
; (list
;  (entry 'x1 0)
;  (entry 'x0 0)
;  (entry 'y1 1)
;  (entry 'y0 0)
;  (entry 's 0)
;  (entry 'z1 0)
;  (entry 'z0 0)
;  (entry 'sc 1)
;  (entry 'u1 0)
;  (entry 'v1 0)
;  (entry 'u0 0)
;  (entry 'v0 0))

;> (final-config latch-ckt (list (entry 'x 1) (entry 'y 1) (entry 'q 0) (entry 'u 0)))
;'none

;> (final-config latch-ckt (list (entry 'x 1) (entry 'y 1) (entry 'q 0) (entry 'u 1)))
;(list (entry 'x 1) (entry 'y 1) (entry 'q 0) (entry 'u 1))

;**********************************************************

(define (final-config circuit config)
  (if (stable? circuit config)
      config
      (simulate2 circuit config (next-config circuit config))))

(define (simulate2 circuit config newconfig) ; config= original config, newconfig constantly changing, basically will keep oging till finding stable or finding wherew config = newconfig
  (cond
    [(stable? circuit newconfig) newconfig]
    [(equal? newconfig config)'none] ; if loops back to same thing over again, then will never be stable
    [else (simulate2 circuit config (next-config circuit newconfig))])); else, add present config on to next

;**********************************************************
; ** problem 9 ** (10 points)
; Define a 4-bit ripple-carry adder circuit as described in lecture
; using the circuit representation developed above.

; Please name it: add-ckt

; Its inputs are x3, x2, x1, x0, y3, y2, y1, y0  (in order)
; Its outputs are z4, z3, z2, z1, z0 (in order)
; What it computes is the sum of the two 4-bit binary numbers
; represented by the x's and the y's.
; For example, if the inputs are 
; x3 = 1, x2 = 0, x1 = 0, x0 = 1    (representing 9 in binary)
; y3 = 1, y2 = 1, y1 = 0, y0 = 1    (representing 13 in binary)
; then the output should be
; z4 = 1, z3 = 0, z2 = 1, z1 = 1, z0 = 0 (representing 22 in binary)

; Examples:
; (good-circuit? add-ckt) => #t
; (ckt-inputs add-ckt) => '(x3 x2 x1 x0 y3 y2 y1 y0)
; (ckt-outputs add-ckt) => '(z4 z3 z2 z1 z0)
; (output-values add-ckt (final-config add-ckt (init-config add-ckt '(1 0 0 1 1 1 0 1)))) => '(1 0 1 1 0)
; (output-values add-ckt (final-config add-ckt (init-config add-ckt '(0 1 1 1 0 1 1 0)))) => '(0 1 1 0 1)

; For the last two tests, your procedures output-values, final-config, init-config must be working.

; You may construct the circuit entirely by hand,
; or you may choose to write procedures to construct your circuit.
;**********************************************************

(define add-ckt
	(ckt '(x3 x2 x1 x0 y3 y2 y1 y0) '(z4 z3 z2 z1 z0) (list

                                                           ;0
                                                           
                                                           (gate 'xor '(x0 y0) 'z0); half-adder, actual value
                                                           (gate 'and '(x0 y0) 'c0) ; first remainder

                                                           ;1
                                                           
                                                           (gate 'xor '(x1 y1) 'xy0); adding first two
                                                           (gate 'xor '(c0 xy0) 'z1); full-adder, actual number
                                                           
                                                           (gate 'and '(x1 y1) 'xy1) ; finding carry over by adding all 3 together
                                                           (gate 'and '(y1 c0) 'c0y1)
                                                           (gate 'and '(x1 c0) 'c0x1)
                                                           (gate 'or '(xy1 c0y1) 'xc0yy1)
                                                           (gate 'or '(c0x1 xc0yy1) 'c1); found carry over,

                                                           ;2

                                                           (gate 'xor '(x2 y2) 'xy02); adding first two
                                                           (gate 'xor '(c1 xy02) 'z2); full-adder, actual number
                                                           
                                                           (gate 'and '(x2 y2) 'xy12) ; finding carry over by adding all 3 together
                                                           (gate 'and '(y2 c1) 'c1y2)
                                                           (gate 'and '(x2 c1) 'c1x2)
                                                           (gate 'or '(xy12 c1y2) 'xc1yy2)
                                                           (gate 'or '(c1x2 xc1yy2) 'c2); found carry over,

                                                           ;3

                                                           (gate 'xor '(x3 y3) 'xy03); adding first two
                                                           (gate 'xor '(c2 xy03) 'z3); full-adder, actual number
                                                           
                                                           (gate 'and '(x3 y3) 'xy13) ; finding carry over by adding all 3 together
                                                           (gate 'and '(y3 c2) 'c2y3)
                                                           (gate 'and '(x3 c2) 'c2x3)
                                                           (gate 'or '(xy13 c2y3) 'xc2yy3)
                                                           (gate 'or '(c2x3 xc2yy3) 'z4)))); 4

                                                           
;  "add-ckt is not defined yet")

;(define ha-ckt
;	(ckt '(x y) '(z co) (list (gate 'xor '(x y) 'z)
;                                  (gate 'and '(x y) 'co))))
;
;
;(define fa-ckt
;	(ckt '(x y ci) '(z co) (list (gate 'xor '(x y) 'xyz);xor returns odd? sum of three inputs
;                                     (gate 'xor '(ci xyz) 'z)
;                                     (gate 'and '(x y) 'xy)
;                                     (gate 'and '(y ci) 'yci)
;                                     (gate 'and '(ci x) 'cix)
;                                     (gate 'or '(xy yci) 'yy)
;                                     (gate 'or '(yy cix) 'co))))

;**********************************************************
; ** problem 10 ** (5 points)
; Define a D-flipflop as described in lecture, using the given representation of circuits.
; Please name it: dff-ckt.

; It has inputs:  s, d (in order) and outputs q, qc (in order)

; Examples
; (good-circuit? dff-ckt) => #t
; (ckt-inputs dff-ckt) => '(s d)
; (ckt-outputs dff-ckt) => '(q qc)
;> (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 0))))
;'(0 1)
;> (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 1))))
;'(1 0)
;**********************************************************

(define dff-ckt
	(ckt '(s d) '(q qc) (list
                             (gate 'nand '(s d) 'x)
                             (gate 'not '(d) 'dp)
                             (gate 'nand '(s dp) 'y)
                             (gate 'nand '(y q) 'qc)
                             (gate 'nand '(x qc) 'q))))
;  "dff-ckt is not defined yet")

;**********************************************************
; ** problem 11 ** (5 points)
; Design a circuit using the given representation that has
; no inputs and one output 't.  When the circuit is started
; in the initial (all zero) configuration, after a few configurations,
; the output is 1 for one step, then 0 for 4 steps, then 1 for
; one step, then 0 for 4 steps, then 1 for one step, and so on.

; Please name your circuit timing-ckt.

; Examples
; (good-circuit? timing-ckt) => #t
; (ckt-inputs timing-ckt) => '()
; (ckt-outputs timing-ckt) => '(t)
;> (map (lambda (config) (output-values timing-ckt config)) (simulate timing-ckt (init-config timing-ckt '()) 20))
;'((0) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1))
;**********************************************************


(define timing-ckt; instead, adds 4 1's and 1 zero
	(ckt '() '(t) (list
                       (gate 'not '(x3) 'x0)
                       (gate 'or '(x0 x0) 'x1)
                       (gate 'not '(x1) 'x1)
                       (gate 'or '(x1 x1) 'x2)
                       (gate 'not '(x2) 'x2)
                       (gate 'and '(x2 x2) 't)
                       (gate 'not '(t) 'x3))))

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
			    '***OK***
			    '***X***)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(test 'hours hours (lambda (x) (> x 0)))



(test 'good-gate? (good-gate? gate1) #t)
(test 'good-gate? (good-gate? gate2) #t)
(test 'good-gate? (good-gate? gate3) #t)
(test 'good-gate? (good-gate? gate4) #t)

(test 'good-gate? (good-gate? (gate 'not 'x 'y)) #f)
(test 'good-gate? (good-gate? (gate 'nor '("x" "y") "z")) #f)
(test 'good-gate? (good-gate? (gate 'and '(1 2) 3)) #f)
(test 'good-gate? (good-gate? (gate 'equal '(x y) 'z)) #f)
(test 'good-gate? (good-gate? (gate 'or '(w x y) 'z)) #f)

(test 'good-circuit? (good-circuit? sel-ckt) #t)
(test 'good-circuit? (good-circuit? eq1-ckt) #t)
(test 'good-circuit? (good-circuit? eq2-ckt) #t)
(test 'good-circuit? (good-circuit? latch-ckt) #t)
(test 'good-circuit? (good-circuit? seq-or-ckt) #t)
(test 'good-circuit? (good-circuit? clock-ckt) #t)

(test 'good-circuit? (good-circuit? 'hi) #f)
(test 'good-circuit? (good-circuit? (ckt '() '() '())) #t)
(test 'good-circuit? (good-circuit? (ckt '(x y) '(z) (list (gate 'and '(x y) 'x) (gate 'or '(x y) 'z)))) #f)
(test 'good-circuit? (good-circuit? (ckt '(x y) '(z) (list (gate 'nor '(x y) 'z) (gate 'nand '(x y) 'z)))) #f)
(test 'good-circuit? (good-circuit? (ckt '(x y) '(u z) (list (gate 'or '(x y) 'z)))) #f)


(test 'all-wires (all-wires eq1-ckt) '(x y z cx cy t1 t2))
(test 'all-wires (all-wires sel-ckt) '(x1 x0 y1 y0 s z1 z0 sc u1 v1 u0 v0))
(test 'find-gate (find-gate 't2 eq1-ckt) (gate 'and '(cx cy) 't2))
(test 'find-gate (find-gate 'w eq2-ckt) (gate 'xor '(x y) 'w))
(test 'find-gate (find-gate 'y sel-ckt) #f)

(test 'ha-ckt (good-circuit? ha-ckt) #t)
(test 'fa-ckt (good-circuit? fa-ckt) #t)
(test 'ha-ckt (ckt-inputs ha-ckt) '(x y))
(test 'ha-ckt (ckt-outputs ha-ckt) '(z co))
(test 'fa-ckt (ckt-inputs fa-ckt) '(x y ci))
(test 'fa-ckt (ckt-outputs fa-ckt) '(z co))

(test 'ha-ckt (output-values ha-ckt 
			     (final-config ha-ckt 
					   (init-config ha-ckt '(1 1)))) 
      '(0 1))
(test 'fa-ckt (output-values fa-ckt (final-config fa-ckt (init-config fa-ckt '(1 1 1)))) '(1 1))

(test 'next-value (next-value 'cx eq1-ckt eq1-config1) 1)
(test 'next-value (next-value 't2 eq1-ckt eq1-config1) 0)
(test 'next-value (next-value 'z eq1-ckt eq1-config2) 0)
(test 'next-value (next-value 'x0 sel-ckt sel-config1) 1)
(test 'next-value (next-value 'v1 sel-ckt sel-config1) 1)
(test 'next-value (next-value 'v0 sel-ckt sel-config2) 0)


(test 'next-config (next-config eq1-ckt eq1-config1)
      (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'cx 1) (entry 'cy 0) (entry 't1 0) (entry 't2 0)))

(test 'next-config (next-config eq1-ckt eq1-config2)
      (list (entry 'x 0) (entry 'y 0) (entry 'z 0) (entry 'cx 1) (entry 'cy 1) (entry 't1 0) (entry 't2 1)))

(test 'next-config (next-config sel-ckt sel-config1)
      (list
       (entry 'x1 0)
       (entry 'x0 1)
       (entry 'y1 1)
       (entry 'y0 0)
       (entry 's 1)
       (entry 'z1 0)
       (entry 'z0 0)
       (entry 'sc 0)
       (entry 'u1 0)
       (entry 'v1 1)
       (entry 'u0 0)
       (entry 'v0 0)))

(test 'next-config (next-config sel-ckt (next-config sel-ckt sel-config1))
      (list
       (entry 'x1 0)
       (entry 'x0 1)
       (entry 'y1 1)
       (entry 'y0 0)
       (entry 's 1)
       (entry 'z1 1)
       (entry 'z0 0)
       (entry 'sc 0)
       (entry 'u1 0)
       (entry 'v1 1)
       (entry 'u0 0)
       (entry 'v0 0)))

(test 'next-config (next-config latch-ckt latch-config1) (list (entry 'x 0) (entry 'y 0) (entry 'q 1) (entry 'u 1)))

(test 'next-config (next-config latch-ckt latch-config2) (list (entry 'x 0) (entry 'y 1) (entry 'q 1) (entry 'u 0)))


(test 'stable? (stable? eq1-ckt (list (entry 'x 0) (entry 'y 0) (entry 'z 1) (entry 'cx 1) (entry 'cy 1) (entry 't1 0) (entry 't2 1))) #t)
(test 'stable? (stable? eq1-ckt (list (entry 'x 0) (entry 'y 0) (entry 'z 0) (entry 'cx 1) (entry 'cy 0) (entry 't1 1) (entry 't2 0))) #f)

(test 'all-stable-configs (all-stable-configs eq2-ckt)
      (list
       (list (entry 'x 0) (entry 'y 0) (entry 'z 1) (entry 'w 0))
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'w 1))
       (list (entry 'x 1) (entry 'y 0) (entry 'z 0) (entry 'w 1))
       (list (entry 'x 1) (entry 'y 1) (entry 'z 1) (entry 'w 0))))

(test 'all-stable-configs (all-stable-configs seq-or-ckt)
      (list (list (entry 'x 0) (entry 'z 0)) (list (entry 'x 0) (entry 'z 1)) (list (entry 'x 1) (entry 'z 1))))

(test 'output-values (output-values eq1-ckt eq1-config2) '(0))
(test 'output-values (output-values latch-ckt latch-config2) '(1 0))
(test 'init-config (init-config eq1-ckt '(1 0)) (list (entry 'x 1) (entry 'y 0) (entry 'z 0) (entry 'cx 0) (entry 'cy 0) (entry 't1 0) (entry 't2 0)))
(test 'init-config (init-config clock-ckt '()) (list (entry 'z 0)))


(test 'simulate (simulate clock-ckt (list (entry 'z 0)) 4)
      (list
       (list (entry 'z 0))
       (list (entry 'z 1))
       (list (entry 'z 0))
       (list (entry 'z 1))
       (list (entry 'z 0))))

(test 'simulate (simulate eq1-ckt eq1-config1 5)
      (list
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'cx 0) (entry 'cy 0) (entry 't1 0) (entry 't2 0))
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'cx 1) (entry 'cy 0) (entry 't1 0) (entry 't2 0))))

(test 'simulate (simulate sel-ckt sel-config1 5)
      (list
       (list
	(entry 'x1 0)
	(entry 'x0 1)
	(entry 'y1 1)
	(entry 'y0 0)
	(entry 's 1)
	(entry 'z1 0)
	(entry 'z0 0)
	(entry 'sc 0)
	(entry 'u1 0)
	(entry 'v1 0)
	(entry 'u0 0)
	(entry 'v0 0))
       (list
	(entry 'x1 0)
	(entry 'x0 1)
	(entry 'y1 1)
	(entry 'y0 0)
	(entry 's 1)
	(entry 'z1 0)
	(entry 'z0 0)
	(entry 'sc 0)
	(entry 'u1 0)
	(entry 'v1 1)
	(entry 'u0 0)
	(entry 'v0 0))
       (list
	(entry 'x1 0)
	(entry 'x0 1)
	(entry 'y1 1)
	(entry 'y0 0)
	(entry 's 1)
	(entry 'z1 1)
	(entry 'z0 0)
	(entry 'sc 0)
	(entry 'u1 0)
	(entry 'v1 1)
	(entry 'u0 0)
	(entry 'v0 0))))

(test 'simulate (simulate latch-ckt latch-config2 3)
      (list (list (entry 'x 0) (entry 'y 1) (entry 'q 1) (entry 'u 0))))

(test 'simulate (simulate eq2-ckt (init-config eq2-ckt '(0 1)) 5)
      (list
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'w 0))
       (list (entry 'x 0) (entry 'y 1) (entry 'z 1) (entry 'w 1))
       (list (entry 'x 0) (entry 'y 1) (entry 'z 0) (entry 'w 1))))

(test 'final-config (final-config clock-ckt (list (entry 'z 0)))
      'none)

(test 'final-config (final-config eq1-ckt (list (entry 'x 1) (entry 'y 1) (entry 'z 0) (entry 'cx 0) (entry 'cy 0) (entry 't1 0) (entry 't2 0)))
      (list (entry 'x 1) (entry 'y 1) (entry 'z 1) (entry 'cx 0) (entry 'cy 0) (entry 't1 1) (entry 't2 0)))

(test 'final-config (final-config sel-ckt (list (entry 'x1 0) (entry 'x0 0) (entry 'y1 1) (entry 'y0 0) (entry 's 0) (entry 'z1 1) (entry 'z0 1) (entry 'sc 0) (entry 'u1 1) (entry 'v1 1) (entry 'u0 0) (entry 'v0 1)))
      (list
       (entry 'x1 0)
       (entry 'x0 0)
       (entry 'y1 1)
       (entry 'y0 0)
       (entry 's 0)
       (entry 'z1 0)
       (entry 'z0 0)
       (entry 'sc 1)
       (entry 'u1 0)
       (entry 'v1 0)
       (entry 'u0 0)
       (entry 'v0 0)))
      
(test 'final-config (final-config latch-ckt (list (entry 'x 1) (entry 'y 1) (entry 'q 0) (entry 'u 0)))
      'none)

(test 'final-config (final-config latch-ckt (list (entry 'x 1) (entry 'y 1) (entry 'q 0) (entry 'u 1)))
      (list (entry 'x 1) (entry 'y 1) (entry 'q 0) (entry 'u 1)))


(test 'add-ckt (good-circuit? add-ckt) #t)
(test 'add-ckt (ckt-inputs add-ckt) '(x3 x2 x1 x0 y3 y2 y1 y0))
(test 'add-ckt (ckt-outputs add-ckt) '(z4 z3 z2 z1 z0))
(test 'add-ckt (output-values add-ckt (final-config add-ckt (init-config add-ckt '(1 0 0 1 1 1 0 1)))) '(1 0 1 1 0))
(test 'add-ckt (output-values add-ckt (final-config add-ckt (init-config add-ckt '(0 1 1 1 0 1 1 0)))) '(0 1 1 0 1))


(test 'dff-ckt (good-circuit? dff-ckt) #t)
(test 'dff-ckt (ckt-inputs dff-ckt) '(s d))
(test 'dff-ckt (ckt-outputs dff-ckt) '(q qc))
(test 'dff-ckt (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 0)))) '(0 1))
(test 'dff-ckt (output-values dff-ckt (final-config dff-ckt (init-config dff-ckt '(1 1)))) '(1 0))

 
(test 'timing-ckt (good-circuit? timing-ckt)  #t)
(test 'timing-ckt (ckt-inputs timing-ckt)  '())
(test 'timing-ckt (ckt-outputs timing-ckt)  '(t))
(test 'timing-ckt (map (lambda (config) (output-values timing-ckt config)) (simulate timing-ckt (init-config timing-ckt '()) 20))
      '((0) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1) (0) (0) (0) (0) (1)))


;**************  end of hw # 5  ************************************
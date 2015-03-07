(load "simpleParser.scm")

; test
(define testvalid
  (lambda ()
    (list 
     (interpret "1.txt") (interpret "2.txt") (interpret "3.txt") (interpret "4.txt") (interpret "5.txt") (interpret "6.txt") (interpret "7.txt") (interpret "8.txt") (interpret "9.txt") (interpret "10.txt")
     (interpret "11.txt") (interpret "12.txt"))))    

; start feeds the interpreters output to Mst
(define interpret
  (lambda (name)
    (valueofwrap 'return (Mstatelist (parser name) (newenv)))))

; main Mstate wrapper
(define Mstatelist 
  (lambda (exp st)
    (cond
      ((null? exp) st); (valueof 'return st))
      (else (Mstatelist (cdr exp) (Mst (car exp) st))))))


; ------------------------------------------<
; M State functions
;

; main Mstate function that directs the rest of the Mstates, called by interpret
(define Mst
  (lambda (exp st)
    (cond
      ((null? exp)   st)
      ((eq? 'var     (operator exp)) (Mst_declare  exp st))
      ((eq? '=       (operator exp)) (Mst_assign   exp st))
      ((eq? 'return  (operator exp)) (Mst_return   exp st))
      ((eq? 'if      (operator exp)) (Mst_if       exp st))
      ((eq? 'begin   (operator exp)) (Mst_begin    exp st))
      ((eq? 'while   (operator exp)) (Mst_while    exp st))
      (else (error 'incorrect-command-identifier-in-code)))))

; '(var x expression) and '(var x)
; Declare variable (place in state with corresponding value), if doesn't have value then 'undefined
; (Mst_declare '(var x) '((x) (10)))
; (Mst_declare '(var x) '(()()))
; (Mst_declare '(var x 10) '(()()))
; (Mst_declare '(var x (* 2 10)) '(()()))
; (Mst_declare '(var x (* x 20)) '((x)(10)))
(define Mst_declare
  (lambda (exp st)
    (cond
      ; prevent redefining
      ((in? (leftoperand exp) st) (error 'redefining))
      ; if right operand is null add left operand to state with undefined
      ((null? (rightoperand exp)) (addst (leftoperand exp) 'undefined st))
      ; if right operand is not null, add left value to state with (Mvalwrap (rightoperand exp) ... (must be Mvalwrap because dont want to save state as true or false, but #t and #f
      (else (addst (leftoperand exp) (Mvalwrap (rightoperand exp) st) st)))))

; (Mst_assign '(= x 10) '(() ()))
; (Mst_assign '(= x 10) '((x) (4)))
(define Mst_assign
  (lambda (exp st)
    (cond
      ((in? (leftoperand exp) st) (replacest (leftoperand exp) (Mvalwrap (rightoperand exp) st) st))
      (else (error 'declare-your-variables-before-assigning-it-a-value)))))

; '(return expression)
; create new return variable and put it in state)
; (Mst_return '(return 10) '(()()))
; (Mst_return '(return (* 10 x)) '((x) (9)))
(define Mst_return
  (lambda (exp st)
    (addst (operator exp) (Mval (leftoperand exp) st) st)))

; (Mst_if '(if (>= x y) (= m x) (= m y)) '((x y m) (1 2 0)))
; (Mst_if '(if (== x y) (= x 10)) '((x y) (5 6)))
; (Mst_if '(if (|| (! z) false) (= z (! z)) (= z z)) '((x y z) (10 20 true))
; cadr = condition, caddr = statement1, cadddr = statement2
(define Mst_if
  (lambda (exp st)
    (cond
      ((Mvalwrap (cadr exp) st) (Mst (caddr exp) st))                 ; if cond true
      ((and (null? (cdddr exp)) (not (Mvalwrap (cadr exp) st))) st)   ; 
      (else (Mst (cadddr exp) st)))))

; (Mst_while
(define Mst_while
  (lambda (exp st)
    (while (leftoperand exp) (rightoperand exp) st)))

(define while
  (lambda (cond body st)
    (letrec ((loop (lambda (cond body state)
                     (if (Mbool1 cond state)
                         (loop cond body
                               (Mst body state))
                         state))))
                         (loop cond body st))))

; (Mst_begin
(define Mst_begin
  (lambda (exp st)
    (cond
      ((null? exp) st)
      (else (removelayer (Mstatelist (cdr exp) (addlayer st)))))))
            

; ------------------------------------------<
; Environment
;

; adds a new layer to the state
; (addlayer (newenv))
; (addlayer '((() ()) ((() ()))))
;    ==> ((() ()) ((() ()) ((() ()))))
; (addlayer '((() ()) ((() ()) ((() ())))))
;    ==> ((() ()) ((() ()) ((() ()) ((() ())))))
; (addlayer '((() ()) ((() ()) ((() ()) ((() ()))))))
; (addlayer '(( (x)(1) ) (((y)(2)) (((z)(3))))))
(define addlayer
  (lambda (st)
    (cons (newlayer) (list st))))

; removes most recently added layer in state
; will only ever be called on multiple layers
; (removelayer '(((y) (2)) (((z) (3)))))
; (removelayer '(( (x)(1) ) (((y)(2)) (((z)(3)) ))))
(define removelayer
  (lambda (st)
    (cond
      ((or (isempty? st)
           (not (islayered? st))) st)      ; remove layers only up to '((()())) or singly layered
      (else (car (cons (car (cdr st)) (cdr (cdr st))))))))
                   
; asks if the state is empty
; (isempty? '((()())))
; (isempty? '( ((x)(1)) (((y)(2)) (((z)(3)) ))))
(define isempty?
  (lambda (st)
    (cond
      ((null? st) #t)
      ((list? (car st)) (and (isempty? (car st)) (isempty? (cdr st))))
      (else #f))))

; asks if the state has been layered
; (islayered? '(()()))
; (islayered? '(( ()() ))
; (islayered? '((()()) ((()()) ((()())))))
(define islayered?
  (lambda (st)
    (cond
      ((null? (cdr st)) #f)                                     ; single 'environment'
      ((not (null? (cdr st))) (not (null? (cadr st))))          ; single 'layer' (#f)
      (else #t))))


; adds variable and value to [current scope] :: ((car (car st)) (cadr (car st))) :: (((variables) (values)) (lower level states))
; replaces variable if state already exists
; (addst 'x '10 '((()())))
; (addst 'z '1000 (addlayer '(((y) (100)) (((x) (10))))))
; (addst 'z '1000 '(((y) (100)) (((x) (10)))))
(define addst
  (lambda (variable expv st)
    (cond
      ((in? variable st) (replacest (addst variable expv (car (removest variable st))) (cdr st)))
      (else (cons (list 
             (addtoend variable (operator (car st)))
             (addtoend expv (leftoperand (car st))))
                  (cdr st))))))

; removes variable and corresponding value from state
; (removest 'r '(((a b r c) (1 2 3 4))))
; (removest 'r '( ((a b)(1 2)) (((c r d)(3 6 4)) (((z) (1))))))
; (removest 'z '( ((a b)(1 2)) (((c r d)(3 6 4)) (((z) (1))))))
(define removest
  (lambda (variable st)
    (cond
      ((isempty? st) st)
      ((inl? variable (car st)) 
       (cond
         ((islayered? st) (list (cons (removestl variable (car st)) (cdr st))))
         (else (list (removestl variable (car st))))))
      ((islayered? st) (cons (car st) (removest variable (car (cdr st)))))
      (else (list (car st) (removest variable (car (cdr st))))))))

; removes variable and corresponding value from layer in state
; (removest 'r '((y x f j r u i l) (2 5 9 1 2 3 5 21)))
; (replacest 'x '20 '(((y z) (2 20)) (((x) (20)))))
(define removestl
  (lambda (variable st)
    (cond
      ((null? (operator st)) (newlayer))
      ((eq? variable (car (operator st))) (removestl variable (cdrcdr st)))
      (else (list 
             (cons (car (operator st)) (car (removestl variable (cdrcdr st))))
             (cons (car (leftoperand st)) (cadr (removestl variable (cdrcdr st)))))))))

; replace variable's current value with exp maintaining state
; add variable and (expression evaluated with current state) into (st that has just removed current 'variable's state)
; (replacest 'x '40 '(((y x z r) (2 5 10 20))))
; (replacest 'r '9999 '( ((a b)(1 2)) (((c r d)(3 6 4)) (((z) (1))))))
; (replacest 'x '999 '(((y z) (2 20)) (((x) (20)))))
; TODO: rewrite this to replace value in place instead of removing it and adding it blindly
(define replacest
  (lambda (variable expv st)
    (cond 
      ((isempty? st) st)
      ((inl? variable (car st))
       (cond
         ((islayered? st) (list (cons (replacestl variable expv (car st)) (cdr st))))
         (else (list (replacestl variable expv (car st))))))
      (else (list (car st) (replacest variable expv (car (cdr st))))))))


; replaces variable's old value with new value in a layer
; (replacestl 'x '40 '((y x z r) (2 5 10 20)))
; variable => r
;expv => 9999
;st => (((c r d) (3 6 4)) (((z) (1))))
(define replacestl
  (lambda (variable expv st)
    (cond 
      ((null? (operator st)) (newlayer))
      ((eq? variable (car (operator st))) (list 
                                           (cons (car (operator st)) (cdr (operator st)))
                                           (cons expv (cdr (leftoperand st)))))
      (else (list 
             (cons (car (operator st)) (car (replacestl variable expv (cdrcdr st))))
             (cons (car (leftoperand st)) (cadr (replacestl variable expv (cdrcdr st)))))))))

; valueof wrap returns not #t or #f but true or false when called
(define valueofwrap
  (lambda (variable env)
    (cond
      ; if (and (variable is return) (return value is boolean)) return Mval of the variable (Mval returns true or false, not #t or #f
      ((and 
       (eq? variable 'return)
       (or (eq? (valueof 'return env) #t) 
        (eq? (valueof 'return env) #f))) (Mval 'return env))
      (else (valueof variable env)))))
      
; returns the value of a variable thats in the state
; (valueof 'r '(((y x f j r u i l) (2 5 9 1 2 3 5 21))))
; (valueof 'r '( ((a b)(1 2)) (((c r d)(3 6 4)) (((z) (1))))))
(define valueof
  (lambda (var env)
    (cond 
      ((isempty? (operator (car env))) #f)      ; hopefully will never be called
      ((inl? var (car env)) (valueofl var (car env)))
      ((islayered? env) (valueof var (car (cdr env))))
      (else #f))))

; returns the value of a variable thats in the first layer
; (valueofl 'r '((y x f j r u i l) (2 5 9 1 2 3 5 21)))
(define valueofl
  (lambda (variable env)
    (cond
      ((isempty? (operator env)) #f)      ; hopefully will never be called
      ((eq? variable (car (operator env))) (car (leftoperand env)))
      (else (valueofl variable (cdrcdr env))))))
      
; is the variable present in env? has it been declared? (use env when not modifying state) 
; (in? 'x '(((y x z) (1 2 3))))
; (in? 'x '(()()))
; (in? 'x '(((x)(1))))
; (in? 'x '(((y)(1))))
; (in? 'x '((()())))
; (in? 'x '(   ((a b)(1 2))  ( ((z)(1))   (((y)(2))   (((x)(1)))))))
; (in? 'x '((()()) ((()()) ((()())))))
(define in?
  (lambda (variable env)
    (cond
      ((void? env) #f)
      ((isempty? env) #f)
      ;((null? (car env)) #f)
      ((null? (cdr env)) (inl? variable (car env)))
      ((not (islayered? env)) (inl? variable (car env)))
      (else (or (inl? variable (car env)) (in? variable (car (cdr env))))))))

; is the variable present in a layer in the environment?
; (inl? 'x '((a b x)(1 4 2)))
; (inl? 'x '(()()))
(define inl?
  (lambda (variable env)
    (cond
      ((null? (car env)) #f)    ; (*()* ())
      ((eq? variable (car (operator env))) #t)
      (else (inl? variable (cdrcdr env))))))  ; if first element of (car st) is not the variable, new state is just cons of (trim first element from car and cdr of env)

; trim the first element off (operator st) and (leftoperand st)
; (cdrcdr '(()()))
; (cdrcdr '((1 2 3) (4 5 6)))
(define cdrcdr
  (lambda (env)
    (cond
      ((null? env) (newenv))
      ((isempty? env) env)
      (else (list 
             (cdr (operator env)) 
             (cdr (leftoperand env)))))))
      
; add a to end of l
; (addtoend 'x '(1 2 3 a b c s))
(define addtoend
  (lambda (a l)
    (cond
      ((null? l) (list a))
      (else (cons (car l) (addtoend a (cdr l)))))))

; asks if `x` is an atom
(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


; ------------------------------------------<
; M Value
;

; wrapper for true and false
(define Mval 
  (lambda (exp st)
    (cond
      ((eq? (Mvalwrap exp st) #t) 'true)
      ((eq? (Mvalwrap exp st) #f) 'false)
      (else (Mvalwrap exp st)))))

; M_value takes an expression, a state and returns its evaluation (#t, #f)
; (Mval '(+ 1 (* 5 x)) '((x) (10)))

; TODO: figure out why this error is happening
;      (Mst_assign '(= z x) '(((x y z)(#t #f undefined))))


(define Mvalwrap
  (lambda (exp state)
    (cond
      ((number? exp) exp)                     ; expression is number
      
      ((and                                   
        (in? exp state)                          ; in state
        (eq? (valueof exp state) 'undefined))    ; undefined
       (error 'make-sure-your-variables-are-defined-with-a-value))
      
        
      ((in? exp state) (valueof exp state))   ; expression is variable in state and defined
      
      ((and                       ; expression is not in state ^
        (not (void? state))
        (not (list? exp))         ; not a complex expression
        (not (number? exp))       ; not a number
        (atom? exp)               ; is an atom
        (not (in? exp state)))     
       (error 'make-sure-your-variables-are-declared))
       
      ((eq? '+ (operator exp)) (+ (Mvalwrap (leftoperand exp) state) (Mvalwrap (rightoperand exp) state)))
      ((eq? '/ (operator exp)) (quotient (Mvalwrap (leftoperand exp) state) (Mvalwrap (rightoperand exp) state)))
      ((eq? '% (operator exp)) (remainder (Mvalwrap (leftoperand exp) state) (Mvalwrap (rightoperand exp) state)))
      ((eq? '- (operator exp)) (Mval_unary_neg exp state))
      ((eq? '* (operator exp)) (* (Mvalwrap (leftoperand exp) state) (Mvalwrap (rightoperand exp) state)))
      
      (else (Mbool1 exp state)))))

; supports the unary negation operator
(define Mval_unary_neg
  (lambda (exp state)
    (cond
      ((null? (rightoperand exp)) (- 0 (Mval (leftoperand exp) state)))       ; 7 - 7 = 0 -> - 7  = -7
      (else (- (Mval (leftoperand exp) state) (Mval (rightoperand exp) state))))))


; ------------------------------------------<
; M Boolean
;

; (Mbool '(< (+ 1 2) (* 10 x)) '((x)(10)))
; (Mbool '(&& (< 1 2) (< 2 1)) '((x)(10)))
; (Mbool '(!= (+ 1 2) (+ 2 1)) '((x)(10)))
(define Mbool1
  (lambda (exp st)
    (cond
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((number? exp) (Mvalwrap exp st))
      ((in? exp st) (valueof exp st))

      ((eq? '&& (operator exp)) (and (Mbool1 (leftoperand exp) st) (Mbool1 (rightoperand exp) st)))
      ((eq? '|| (operator exp)) (or (Mbool1 (leftoperand exp) st) (Mbool1 (rightoperand exp) st)))
      ((eq? '!  (operator exp)) (not (Mbool1 (leftoperand exp) st)))
      
      ((eq? '== (operator exp)) (eq? (Mvalwrap (leftoperand exp) st) (Mvalwrap (rightoperand exp) st)))
      ((eq? '!= (operator exp)) (not (eq? (Mvalwrap (leftoperand exp) st) (Mvalwrap (rightoperand exp) st))))
      
      ((eq? '<  (operator exp)) (< (Mvalwrap (leftoperand exp) st) (Mvalwrap (rightoperand exp) st)))
      ((eq? '>  (operator exp)) (> (Mvalwrap (leftoperand exp) st) (Mvalwrap (rightoperand exp) st)))
      ((eq? '>= (operator exp)) (>= (Mvalwrap (leftoperand exp) st) (Mvalwrap (rightoperand exp) st)))
      ((eq? '<= (operator exp)) (<= (Mvalwrap (leftoperand exp) st) (Mvalwrap (rightoperand exp) st)))
      
      (else (error 'bad-operator)))))


; ------------------------------------------<
; Abstractions
;

; helper functions to get the first list in st and second
;  for some reason these didn't work in anything other than #lang racket, so changed all instances of first and second to operator and leftoperand respectively
;(define first car)
;(define second cadr)

; the helper functions to determine where the operator and operands are depending on the form



; returns new environment
(define newenv (lambda () '(( ()() )) ))
; returns new layer
(define newlayer (lambda () '(()())))

(define operator car)
(define leftoperand cadr)
(define rightoperand 
  (lambda (exp)
    (cond
      ((null? (cddr exp)) '())      ; unary
      (else (car (cddr exp))))))
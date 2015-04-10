(load "simpleParser.scm")

; test
(define testvalid
  (lambda ()
    (list 
     (interpret "1.txt") (interpret "2.txt") (interpret "3.txt") (interpret "4.txt") (interpret "5.txt") (interpret "6.txt") (interpret "7.txt") (interpret "11.txt") (interpret "12.txt"))))    

; start feeds the interpreters output to Mst
(define interpret
  (lambda (name)
    (formatoutput (call/cc
                   (lambda (return)
                     (Mstatelist (parser name) (newenv) return (lambda (b) b) (lambda (c) c)))))))

(define formatoutput
  (lambda (condition)
    (cond
      ((eq? #t condition) 'true)
      ((eq? #f condition) 'false)
      (else condition))))

; main Mstate wrapper
(define Mstatelist 
  (lambda (exp st return continue break)
    (cond
      ((null? exp) st) 
      (else (Mstatelist (cdr exp) (Mst (car exp) st return break continue) return break continue)))))

(define defaultbreak
  (lambda (b) b))

(define defaultcontinue
  (lambda (c) c))

; ------------------------------------------<
; M State functions
; 

; main Mstate ƒunction that directs the rest of the Mstates, called by interpret
(define Mst
  (lambda (exp st return break continue)
    (cond
      ((null? exp)   st)
      ((eq? 'break    (operator exp)) (Mst_break    st break))
      ((eq? 'continue (operator exp)) (Mst_continue st continue))
      ((eq? 'var      (operator exp)) (Mst_declare  exp st))
      ((eq? '=        (operator exp)) (Mst_assign   exp st))
      ((eq? 'return   (operator exp)) (Mst_return   exp st return))
      ((eq? 'if       (operator exp)) (Mst_if       exp st return break continue))
      ((eq? 'begin    (operator exp)) (Mst_begin    exp st return break continue))
      ((eq? 'while    (operator exp)) (Mst_while    exp st return break continue))
      (else (error 'out-of-place-command-identifier-in-code)))))

; '(var x expression) and '(var x)
; Declare variable (place in state with corresponding value), if doesn't have value then 'undefined
(define Mst_declare
  (lambda (exp st)
    (cond
      ; prevent redefining
      ((in? (leftoperand exp) st) (error 'redefining))
      ; if right operand is null add left operand to state with undefined
      ((null? (rightoperand exp)) (addst (leftoperand exp) 'undefined st))
      ; if right operand is not null, add left value to state with (Mval (rightoperand exp) ... (must be Mval because dont want to save state as true or false, but #t and #f
      (else (addst (leftoperand exp) (Mval (rightoperand exp) st) st)))))

; Assign a value to an existing variable (throw error if doesn't exist)
(define Mst_assign
  (lambda (exp st)
    (cond
      ((in? (leftoperand exp) st) (replacest (leftoperand exp) (Mval (rightoperand exp) st) st))
      (else (error 'declare-your-variables-before-assigning-it-a-value)))))

; '(return expression)
; create new return variable and put it in state)
(define Mst_return
  (lambda (exp st return)
    (if (null? exp) 
        state
        (return (Mval (leftoperand exp) st)))))

; (Mst_if '(if (|| (! z) false) (= z (! z)) (= z z)) '((x y z) (10 20 true))
; cadr = condition, caddr = statement1, cadddr = statement2
(define Mst_if
  (lambda (exp st return break continue)
    (cond
      ((Mval (cadr exp) st) (Mst (caddr exp) st return break continue))                 ; if cond true
      ((and (null? (cdddr exp)) (not (Mval (cadr exp) st))) st)   ; 
      (else (Mst (cadddr exp) st return break continue)))))

; (Mst_while
(define Mst_while
  (lambda (exp st return break continue)
    (while (leftoperand exp) (rightoperand exp) st return)))

(define while
  (lambda (c b st return)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (cond body state)
                                (if (Mbool1 cond state)
                                    (loop cond body (call/cc (lambda (continue) 
                                                     (Mst body state return break continue))))
                                state))))
               (loop c b st))))))

; (Mst_begin
(define Mst_begin
  (lambda (exp st return break continue)
    (cond
      ((null? exp) st)
      (else (removelayer (Mstatelist (cdr exp) (addlayer st) return (lambda (v) (break (removelayer v))) (lambda (v) (continue (removelayer v)))))))))

; (Mst_break
(define Mst_break
  (lambda (st break)
    (break st)))

; (Mst_continue
(define Mst_continue
  (lambda (st continue)
    (continue st)))

; Mst_funclosure
(define Mst_funclosure
  (lambda (syntax st return break continue)
    (if (in? (leftoperand syntax) st)
        (error 'cannot-declare-function-more-than-once)
        (addst (leftoperand syntax) (makeclosure syntax st) st))))
  
; helper ƒunction to make closure
(define makeclosure
  (lambda (function st)
    (list
     (rightoperand function)
     (funbody function)
     (mktrimmer (length st)))))

; makes a ƒunction that takes the current calling environment returns the environment the function is declared in
; (mktrimmer (length '(((a) (1))))
(define mktrimmer
  (lambda (n)
    ((lambda (m)
       (m m))
     (lambda (trim)
       (lambda (env)
         (cond
           ((eq? n (length env)) env)
           (else ((trim trim) (cdr env)))))))))
  

; ------------------------------------------<
; Environment
;

; adds a new layer to the state
; (addlayer (newenv))
; (addlayer '((() ()) (() ())))
;    ==> ((() ()) (() ()) (() ()))
(define addlayer
  (lambda (st)
    (cons (newlayer) st)))

; removes most recently added layer in state
; will only ever be called on multiple layers
; (removelayer '(((y) (2)) ((z) (3))))
; (removelayer '(((x)(1)) ((y)(2)) ((z)(3)) ))
(define removelayer
  (lambda (st)
    (cdr st)))
                   
; asks if the state is empty
; (isempty? '((()())))
; (isempty? '((() ()) ((x) (0))))
; (isempty? '( ((x)(1)) ((y)(2)) ((z)(3)) ))
; (isempty? '((() ()) ((x) (0))))
(define isempty?
  (lambda (st)
    (cond
      ((null? st) #t)
      ((>= (length st) 1) #f)
      ((and (list? st) (list? (car st))) (and (isempty? (car st)) (isempty? (cdr st))))
      (else #f))))

; asks if the state has been layered
; (islayered? '(()()))
; (islayered? '(( ()() )))
; (islayered? '((()()) (()()) (()())) )
(define islayered?
  (lambda (st)
    (cond
      ((null? (cdr st)) #f)                                     ; single 'environment'
      ((not (null? (cdr st))) (not (null? (cadr st))))          ; single 'layer' (#f)
      (else #t))))

; asks if the environment passed in is a layer. if its a new environment, or layered environment, then #f
; only ever called with either '(()()) or '((()()))
(define islayer?
  (lambda (env)
    (cond
      ((eq? (length env) 1) #f)
      (else #t))))

; length of list (number of elements in the list
;(define length
;  (lambda (l)
;    (cond
;      ((null? l) 0)
;      (else (+ 1 (length (cdr l)))))))
      

; adds variable and value to [current scope] :: ((car (car st)) (cadr (car st))) :: (((variables) (values)) (lower level states))
; replaces variable if state already exists
; (addst 'x '10 '((()())))
; (addst 'z '1000 (addlayer '(((y) (100)) (((x) (10))))))
; (addst 'z '1000 '(((y) (100)) (((x) (10)))))
(define addst
  (lambda (variable expv st)
    (cond
      ((in? variable st) (replacest variable expv st))
      (else (cons (list 
             (addtoend variable (operator (car st)))
             (addtoend (box expv) (leftoperand (car st))))
                  (cdr st))))))

; i dont actually ever call removest..
; removes variable and corresponding value from state
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
(define replacest
  (lambda (variable expv st)
    (cond 
      ((isempty? st) st)
      ((not (in? variable st)) st)
      ((inl? variable (car st))
       (cond
         ((islayered? st) (cons (replacestl variable expv (car st)) (cdr st)))
         (else (list (replacestl variable expv (car st))))))
      (else (cons (car st) (replacest variable expv (cdr st)))))))


; replaces variable's old value with new value in a layer
(define replacestl
  (lambda (variable expv st)
    (cond 
      ((null? (operator st)) (newenv))
      ((eq? variable (car (operator st))) (begin (set-box! (car (leftoperand st)) expv) st))
      (else (list 
             (cons (car (operator st)) (car (replacestl variable expv (cdrcdr st))))
             (cons (car (leftoperand st)) (cadr (replacestl variable expv (cdrcdr st)))))))))

      
; returns the value of a variable thats in the state
(define valueof
  (lambda (var env)
    (cond 
      ((isempty? env) #f)      ; hopefully will never be called
      ((null? (cdr env)) (valueofl var (car env)))  ; last layer        
      ((inl? var (car env)) (valueofl var (car env)))
      ((islayered? env) (valueof var (cdr env)))    ; layered and not in first layer, then check other layers
      (else (valueofl var (car env))))))            ; last layer

; returns the value of a variable thats in the first layer
(define valueofl
  (lambda (variable env)
    (cond
      ((isempty? (operator env)) #f)      ; hopefully will never be called
      ((and (not (islayer? env)) (eq? variable (car (operator env)))) (unbox (car (leftoperand env)))) ; safety check. shouldn't ever go through but if it does o wel
      ((eq? variable (car (operator env))) (unbox (car (leftoperand env))))
      ((islayer? env) (valueofl variable (cdrcdr env)))
      (else #f))))
      
; is the variable present in env? has it been declared? (use env when not modifying state) 
(define in?
  (lambda (variable env)
    (not (eq? (valueof variable env) #f))))

; is the variable present in a layer in the environment?
(define inl?
  (lambda (variable env)
    (not (eq? (valueofl variable env) #f))))
      
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

(define Mval
  (lambda (exp state)
    (cond
      ((number? exp) exp)                     ; expression is number
      
      ((and                                   
        (in? exp state)                          ; in state
        (eq? (valueof exp state) 'undefined))    ; undefined
       (error 'make-sure-your-variables-are-defined-with-a-value))
      
        
      ((in? exp state) (valueof exp state))   ; expression is variable in state and defined
      
      ((and                       ; expression is not in state ^
        (not (list? exp))         ; not a complex expression
        (not (number? exp))       ; not a number
        (atom? exp)               ; is an atom
        (not (in? exp state)))     
       (error 'make-sure-your-variables-are-declared))
       
      ((eq? '+ (operator exp)) (+ (Mval (leftoperand exp) state) (Mval (rightoperand exp) state)))
      ((eq? '/ (operator exp)) (quotient (Mval (leftoperand exp) state) (Mval (rightoperand exp) state)))
      ((eq? '% (operator exp)) (remainder (Mval (leftoperand exp) state) (Mval (rightoperand exp) state)))
      ((eq? '- (operator exp)) (Mval_unary_neg exp state))
      ((eq? '* (operator exp)) (* (Mval (leftoperand exp) state) (Mval (rightoperand exp) state)))
      
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
      ((number? exp) (Mval exp st))
      ((in? exp st) (valueof exp st))

      ((eq? '&& (operator exp)) (and (Mbool1 (leftoperand exp) st) (Mbool1 (rightoperand exp) st)))
      ((eq? '|| (operator exp)) (or (Mbool1 (leftoperand exp) st) (Mbool1 (rightoperand exp) st)))
      ((eq? '!  (operator exp)) (not (Mbool1 (leftoperand exp) st)))
      
      ((eq? '== (operator exp)) (eq? (Mval (leftoperand exp) st) (Mval (rightoperand exp) st)))
      ((eq? '!= (operator exp)) (not (eq? (Mval (leftoperand exp) st) (Mval (rightoperand exp) st))))
      
      ((eq? '<  (operator exp)) (< (Mval (leftoperand exp) st) (Mval (rightoperand exp) st)))
      ((eq? '>  (operator exp)) (> (Mval (leftoperand exp) st) (Mval (rightoperand exp) st)))
      ((eq? '>= (operator exp)) (>= (Mval (leftoperand exp) st) (Mval (rightoperand exp) st)))
      ((eq? '<= (operator exp)) (<= (Mval (leftoperand exp) st) (Mval (rightoperand exp) st)))
      
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
(define funbody cadddr)
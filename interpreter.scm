(load "functionParser.scm")
;(load "simpleParser.scm")


; test
(define testvalid
  (lambda ()
    (list
     (interpret "1.txt") (interpret "2.txt") (interpret "3.txt") (interpret "4.txt") (interpret "5.txt") (interpret "6.txt") (interpret "7.txt") (interpret "8.txt") (interpret "9.txt") (interpret "10.txt")
     (interpret "11.txt") (interpret "13.txt") (interpret "14.txt") (interpret "15.txt") (interpret "16.txt"))))

(define testvalid2
  (lambda ()
    (list 
     (interpret "2tests/1.txt") (interpret "2tests/2.txt") (interpret "2tests/3.txt") (interpret "2tests/4.txt") (interpret "2tests/5.txt") (interpret "2tests/6.txt") (interpret "2tests/7.txt") (interpret "2tests/11.txt") (interpret "2tests/12.txt"))))    
(define ppt
  (lambda (name)
    (print (parser name))))

(define parsetree
  (lambda (name)
    (parser name)))

; 'outer' interpret
(define interpret
  (lambda (name)
    (innerinterpret (body (closure 'main (Mstatelistglobal (parser name) (newenv))))
                (Mstatelistglobal (parser name) (newenv)))))

; called each time function is called to interpret the function body, therefore need to add layer here
(define innerinterpret
  (lambda (exp st)
    (call/cc
     (lambda (return)
       (Mstatelist exp st return (lambda (b) b) (lambda (c) c))))))

(define callmain
  (lambda (exp st)
    (cond
      ((null? exp) (error 'no-main-function))
      ((eq? 'main  (leftoperand (car exp))) (formatoutput (Mst_funcall     
                                                     (car exp)
                                                     st)))
      (else (callmain (cdr exp) st)))))
    
(define formatoutput
  (lambda (condition)
    (cond
      ((eq? #t condition) 'true)
      ((eq? #f condition) 'false)
      (else condition))))

; main Mstate wrapper
(define Mstatelist 
  (lambda (exp st return break continue)
    (cond
      ((null? exp) st) 
      ((list? (Mst (car exp) st return break continue)) (Mstatelist (cdr exp) (Mst (car exp) st return break continue) return break continue))
      (else (Mstatelist (cdr exp) (Mst (car exp) st return break continue) return break continue)))))

;;; global Mstatelist
(define Mstatelistglobal
  (lambda (exp st)
    (cond
      ((null? exp) st)
      (else (Mstatelistglobal (cdr exp) (Mstg (car exp) st))))))
;;;
    
(define functionbody
  (lambda (syntax)
    (cadddr (car syntax))))
    
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
      ((null? exp)    st)
      ((eq? 'break    (operator exp)) (Mst_break       st break))
      ((eq? 'continue (operator exp)) (Mst_continue    st continue))
      ((eq? 'var      (operator exp)) (Mst_declare     exp st))
      ((eq? '=        (operator exp)) (Mst_assign      exp st))
      ((eq? 'return   (operator exp)) (Mst_return      exp st return))
      ((eq? 'if       (operator exp)) (Mst_if          exp st return break continue))
      ((eq? 'begin    (operator exp)) (Mst_begin       exp st return break continue))
      ((eq? 'while    (operator exp)) (Mst_while       exp st return break continue))
      ((eq? 'function (operator exp)) (Mst_funclosure  exp st))
      ((eq? 'funcall  (operator exp)) (Mst_funcall     exp st))
      (else (error    'out-of-place-command-identifier-in-code)))))


(define Mstg
  (lambda (exp st)
    (cond
      ((null? exp)    st)
      ((eq? 'var      (operator exp)) (Mst_declare     exp st))
      ((eq? 'function (operator exp)) (Mst_funclosure  exp st))
      ((eq? 'funcall  (operator exp)) (Mst_funcall     exp st))
      ; next lines dont ever get called 
      ((eq? 'main  (leftoperand exp)) (formatoutput (Mst_funcall     
                                       exp
                                       st)))
      (else (error    'only-global-variables-and-functions-allowed)))))


; '(var x expression) and '(var x)
; Declare variable (place in state with corresponding value), if doesn't have value then 'undefined
(define Mst_declare
  (lambda (exp st)
    (cond
      ; prevent redefining
      ((inl? (leftoperand exp) (car st)) (error 'redefining))
      ; if right operand is null add left operand to state with undefined
      ((null? (rightoperand exp)) (addst (leftoperand exp) 'undefined st))
      ; if right operand is not null, add left value to state with (Mval (rightoperand exp) ... (must be Mval because dont want to save state as true or false, but #t and #f
      (else (addst (leftoperand exp) (Mvalfunc (rightoperand exp) st) st)))))

; Assign a value to an existing variable (throw error if doesn't exist)
(define Mst_assign
  (lambda (exp st)
    (cond
      ((in? (leftoperand exp) st) (replacest (leftoperand exp) (Mvalfunc (rightoperand exp) st) st))
      (else (error 'declare-your-variables-before-assigning-it-a-value)))))

; '(return expression)
(define Mst_return
  (lambda (exp st return)
    (if (null? exp) 
        state
        (return (Mvalfunc (leftoperand exp) st)))))

; (Mst_if '(if (|| (! z) false) (= z (! z)) (= z z)) '((x y z) (10 20 true))
; cadr = condition, caddr = statement1, cadddr = statement2
(define Mst_if
  (lambda (exp st return break continue)
    (cond
      ((Mvalfunc (cadr exp) st) (Mst (caddr exp) st return break continue))                 ; if cond true
      ((and (null? (cdddr exp)) (not (Mvalfunc (cadr exp) st))) st)   ; 
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
  (lambda (syntax st)
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


; Mst_funcall
(define Mst_funcall
  (lambda (syntax st)
    (innerinterpret (body (closure (cadr syntax) st))          ; syntax should include funcall statement
                 (checkformaltoactualparameters 
                  (parameters (closure (cadr syntax) st))      ; formal parameters
                  (cddr syntax)                                ; actual parameters
                  st                                           ; current state
                  (trimfunc (closure (cadr syntax) st)))       ; trim function with current state 'remembered'
                 )))
                
; helpers for Mst_funcall
(define closure
  (lambda (functionname st)
    (cond
      ((not (eq? (valueof functionname st) #f)) (valueof functionname st))
      (else (error 'define-function-before-calling-it)))))

(define parameters
  (lambda (closure)
    (car closure)))
(define body
  (lambda (closure)
    (cadr closure)))
(define trimfunc
  (lambda (closure)
    (caddr closure)))
    
; checkformaltoactualparameters returns the new state that pertains directly to function execution
(define checkformaltoactualparameters
  (lambda (formalparameters actualparameters state trimfunc)
    (cond
      ((not (eq? (length formalparameters) (length actualparameters))) (error 'mismatched-parameters-and-arguments))
      ((not (allinst? actualparameters state)) (error 'declare-your-function-variables-before-passing-them))
      (else (cons (functionstate formalparameters (evaluate actualparameters state) (newlayer) state) (trimfunc state))))))
      
; checks if all the parameters are in the declared state, true if so, false if not
(define allinst?
  (lambda (parameters st)
    (cond
      ((null? parameters) #t)
      ((list? (car parameters)) (allinst? (cdr parameters) st))    ;; pass through for now... not sure if this is right. 
      ((number? (car parameters)) (allinst? (cdr parameters) st))
      ((eq? (car parameters) 'true) (allinst? (cdr parameters) st))
      ((eq? (car parameters) 'false) (allinst? (cdr parameters) st))
      ((in? (car parameters) st) (allinst? (cdr parameters) st))
      (else #f))))

; generates a layer for current function with properly bound parameters
; only called when parameters match and are all in state
; (functionstate '(a) '(x) (newlayer) (addst 'x 1 (newenv)))
; (functionstate '(a b) '(x y) (newlayer) '(((x y) (#&1 #&2))))
(define functionstate
  (lambda (formals actuals layer state)
    (cond
      ((null? formals) layer)
      (else (functionstate (cdr formals)
                           (cdr actuals)
                           (list 
                            (addtoend (car formals) (operator layer))
                            (addtoend (box (car actuals)) (leftoperand layer)))
                           state)))))
    
; returns a list of parameters that have been evaluated with mval
(define evaluate
  (lambda (actualparameters state)
    (cond
      ((null? actualparameters) '())
      (else (cons (Mvalfunc (car actualparameters) state) (evaluate (cdr actualparameters) state))))))


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
(define replacestl2
  (lambda (variable expv st)
    (cond 
      ((null? (operator st)) (newenv))
      ((eq? variable (car (operator st))) st)
      (else (list 
             (cons (car (operator st)) (car (replacestl variable expv (cdrcdr st))))
             (cons (car (leftoperand st)) (cadr (replacestl variable expv (cdrcdr st)))))))))

; replaces variable's old value with new value in a layer
(define replacestl
  (lambda (variable expv st)
    (cond 
      ((null? (operator st)) (newenv))
      ((eq? variable (car (operator st))) (begin (set-box! (car (leftoperand st)) expv) st))
      (else (list 
             (cons (car (operator st)) (car (replacestl variable expv (cdrcdr st))))
             (cons (car (leftoperand st)) (cadr (replacestl variable expv (cdrcdr st)))))))))


(define test
  (lambda ()
    (replacest 'z 5 (addst 'z 3 (addst 'y 2 (addst 'x 1 (newenv)))))))
      
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
; ------------------------------------------<
; M Value
;

(define Mvalfunc
  (lambda (exp state)
    (cond
      ;;; error checking
      ((number? exp) exp)                     ; expression is number
      ((isbool? exp) exp)
      
      ;;; only call operator or Mst_funcall if its a list..
      ((and (list? exp) (eq? 'funcall (operator exp))) (Mst_funcall exp state))
      (else (Mval exp state)))))
      

(define Mval
  (lambda (exp state)
    (cond
      ;;; error checking
      ((number? exp) exp)                     ; expression is number
      ((and                                   
        (in? exp state)                          ; in state
        (eq? (valueof exp state) 'undefined))    ; undefined
       (error 'make-sure-your-variables-are-defined-with-a-value))
      
        
      ((in? exp state) (valueof exp state))   ; expression is variable in state and defined
      
      ((and                       ; expression is not in state ^
        (not (isbool? exp))       ; not a bool value
        (not (list? exp))         ; not a complex expression
        (not (number? exp))       ; not a number
        (atom? exp)               ; is an atom
        (not (in? exp state)))     
       (error 'make-sure-your-variables-are-declared))
      ;;; ...
      
      ((isbool? exp) (Mbool1 exp state))
      
      ((eq? '+ (operator exp)) (+ (Mvalfunc (leftoperand exp) state) (Mvalfunc (rightoperand exp) state)))
      ((eq? '/ (operator exp)) (quotient (Mvalfunc (leftoperand exp) state) (Mvalfunc (rightoperand exp) state)))
      ((eq? '% (operator exp)) (remainder (Mvalfunc (leftoperand exp) state) (Mvalfunc (rightoperand exp) state)))
      ((eq? '- (operator exp)) (cond
                                 ((null? (rightoperand exp)) (- 0 (Mvalfunc (leftoperand exp) state)))       ; 7 - 7 = 0 -> - 7  = -7
                                 (else (- (Mvalfunc (leftoperand exp) state) (Mvalfunc (rightoperand exp) state)))))
      ((eq? '* (operator exp)) (* (Mval (leftoperand exp) state) (Mval (rightoperand exp) state)))
      
      (else (Mbool1 exp state)))))


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
      
      ;;; only call operator or Mst_funcall if its a list..
      ((and (list? exp) (eq? 'funcall (operator exp))) (Mst_funcall exp st))

      ((eq? '&& (operator exp)) (and (Mbool1 (leftoperand exp) st) (Mbool1 (rightoperand exp) st)))
      ((eq? '|| (operator exp)) (or (Mbool1 (leftoperand exp) st) (Mbool1 (rightoperand exp) st)))
      ((eq? '!  (operator exp)) (not (Mbool1 (leftoperand exp) st)))
      
      ((eq? '== (operator exp)) (eq? (Mvalfunc (leftoperand exp) st) (Mvalfunc (rightoperand exp) st)))
      ((eq? '!= (operator exp)) (not (eq? (Mvalfunc (leftoperand exp) st) (Mvalfunc (rightoperand exp) st))))
      
      ((eq? '<  (operator exp)) (< (Mvalfunc (leftoperand exp) st) (Mvalfunc (rightoperand exp) st)))
      ((eq? '>  (operator exp)) (> (Mvalfunc (leftoperand exp) st) (Mvalfunc (rightoperand exp) st)))
      ((eq? '>= (operator exp)) (>= (Mvalfunc (leftoperand exp) st) (Mvalfunc (rightoperand exp) st)))
      ((eq? '<= (operator exp)) (<= (Mvalfunc (leftoperand exp) st) (Mvalfunc (rightoperand exp) st)))
      
      (else (error 'bad-operator)))))

(define isbool?
  (lambda (exp)
    (or (eq? 'true exp) (eq? 'false exp) (eq? exp #t) (eq? exp #f))))


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
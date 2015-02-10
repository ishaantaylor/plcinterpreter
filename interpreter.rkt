#lang racket
; M_value takes an expression, a state and returns its evaluation
; (Mval '(+ 1 (* 5 x)) '((x) (10)))
; doesn't yet support undefined vairables
(define Mval
  (lambda (exp state)
    (cond
      ((number? exp) exp)                     ; expression is number
      ((and                                   
        (in? exp state)                       ; in state
        (eq? (valueof exp state) 'undefined))   ; undefined
       (error 'make-sure-your-variables-are-defined))
        
      ((in? exp state) (valueof exp state))   ; expression is variable in state and defined
      
      ((and                       ; expression is not in state ^
        (not (number? exp))       ; not a number
        (atom? exp))              ; is an atom
       (error 'make-sure-your-variables-are-declared))
       
      
      ((eq? '+ (operator exp)) (+ (Mval (leftoperand exp) state) (Mval (rightoperand exp) state)))
      ((eq? '/ (operator exp)) (quotient (Mval (leftoperand exp) state) (Mval (rightoperand exp) state)))
      ((eq? '% (operator exp)) (remainder (Mval (leftoperand exp) state) (Mval (rightoperand exp) state)))
      ((eq? '- (operator exp)) (Mval_unary_neg exp state))
      ((eq? '* (operator exp)) (* (Mval (leftoperand exp) state) (Mval (rightoperand exp) state)))
           
      (else (error 'bad-operator)))))


; supports the unary negation operator
(define Mval_unary_neg
  (lambda (exp state)
    (cond
      ((null? (rightoperand exp)) (- 0 (Mval (leftoperand exp) state)))       ; 7 - 7 = 0 -> - 7  = -7
      (else (- (Mval (leftoperand exp) state) (Mval (rightoperand exp) state))))))

; doesn't yet understand the difference between numbers and booleans for == !=, and doesn't know how to check for either
(define Mbool
  (lambda (exp st)
    (cond
      ((number? (Mval exp st)) (Mval exp st))
      ((eq? exp 'true) #t)
      ((eq? exp 'false) #f)
      ((eq? '&& (operator exp)) )
      ((eq? '|| (operator exp)) )
      ((eq? '< (operator exp)) (< (Mbool (leftoperand exp) st) (Mbool (rightoperand exp) st)))
      ((eq? '> (operator exp)) (> (Mbool (leftoperand exp) st) (Mbool (rightoperand exp) st)))
      ((eq? '>= (operator exp)) (>= (Mbool (leftoperand exp) st) (Mbool (rightoperand exp) st)))
      ((eq? '<= (operator exp)) (<= (Mbool (leftoperand exp) st) (Mbool (rightoperand exp) st)))
      ((eq? '== (operator exp)) (= (Mbool (leftoperand exp) st) (Mbool (rightoperand exp) st)))
      ((eq? '!= (operator exp)) (not (= (Mbool (leftoperand exp) st) (Mbool (rightoperand exp) st))))
      )))
            
; main Mstate function that directs the rest of the Mstates, called by interpret
(define Mst
  (lambda (exp st)
    (cond
      ((eq? 'var     (operator exp)) (Mst_declare  exp st))
      ((eq? '=       (operator exp)) (Mst_assign   exp st))
      ((eq? 'return  (operator exp)) (Mst_return   exp st))
      ((eq? 'if      (operator exp)) (Mst_if       exp st)))))

; '(var x expression) and '(var x)
; Declare variable (place in state with corresponding value), if doesn't have value then 'undefined
; (Mst_declare '(var x) '(()()))
; (Mst_declare '(var x 10) '(()()))
; (Mst_declare '(var x (* 2 10)) '(()()))
; (Mst_declare '(var x (* x 10)) '((x)(10)))
(define Mst_declare
  (lambda (exp st)
    (cond
      ((null? (rightoperand exp)) (addst (leftoperand exp) 'undefined st))
      (else (Mst_assign exp st)))))

; '(return expression)
; create new return variable and put it in state)
; (Mst_return '(return 10) '(()()))
; (Mst_return '(return (* 10 x)) '((x) (9)))
(define Mst_return
  (lambda (exp st)
    (addst (operator exp) (Mval (leftoperand exp) st) st)))

; (Mst_assign '(= x 10) '(() ()))
; (Mst_assign '(= x 10) '((x) (4)))
(define Mst_assign
  (lambda (exp st)
    (cond
      ((in? (leftoperand exp) st) (addst (leftoperand exp) (Mval (rightoperand exp) st) (removest (leftoperand exp) st)))
      (else (error 'declare-before-assigning-variable)))))

; (Mst_if '(if (>= x y) (= m x) (= m y)) '((x y m) (1 2 0)))
; cadr = condition, caddr = statement1, cadddr = statement2
(define Mst_if
  (lambda (exp st)
    (cond
      ((Mbool (cadr exp) st) (Mst (caddr exp) st))
      (else (Mst (cadddr exp) st)))))


; adds variable to car st and value to (same position) of cadr :: ((variables) (values))
; (addst 'x '(* 10 2) '(()()))
(define addst
  (lambda (variable exp st)
    (cond
      ((in? variable st) (replacest variable exp st))
      (else (list 
             (addtoend variable (first st))
             (addtoend (Mval exp st) (second st)))))))

; removes variable and corresponding value from st
; (removest 'r '((y x f j r u i l) (2 5 9 1 2 3 5 21)))
(define removest
  (lambda (variable st)
    (cond
      ((null? (first st)) '(() ()))
      ((eq? variable (car (first st))) (removest variable (cdrcdr st)))
      (else (list 
             (cons (car (first st)) (car (removest variable (cdrcdr st))))
             (cons (car (second st)) (cadr (removest variable (cdrcdr st)))))))))

; returns the value of a variable thats in the state
; (valueof 'r '((y x f j r u i l) (2 5 9 1 2 3 5 21)))
(define valueof
  (lambda (variable env)
    (cond
      ((null? (first env)) #f)      ; hopefully will never be called
      ((eq? variable (car (first env))) (car (second env)))
      (else (valueof variable (cdrcdr env))))))
      
; is the variable present in env? has it been declared? (use env when not modifying state) 
; (in? 'x '((y x z) (1 2 3)))
(define in?
  (lambda (variable env)
    (cond
      ((null? (car env)) #f)    ; (*()* ())
      ((eq? variable (car (first env))) #t)
      (else (in? variable (cdrcdr env))))))  ; if first element of (car st) is not the variable, new state is just cons of (trim first element from car and cdr of env)

; replace variable's value with exp
; add variable and (expression evaluated with current state) into (st that has just removed current 'variable's state)
; (replacest 'x '(* 4 10) '((y x z r) (2 5 10 20)))
(define replacest
  (lambda (variable exp st)
    (addst variable (Mval exp st) (removest variable st))))


; trim the first element off (first st) and (second st)
; (cdrcdr '((1 2 3) (4 5 6)))
(define cdrcdr
  (lambda (env)
    (cond
      ((null? (first env)) '(() ()))
      (else (list 
             (cdr (first env)) 
             (cdr (second env)))))))
      
; add a to end of l
; (addtoend 'x '(1 2 3 a b c s))
(define addtoend
  (lambda (a l)
    (cond
      ((null? l) (cons a '()))
      (else (cons (car l) (addtoend a (cdr l)))))))

; asks if `x` is an atom
(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; helper functions to get the first list in st and second
(define first car)
(define second cadr)

; the helper functions to determine where the operator and operands are depending on the form
(define operator car)
(define leftoperand cadr)
(define rightoperand 
  (lambda (exp)
    (cond
      ((null? (cddr exp)) '())      ; unary
      (else (car (cddr exp)))))) 
#lang racket
; M_value takes an expression, a state and returns its evaluation
; (Mval '(+ 1 (* 5 x)) '((x) (10)))
(define Mval
  (lambda (exp state)
    (cond
      ((number? exp) exp)
      ((in? exp state) (valueof exp state))
      ((eq? 'undefined exp) 'undefined)
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
            

; '(var x expression) and '(var x)
; (Mst_declare '(var x) '(()()))
; (Mst_declare '(var x 10) '(()()))
; (Mst_declare '(var x (* 2 10)) '(()()))
(define Mst_declare
  (lambda (exp st)
    (cond
      ((null? (rightoperand exp)) (addst (leftoperand exp) 'undefined st))
      (else (addst (leftoperand exp) (Mval (rightoperand exp) st) st)))))


; '(return expression)
(define Mst_return
  (lambda (exp st)
    ; create new return variable and put it in state)
    (addst (operator exp) (Mval (leftoperand exp) st) st)))


; (Mst_assign '(= x 10) '(() ()))
(define Mst_assign
  (lambda (exp st)
    (addst (leftoperand exp) (Mval (rightoperand exp) st) (removest (leftoperand exp) st))))


; adds variable to car st and value to (same position) of cadr :: ((variables) (values))
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
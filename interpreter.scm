; Ishaan Taylor
(load "classParser.scm")
;(load "functionParser.scm")
;(load "simpleParser.scm")


;; Tests
(define testvalid
  (lambda ()
    (list
     (interpret "1.txt" "A") (interpret "2.txt" "Square") (interpret "3.txt" "B") (interpret "4.txt" "B") (interpret "5.txt" "C"))))
(define parse
  (lambda (name)
    (pretty-print (parser name))))


;;;;;;;;;;;;;;;;;;;;;;; BEGIN ;;;;;;;;;;;;;;;;;;;;;;;;;

(define interpret
  (lambda (name class)
    (formatoutput (callmains (parser name) (Mstatelistclass (parser name) (newenv)) (string->symbol class)))))

; 'outer' interpret
; TODO: get-main only works now for class body, not entire parse tree.
(define maininterpret
  (lambda (name)
    (Mv_funcall_main (get-main (parser name)) (Mstatelistglobal (parser name) (newenv)))))

; called each time function is called to interpret the function body
(define functioninterpret
  (lambda (exp st vore)
    (call/cc
     (lambda (return)
       (removelayer (Mstatelist exp (addlayer st) return (lambda (b) b) (lambda (c) c) (lambda (t) t) vore))))))

; returns list of main function executions
(define callmains
  (lambda (exp st mainclass)
    (Mv_funcall_main (getmain mainclass st) st)))                 
      

; formats output from #t to true ...
(define formatoutput
  (lambda (condition)
    (cond
      ((eq? #t condition) 'true)
      ((eq? #f condition) 'false)
      (else condition))))

; main Mstate wrapper
(define Mstatelist 
  (lambda (exp st return break continue throw vore)
    (cond
      ((null? exp) st)
      (else (Mstatelist (cdr exp) (Mst (car exp) st return break continue throw vore) return break continue throw vore)))))

;;; global Mstatelist
(define Mstatelistglobal
  (lambda (exp st)
    (cond
      ((null? exp) st)
      (else (Mstatelistglobal (cdr exp) (Mstg (car exp) st))))))
;;;

; TODO: change Mstg to Mstc or something , did this in a rush
(define Mstatelistclass
  (lambda (exp st)
    (cond
      ((null? exp) st)
      (else (Mstatelistclass (cdr exp) (Mstg (car exp) st))))))
    
(define functionbody (lambda (syntax) (cadddr (car syntax))))

; TODO: change cadr to leftoperator
;reconstruct function from closure
(define getmain
  (lambda (classname st)
    (valueofc-inobj 'main (getclass classname st))))
    
(define functionname (lambda (exp) (leftoperand exp)))
    
(define defaultbreak
  (lambda (b) b))

(define defaultcontinue
  (lambda (c) c))

(define defaultthrow
  (lambda (t) t))


; ------------------------------------------<
; M State functions
; 

; main Mstate ƒunction that directs the rest of the Mstates, called by interpret
(define Mst
  (lambda (exp st return break continue throw vore)
    (cond
      ((null? exp)        st)
      ((eq? 'break        (operator exp)) (Mst_break       st break))
      ((eq? 'continue     (operator exp)) (Mst_continue    st continue))
      ((eq? 'var          (operator exp)) (Mst_declare     exp st))
      ((eq? '=            (operator exp)) (Mst_assign      exp st))
      ((eq? 'return       (operator exp)) (Mst_return      exp st return vore))
      ((eq? 'if           (operator exp)) (Mst_if          exp st return break continue throw vore))
      ((eq? 'begin        (operator exp)) (Mst_begin       exp st return break continue throw vore))
      ((eq? 'while        (operator exp)) (Mst_while       exp st return break continue throw vore))
      ((eq? 'function     (operator exp)) (Mst_funclosure  exp st))
      ((eq? 'funcall      (operator exp)) (Mst_funcall     exp st))
      ((eq? 'class        (operator exp)) (Mst_class       exp st))
      ((eq? 'try          (operator exp)) (Mst_try         (cdr exp) st return break continue throw vore))
      ((eq? 'throw        (operator exp)) (Mst_throw       exp st throw))
      (else (error        'out-of-place-command-identifier-in-code)))))


; Mst parsing for functions
(define Mstg
  (lambda (exp st)
    (cond
      ((null? exp)    st)
      ((eq? 'class    (operator exp)) (Mst_class       exp st))
      
      ; rest shouldnt happen
      ((eq? 'var      (operator exp)) (Mst_declare     exp st))
      ((eq? 'function (operator exp)) (Mst_funclosure  exp st))
      (else (error    'only-global-variables-and-functions-allowed)))))


;=====================================
;; CLASSES
; Mst parsing for class
(define Mstc
  (lambda (exp st)
    (cond
      ((null? exp)    st)
      ((eq? 'static-var       (operator exp)) (Mst_declare     exp st))
      ((eq? 'static-function  (operator exp)) (Mst_funclosure  exp st))
      (else (error            'only-global-variables-and-functions-allowed)))))

; add class key and class value to st
(define Mst_class
  (lambda (exp st)
    (addst (classname exp) (Mst_class_inner exp) st)))

; helper
(define classname (lambda (exp) (cadr exp)))
             
; Returns a class 'object' -> returns a layer of state with class in it
; ((A) (<classobj>)
(define Mst_class_inner
  (lambda (exp)
    (cond
      ((null? (funbody exp)) (error 'empty-class-body))
      (else 
       (list
        (createclass exp))))))

; gets class 'object's value
(define createclass
  (lambda (exp)
    (list
     (car (createparent            (extend exp)))
     (car (createclassfieldenv     (classbody exp)))
     (car (createmethodenv         (classbody exp)))
     (car (createinstancefieldenv  (classbody exp))))))

; helpers for createclass
(define extend     (lambda (exp) (caddr exp)))
(define classbody  (lambda (exp) (cadddr exp)))

; returns value of the parent class, '() if null
; takes '(() body)
(define createparent
  (lambda (extends-stmt)
    (cond
      ((null? extends-stmt) (list '()))
      ((eq? 'extends     (operator extends-stmt)) (list (leftoperand extends-stmt)))
      (else '()))))

; returns class field layer
(define createclassfieldenv
  (lambda (stmt-list)
    (Mstatelistmod 'static-var       stmt-list (newenv))))

; returns static method layer
(define createmethodenv
  (lambda (stmt-list)
    (Mstatelistmod 'static-function  stmt-list (newenv))))
 
; returns instance field layer
(define createinstancefieldenv
  (lambda (stmt-list)
    (newenv)))
    ;(Mstatelistmod 'var              stmt-list (newenv))))

  
; selectively calls Mst for the specified `comp` parameter
; ie. if comp : static-var, will call Mstc on that exp
(define Mstatelistmod
  (lambda (comp stmt-list state)
    (cond
      ((null? stmt-list) state)
      ((eq? comp (operator (car stmt-list))) (Mstatelistmod comp (cdr stmt-list) (Mstc (car stmt-list) state)))
      (else (Mstatelistmod comp (cdr stmt-list) state)))))
;=================================
;
  
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
  (lambda (exp st return vore)
    (cond
      ((null? exp) st)
      ((eq? vore 'value) (return (Mvalfunc (leftoperand exp) st)))
      ((eq? vore 'return) (return (removelayer (Mst exp st return (lambda (b) b) (lambda (c) c)))))
      (else st))))

; (Mst_if '(if (|| (! z) false) (= z (! z)) (= z z)) '((x y z) (10 20 true))
; cadr = condition, caddr = statement1, cadddr = statement2
(define Mst_if
  (lambda (exp st return break continue throw vore)
    (cond
      ((Mvalfunc (cadr exp) st) (Mst (caddr exp) st return break continue vore))                 ; if cond true
      ((and (null? (cdddr exp)) (not (Mvalfunc (cadr exp) st))) st)   ; 
      (else (Mst (cadddr exp) st return break continue vore)))))

; (Mst_while
(define Mst_while
  (lambda (exp st return break continue throw vore)
    (while (leftoperand exp) (rightoperand exp) st return vore)))

(define while
  (lambda (c b st return throw vore)
    (call/cc (lambda (break)
               (letrec ((loop (lambda (cond body state)
                                (if (Mbool1 cond state)
                                    (loop cond body (call/cc (lambda (continue) 
                                                     (Mst body state return break continue throw vore))))
                                state))))
               (loop c b st))))))

; (Mst_begin
(define Mst_begin
  (lambda (exp st return break continue throw vore)
    (cond
      ((null? exp) st)
      (else (removelayer (Mstatelist (cdr exp) (addlayer st) return (lambda (v) (break (removelayer v))) (lambda (v) (continue (removelayer v))) vore))))))
    
    
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
     (mktrimmer (length st))
     ; TODO: (mkruntype)))
     )))

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
    (functioninterpret (body (closure (cadr syntax) st))          ; syntax should include funcall statement
                    (checkformaltoactualparameters 
                     (parameters (closure (cadr syntax) st))      ; formal parameters
                     (cddr syntax)                                ; actual parameters
                     st                                           ; current state with new layer
                     (trimfunc (closure (cadr syntax) st)))       ; trim function with current state 'remembered'
                    'expression
                    )))

; Mv_funcall, only main should call
(define Mv_funcall
  (lambda (syntax st)
    (functioninterpret (body (closure (cadr syntax) st))          ; syntax should include funcall statement
                    (checkformaltoactualparameters 
                     (parameters (closure (cadr syntax) st))      ; formal parameters
                     (if (hasbody syntax)                         ; TODO: fix this, always evaluates to true, see function has-sublists, no #t case
                         (caddr syntax)      
                         (cddr  syntax))                           ; actual parameters
                     st                                           ; current state with new layer
                     (trimfunc (closure (cadr syntax) st)))       ; trim function with current state 'remembered'
                    'value
                    )))

; Mv_funcall_main
(define Mv_funcall_main
  (lambda (syntax st)
    (functioninterpret (funbody syntax) st 'value)))
                    
       
(define hasbody
  (lambda (syntax)
    (has-sublists syntax)))
(define has-sublists
  (lambda (syntax)
    (cond
      ((null? syntax) #f)
      (else (has-sublists (cdr syntax))))))


; helpers for Mst_funcall
(define closure
  (lambda (functionname st)
    (cond
      ((and (list? functionname) (eq? 'dot (operator functionname))) (getclosure (leftoperand functionname) (rightoperand functionname) st))  ; TODO: write function that gets closure for (dot A funcname) 
      (else (valueof functionname st)))))

(define getclosure
  (lambda (classname functionname st)
    (cond
      ((inl? classname (baselayer st)) )
      (else 1))))
      

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
      ((list? (car parameters)) (Mval (car parameters) st))    ;; pass through for now... not sure if this is right. 
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
                            (cons (car formals) (operator layer))
                            (cons (box (car actuals)) (leftoperand layer)))
                           state)))))
    
; returns a list of parameters that have been evaluated with mval
(define evaluate
  (lambda (actualparameters state)
    (cond
      ((null? actualparameters) '())
      (else (cons (Mvalfunc (car actualparameters) state) (evaluate (cdr actualparameters) state))))))

; TODO: define interpret dot value
  

; try catch finally
(define Mst_try
  (lambda (exp st return break continue throw vore)
    ((lambda (try_state)
       (finally (cdr exp) try_state return break continue throw vore))
     (try exp st return break continue throw vore))))
                      
; Mst_throw
(define Mst_throw
  (lambda (exp st throw)
    (throw (Mval (cadr exp) st))))

; try
(define try
  (lambda (exp st return break continue throw vore)
    (call/cc
     (lambda (throw-without-catch)
       (removelayer (Mstatelist (car exp) (addlayer st) return break continue 
                                (lambda (v)
                                  (throw-without-catch (catch v (cdr exp) st return break continue throw vore)))
                                vore))))))

; catch
(define catch
  (lambda (e body st return break continue throw vore)
    ((lambda (new-state)
       (cond
         ((null? body) st)
         ((eq? (operator body) 'catch) (removelayer (Mstatelist (catchbody body) new-state return break continue throw vore)))))
     (addst 'exception e (addlayer st)))))

(define catchbody (lambda (block) (caddr (car block))))

; finally
(define finally
  (lambda (body st return break continue throw vore)
    (cond
      ((null? body) st)
      ((eq? (operator body) 'catch) (finally (cdr body) st return break continue throw vore))
      (else (removelayer (Mstatelist (finallybody body) (addlayer st) return break continue throw vore))))))

(define finallybody (lambda (block) (cadr (car block))))

                                      
                                      
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
    (cond
      ((null? st) st)
      (else (cdr st)))))
                   
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
             (addtoend variable (names (car st)))
             (cons (box expv) (vals (car st))))
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
      ((null? (names st)) (newlayer))
      ((eq? variable (car (names st))) (removestl variable (cdrcdr st)))
      (else (list 
             (cons (car (names st)) (car (removestl variable (cdrcdr st))))
             (cons (car (vals st)) (cadr (removestl variable (cdrcdr st)))))))))


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
  (lambda (variable expv layer)
    (cond 
      ((null? (names layer)) (newenv))
      ((eq? variable (car (names layer))) (if (classname? variable)
                                                 (replacestc x expv (indexof (length (cdr (names layer)))))
                                                 (begin (set-box! (indexof (length (cdr (names layer))) 
                                                                          (vals layer))
                                                                 expv)
                                                       layer)))
      (else (list 
             (cons (car (names layer))    (names (replacestl variable expv (trimnames layer))))
             (vals (replacestl variable expv (trimnames layer))))))))

; replace a variable thats in a class defintion, not local)
(define replacestc
  (lambda (x expv c-obj)
    (cond
      ((inl? (fields c-obj))  (replacestlc x expv (fields c-obj)))
      ((inl? (methods c-obj)) (replacestlc x expv (methods c-obj)))
      (else (error 'dont-call-this)))))

; replaces variable's old value with new value in a layer
(define replacestlc
  (lambda (variable expv layer)
    (cond 
      ((null? (names layer)) (newlayer))
      ((eq? variable (car (names layer))) (begin (set-box! (indexof (length (cdr (names layer)))
                                                                          (vals layer))
                                                                 expv)
                                                       layer))
      (else (list 
             (cons (car (names layer)) (names (replacestlc variable expv (trimnames layer))))
             (vals (replacestlc variable expv (trimnames layer))))))))
  

(define test
  (lambda ()
    (replacest 'y '1000 (addst 'o 'honey (replacest 'y 10 (replacest 'z 5 (addst 'z 3 (addst 'y 2 (addst 'x 1 (newenv))))))))))

; returns the value of a variable thats in the state
(define valueof
  (lambda (var env)
    (cond 
      ((isempty? env) '())      ; hopefully will never be called
      ((null? (cdr env)) (valueofbase var (car env)))  ; last layer        
      ((inl? var (car env)) (valueofl var (car env)))
      ((islayered? env) (valueof var (cdr env)))    ; layered and not in first layer, then check other layers
      (else (valueofl var (car env))))))            ; last layer

; returns the value of a variable thats in the first layer taking into account classes
(define valueofl
  (lambda (variable layer)
    (cond
      ((null? (names layer)) '())
      ((and (not (islayer? layer)) (eq? variable (car (names layer)))) (unbox (indexof (length (cdr (names (layer))) (vals layer))))) ; safety check. shouldn't ever go through but if it does o wel
      ((eq? variable (car (names layer))) (if (classname? variable)
                                              (valueofc-inobj variable (unbox (indexof (length (cdr (names layer))) (vals layer))))
                                              (unbox (indexof (length (cdr (names layer))) (vals layer)))))
      ((islayer? layer) (valueofl variable (trimnames layer)))
      (else '()))))

; returns value of a variable or function thats in a class definition (searches entire class space) given a class object
(define valueofc-inobj
  (lambda (x c-obj)
    (cond
      ((inl? x (fields c-obj))  (valueoflc x (fields c-obj)))
      ((inl? x (methods c-obj)) (valueoflc x (methods c-obj)))
      (else '()))))

; returns the value of a variable thats in the first layer without delving into classes
(define valueoflc
  (lambda (variable layer)
    (cond
      ((null? (names layer)) '())
      ;((and (not (islayer? layer)) (eq? variable (car (names layer)))) (unbox (indexof (length (cdr (names (layer))) (vals layer))))) ; safety check. shouldn't ever go through but if it does o wel
      ((eq? variable (car (names layer))) (unbox (indexof (length (cdr (names layer))) (vals layer))))
      ((islayer? layer) (valueoflc variable (trimnames layer)))
      (else '()))))

; valueof lookup in c-objs
(define valueofbase
  (lambda (variable layer)
    (cond
      ((null? (leftoperand layer)) '())
      ((inl? variable (fields (car (unbox (car (vals layer))))))  (valueofl variable (fields  (car (unbox (car (vals layer)))))))
      ((inl? variable (methods (car (unbox (car (vals layer)))))) (valueofl variable (methods (car (unbox (car (vals layer)))))))
      (else (valueofbase variable (trimvalues layer))))))

; get a c-obj from the base layer of the state
(define getclass
  (lambda (classname st)
    (valueofl classname (baselayer st))))

; is x in the environment? 
(define in?
  (lambda (x env)
    (cond
      ((list? x) #f)
      ((null? env) #f)
      ;((null? (cdr env)) (inclasses? x (car env)))
      ((null? (names (car env))) (in? x (cdr env)))
      (else (or (inl? x (car env)) (in? x (cdr env)))))))

; is x in the layer?
(define inl?
  (lambda (x layer)
    (cond
      ((null? (names layer)) #f)
      ((eq? (car (names layer)) x) #t)
      ((classname? x) (inc? x (valueofl x layer)))
      (else (inl? x (list (cdr (names layer)) (vals layer)))))))
  
; is x in the static definitions in a class obj value?
(define inc?
  (lambda (x c-obj)
    (if (not (null? c-obj))
        (or (inl? x (fields (car c-obj))) (inl? x (methods c-obj)))
        #f)))

(define fields (lambda (c-obj) (leftoperand c-obj)))
(define methods (lambda (c-obj) (rightoperand c-obj)))

; checks if a name is defined in the c-objs
(define inclasses?
  (lambda (x layer)
    (cond
      ((null? (vals layer)) #f)
      ((inl? x (fields  (car (unbox (car (vals layer)))))) #t)
      ((inl? x (methods (car (unbox (car (vals layer)))))) #t)
      (else (inclasses? x (trimvalues layer))))))

; need to make a wrapper function that first checks if there are any classes, or use some other method to search the things NOT in classes first v
(define inclasses2?
  (lambda (x layer)
    (cond
      ((null? (vals layer)) #f)
      ((inl? x (unbox (car (vals layer)))) #t)
      ((inl? x (unbox (car (vals layer)))) #t)
      (else (inclasses? x (trimvalues layer))))))

(define names (lambda (layer) (operator layer)))
(define vals (lambda (layer) (leftoperand layer)))

; retrieve base layer of environment
(define baselayer
  (lambda (env)
    (cond
      ((null? (cdr env)) (car env))
      (else (baselayer (cdr env))))))

; is the variable name a class name (capital letter)
(define classname? (lambda (x) (beginswithcapital? x)))
(define beginswithcapital?
  (lambda (x)
    (member? (first (string->list (symbol->string x)))
             '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))))
(define member?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq? x (car l)) #t)
      (else (member? x (cdr l))))))

; get the value of the nth index in l
(define indexof
  (lambda (n l)
    (cond
      ((null? l) -1)
      ((eq? n 0) (car l))
      (else (indexof (- n 1) (cdr l))))))


; trim the car off (names st) leave (vals st) intact
(define trimnames
  (lambda (layer)
    (cond
      ((null? layer) (newlayer))
      ((null? (names layer)) (newlayer))
      (else (list (cdr (names layer)) (leftoperand layer))))))

; trim the car off (vals st) leave (names st) intact
(define trimvalues
  (lambda (layer)
    (cond 
      ((null? layer) (newlayer))
      ((null? (leftoperand layer)) (newlayer))
      (else (list (operator layer) (cdr (leftoperand layer)))))))

; trim the first element off (names st) and (vals st)
; (cdrcdr '(()()))
; (cdrcdr '((1 2 3) (4 5 6)))
(define cdrcdr
  (lambda (env)
    (cond
      ((null? env) (newenv))
      ((isempty? env) env)
      (else (list 
             (cdr (names env)) 
             (cdr (vals env)))))))
      
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
; Class Environment manip
;
    


; ------------------------------------------<
; ------------------------------------------<
; M Value
;

(define Mvalfunc
  (lambda (exp state)
    (cond
      ;;; error checking
      ((number? exp) exp)                     ; expression is number
      ((isbool? exp) 
       (cond
         ((eq? exp 'true) #t)
         ((eq? exp 'false) #f)
         (else exp)))
      
      ;;; only call operator or Mst_funcall if its a list..
      ((and (list? exp) (eq? 'funcall (operator exp))) (Mv_funcall exp state))
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
      
      ((dot? exp) (evaldot exp state))
        
      ((in? exp state) (valueof exp state))   ; expression is variable in state and defined
      ((and (not (list? exp)) (inclasses? exp (baselayer state)))
           (valueofbase exp (baselayer state)))
      
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


(define dot? 
  (lambda (exp) 
    (if (list? exp)
        (eq? 'dot (operator exp))
        #f)))
(define evaldot
  (lambda (exp st)
    (Mvalfunc (valueofc-inobj (rightoperand exp) (car (valueoflc (classname exp) (baselayer st)))) st)))
(define classname (lambda (exp) (leftoperand exp)))

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
      
      ((and (list? exp) (in? (car exp) st)) (valueof (car exp) st))
      
      ;;; only call operator or Mst_funcall if its a list..
      ((and (list? exp) (eq? 'funcall (operator exp))) (Mv_funcall exp st))
      
      ((not (list? exp)) ('make-sure-boolean-variables-are-declared-properly))

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
; returns new class 'object'. in key->value model, this is a value
(define newclassval (lambda () '( () () () () ))) 

(define operator car)
(define leftoperand cadr)
(define rightoperand 
  (lambda (exp)
    (cond
      ((null? (cddr exp)) '())      ; unary
      (else (car (cddr exp))))))
(define funbody cadddr)
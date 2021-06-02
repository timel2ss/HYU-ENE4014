#lang racket
(provide (all-defined-out)) ;; exports the defined variables in this file.

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

(define (make-array length)
    (if (= length 0)
        null
        (mcons (int 0) (make-array (- length 1)))))
(define (set-array-val array index val)
  (if (= index 0)
      (set-mcar! array val)
      (mcar array)))
;; Problem 1

;; CHANGE (put your solutions here)
(define (racketlist->mupllist rlist)
  (if (null? rlist)
      (aunit)
      (apair (car rlist) (racketlist->mupllist (cdr rlist)))))

(define (mupllist->racketlist mlist)
  (if (aunit? mlist)
      null
      (cons (apair-e1 mlist) (mupllist->racketlist (apair-e2 mlist)))))

;; Test Codes
(equal? (racketlist->mupllist '()) (aunit))
(equal? (racketlist->mupllist '(1 2 3)) (apair 1 (apair 2 (apair 3 (aunit)))))
(equal? (mupllist->racketlist (aunit)) '())
(equal? (mupllist->racketlist (apair -5 (apair -4 (apair -3 (aunit))))) '(-5 -4 -3))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)(int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (letrec ([v (eval-under-env (mlet-e e) env)]
                  [letenv (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) letenv))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (let* ([cfun (closure-fun v1)]
                      [cenv (closure-env v1)]
                      [newenv (cons (cons (fun-formal cfun) v2) cenv)]
                      [fmap (cons (fun-nameopt cfun) v1)]
                      [fbody (fun-body cfun)])
                 (if (fun-nameopt cfun)
                     (eval-under-env fbody (cons fmap newenv))
                     (eval-under-env fbody newenv)))
               (error "MUPL call applied to non-closure")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd applied to non-apair")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Test Codes
(equal? (eval-exp (ifgreater (int 3) (add (int 1) (int 1)) (aunit) (int 42))) (aunit))
(equal? (eval-exp (ifgreater (add (int 1) (int 1)) (int 3) (aunit) (int 42))) (int 42))
(equal? (eval-exp (call (fun "fun" "x" (add (int 1) (var "x"))) (int 3))) (int 4))
(equal? (eval-exp (mlet "x" (int 42) (call (fun "fun" "arg" (add (var "x") (var "arg"))) (int 1)))) (int 43))
(equal? (eval-exp (apair (add (int 1) (int 2)) (aunit))) (apair (int 3) (aunit)))
(equal? (eval-exp (fst (apair (int 42) (int 43)))) (int 42))
(equal? (eval-exp (snd (apair (int 42) (int 43)))) (int 43))
(equal? (eval-exp (isaunit (aunit))) (int 1))
(equal? (eval-exp (isaunit (apair (int 2) (aunit)))) (int 0))
; (eval-exp (fst (int 7))) -> error case
; (eval-exp (snd (int 7))) -> error case
; (eval-exp (ifgreater (aunit) (int 42) (int 1) (int 2))) -> error case
; (eval-exp (call (apair (int 1) (aunit)) (int 1))) -> error case

;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4
                    (ifgreater (var "_y") (var "_x") e4 e3))))

;; Test Codes
(equal? (eval-exp (ifaunit (aunit) (int 42) (int 34))) (int 42))
(equal? (eval-exp (ifaunit (int 1) (int 42) (int 34))) (int 34))
(equal? (eval-exp (mlet* (list (cons "a" (int 1)) (cons "b" (int 2))) (add (var "a") (var "b")))) (int 3))
(equal? (eval-exp (ifeq (int 1) (int 1) (int 3) (int 4))) (int 3))
(equal? (eval-exp (ifeq (int 1) (int 2) 3 (int 4))) (int 4))

;; Problem 4

(define mupl-map
  (fun #f "func"
       (fun "map" "list"
           (ifaunit (var "list")
                    (aunit)
                    (apair (call (var "func") (fst (var "list")))
                           (call (var "map") (snd (var "list"))))))))

(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "#f" "I"
             (call (var "map") (fun #f "x" (add (var "x") (var "I")))))))

;; Test Codes
(equal? (eval-exp (call (call mupl-map (fun "fun" "x" (add (var "x") (int 1)))) (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))))) (apair (int 2) (apair (int 3) (apair (int 4) (aunit)))))
(equal? (eval-exp (call (call mupl-mapAddN (int 3)) (apair (int 1) (apair (int 2) (apair (int 3) (aunit)))))) (apair (int 4) (apair (int 5) (apair (int 6) (aunit)))))
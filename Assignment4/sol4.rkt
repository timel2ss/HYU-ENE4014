#lang racket
(provide (all-defined-out))
; 2019054957

; 1. check_bst
(define (check_bst bst)
  (if (null? bst)
      #t
      (letrec ([check_left (lambda(x)
                             (if (null? x)
                                #t
                                (> (car bst) (car x))))]
               [check_right (lambda(x)
                             (if (null? x)
                                 #t
                                 (< (car bst) (car x))))])
   (and (check_left (cadr bst)) (check_right (caddr bst)) (check_bst (cadr bst)) (check_bst (caddr bst))))))

; 2. apply
(define (apply f bst)
  (if (null? bst)
      null
      (list (f (car bst)) (apply f (cadr bst)) (apply f (caddr bst)))))

; 3. equals
(define (equals bst1 bst2)
  (letrec ([exist? (lambda(x ys)
                     (if (null? ys)
                         #f
                         (or (= x (car ys)) (exist? x (cadr ys)) (exist? x (caddr ys)))))]
           [exist_all_elements? (lambda (xs ys)
                                (if (null? xs)
                                    #t
                                    (and (exist? (car xs) ys) (exist_all_elements? (cadr xs) ys) (exist_all_elements? (caddr xs) ys))))])
    (and (exist_all_elements? bst1 bst2) (exist_all_elements? bst2 bst1))))
           
                                                               
; Test Codes
(equal? (check_bst '(6 (4 ()()) (7 ()()))) #t)
(equal? (check_bst '(8 (3 (1 ()()) (6 (4 ()())(7 ()()))) (10 ()(14 (13 ()())())))) #t)
(equal? (check_bst '(6 (7 ()())(8 ()()))) #f)

(equal? (apply (lambda (v) (+ v 1)) '(7 (6 ()()) (8 ()()))) '(8 (7 ()()) (9 ()())))

(equal? (equals '(7 (6 ()()) (8 ()())) '(6 () (7 () (8 ()())))) #t)
(equal? (equals '(7 (6 ()()) (8 ()())) '(7 (6 ()()) (8 ()(9 () ())))) #f)
#lang racket

(define (compare-single-values x y)
    (cond [(and (boolean? x) (boolean? y)) (compare-boolean x y)]
          [(equal? x y) x]
          [else `(if % ,x ,y)]))

(define (compare-boolean x y)
    (cond [(and x (not y)) '%]
          [(and (not x) y) '(not %)]
          [else (and x y)]))

(define (compare-lists l1 l2)
    (letrec
        ([helper
            (lambda (acc l1 l2)
                (cond 
                    [(or (null? l1) (null? l2)) acc] ;; double-check this base case
                    [(or (and (equal? 'quote (car l1)) (equal? 'quote (car l2)))
                         (and (or (equal? 'if (car l1)) (equal? 'if (car l2)))
                              (not (and (equal? 'if (car l1)) (equal? 'if (car l2)))))) 
                            (if (null? acc) (compare-single-values l1 l2) (append acc (list (compare-single-values l1 l2))))]
                    [(and (list? (car l1)) (list? (car l2))) 
                        (cond [(and (or (equal? 'lambda (car (car l1))) (equal? 'λ (car (car l1))))
                                    (or (equal? 'lambda (car (car l2))) (equal? 'λ (car (car l2)))))
                                    (helper (append acc (list (compare-lambdas (car l1) (car l2)))) (cdr l1) (cdr l2))]
                              [else (helper (append acc (list (compare-lists (car l1) (car l2)))) (cdr l1) (cdr l2))])]
                    [else (helper (append acc (list (compare-single-values (car l1) (car l2)))) (cdr l1) (cdr l2))]))])
    (helper '() l1 l2)))

(define (expr-compare x y)
    (cond [(and (boolean? x) (boolean? y)) (compare-boolean x y)]
          [(or (and (number? x) (number? y)) 
               (and (symbol? x) (list? y))
               (and (list? x) (symbol? y))
               (not (equal? (length x) (length y)))) (compare-single-values x y)]
          [else (compare-lists x y)]))

(define (make-dict l1 l2)
    (letrec
        ([helper
            (lambda (dict l1 l2)
                (cond [(or (null? l1) (null? l2)) dict]
                      [(and (list? (car l1)) (list? (car l2))) (make-dict (car l1) (car l2))]
                      [else (helper (dict-set dict (car l1) (car l2)) (cdr l1) (cdr l2))]))])
        (helper '#hash() l1 l2)))

(define (compare-symbols dict x y)
    (cond [(equal? x y) x] ;; if they're the same symbol, just return 
          [(or (and (equal? 'lambda x) (equal? 'λ y))
               (and (equal? 'λ x) (equal? 'lambda y))) 'λ]
          [(dict-has-key? dict x) 
            (cond [(equal? (dict-ref dict x) x) (compare-single-values x y)]
                  [else (string->symbol (string-append (symbol->string x) "!" (symbol->string (dict-ref dict x))))])]
          [else (compare-single-values x y)]         
)) 

(define (compare-lambdas-lists dict l1 l2)
    (letrec
        ([helper
            (lambda (acc l1 l2)
                (cond 
                    [(or (null? l1) (null? l2)) acc] ;; double-check this base case
                    [(and (list? (car l1)) (list? (car l2))) 
                        (cond [(not (equal? (car (car l1)) (car (car l2)))) 
                            (helper (append acc (compare-lists (car l1) (car l2))) (cdr l1) (cdr l2))] ; if operator is different, treat like a normal list
                            [else (helper (append acc (list (compare-lambdas-lists dict (car l1) (car l2)))) (cdr l1) (cdr l2))] ; operator the same, look for substitution
                        )]
                    [else (helper (append acc (list (compare-symbols dict (car l1) (car l2)))) (cdr l1) (cdr l2))]))])
    (helper '() l1 l2)))

(define (compare-lambdas l1 l2)
    (let ([ht (make-dict l1 l2)]) 
        (letrec 
            ([helper
                (lambda (res l1 l2)
                    (cond [(null? l1) res]
                          [(and (list? (car l1)) (list? (car l2))) (helper (append res (list (compare-lambdas-lists ht (car l1) (car l2)))) (cdr l1) (cdr l2))]
                          [else (helper (append res (list (compare-symbols ht (car l1) (car l2)))) (cdr l1) (cdr l2))]))])
        (helper '() l1 l2))))

(define test1 '(lambda (a) (f a)))
(define test2 '(lambda (a) (g a)))

;(compare-lambdas test1 test2)
(expr-compare 12 12)  ; ⇒  12
(expr-compare 12 20)  ; ⇒  (if % 12 20)
(expr-compare #t #t)  ; ⇒  #t
(expr-compare #f #f)  ; ⇒  #f
(expr-compare #t #f)  ; ⇒  %
(expr-compare #f #t)  ; ⇒  (not %)

;; Although (/ 1 0) would divide by zero if executed,
;; no division actually occurs here.
(expr-compare '(/ 1 0) '(/ 1 0.0))  ; ⇒  (/ 1 (if % 0 0.0))
;; Some of the later examples might also raise exceptions.

(expr-compare 'a '(cons a b))  ; ⇒  (if % a (cons a b))

(expr-compare '(cons a b) '(cons a b))  ; ⇒  (cons a b)

(expr-compare '(cons a lambda) '(cons a λ))  ; ⇒  (cons a (if % lambda λ))

(expr-compare '(cons (cons a b) (cons b c))
              '(cons (cons a c) (cons a c)))
  ; ⇒ (cons (cons a (if % b c)) (cons (if % b a) c))

(expr-compare '(cons a b) '(list a b))  ; ⇒  ((if % cons list) a b)

(expr-compare '(list) '(list a))  ; ⇒  (if % (list) (list a))

(expr-compare ''(a b) ''(a c))  ; ⇒  (if % '(a b) '(a c)) 

(expr-compare '(quote (a b)) '(quote (a c)))  ; ⇒  (if % '(a b) '(a c))

(expr-compare '(quoth (a b)) '(quoth (a c)))  ; ⇒  (quoth (a (if % b c)))

(expr-compare '(if x y z) '(if x z z))  ; ⇒  (if x (if % y z) z)

(expr-compare '(if x y z) '(g x y z))
  ; ⇒ (if % (if x y z) (g x y z))

(expr-compare '((lambda (a) (f a)) 1) 
              '((lambda (a) (g a)) 2))
  ; ⇒ ((lambda (a) ((if % f g) a)) (if % 1 2))

(expr-compare '((lambda (a) (f a)) 1) 
              '((λ (a) (g a)) 2))
  ; ⇒ ((λ (a) ((if % f g) a)) (if % 1 2))

(expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
  ; ⇒ ((lambda (a!b) a!b) (if % c d))
(expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
  ; ⇒ (if % '((λ (a) a) c) '((lambda (b) b) d))
(expr-compare '(+ #f ((λ (a b) (f a b)) 1 2)) 
               '(+ #t ((lambda (a c) (f a c)) 1 2)))
  ; ⇒ (+
  ;   (not %)
  ;   ((λ (a b!c) (f a b!c)) 1 2))

(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a b) (f b a)) 1 2))
  ; ⇒ ((λ (a b) (f (if % a b) (if % b a))) 1 2)


(expr-compare '((λ (a b) (f a b)) 1 2)
              '((λ (a c) (f c a)) 1 2))
  ; ⇒ ((λ (a b!c) (f (if % a b!c) (if % b!c a)))
  ;   1 2)

(expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
              '((lambda (if) (+ if if (f λ))) 3))
  ; ⇒ ((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3)
(expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
                                    a (lambda (a) a))))
                (lambda (b a) (b a)))
              '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
                                a (λ (b) a))))
                (lambda (a b) (a b))))
  ; ⇒ ((λ (a)
  ;    ((if % eq? eqv?)
  ;     a
  ;     ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
  ;      a (λ (a!b) (if % a!b a)))))
  ;   (lambda (b!a a!b) (b!a a!b))) 

#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Problem 1
(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride))))

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (letrec ([ith (remainder n (length xs))])
              (if (= ith 0)
                  (car xs)
                  (list-nth-mod (cdr xs) (- n 1))))]))

; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; Problem 5
(define funny-number-stream
  (letrec ([fn (lambda (y) (cond [(= (remainder (+ y 1) 5) 0)(* -1 (+ y 1))]
                                 [(< y 0) (+ 1 (* -1 y))]
                                 [(< (+ y 1) 5)(+ y 1)]
                                 [#t (+ y 1)]))]
           [f (lambda (x) (cons x (lambda () (f (fn x)))))])
    (lambda () (f 1))))

; Problem 6
(define dan-then-dog
  (let ([fn (lambda() (cons "dog.jpg" dan-then-dog))])
    (lambda () (cons "dan.jpg" fn))))

; Problem 7
(define (stream-add-zero s) (lambda () (cons (cons 0 (car (s))) (lambda () (stream-add-zero (cdr s))))))

; Problem 8 
(define (cycle-lists xs ys)(letrec ([rec-help (lambda (n arr) (cond
                                                                [(= n (length arr)) 0]
                                                                [#t (+ n 1)]))]
                                    [f (lambda (m n x-list y-list)(letrec ([x-inc (rec-help m x-list)]
                                                                           [y-inc (rec-help n y-list)]
                                                                           [xs-val (list-nth-mod x-list x-inc)]
                                                                           [ys-val (list-nth-mod y-list y-inc)])
                                                                    (lambda ()(cons (cons xs-val ys-val) (f x-inc y-inc x-list y-list)))))])
                             (f (length xs) (length ys) xs ys)))

; Problem 9
(define (vector-assoc v vec)
  (letrec ([containsV (lambda (val vs n)
                        (letrec ([v-ref (vector-ref vs n)])
                          (cond
                            [(and (pair? v-ref)(equal? val (car v-ref))) v-ref]
                            [#t (cond [(= n (- (vector-length vs) 1)) '()]
                                      [#t (containsV val vs (+ n 1))])])))]
           [assocVal (containsV v vec 0)])
    (cond
      [(pair? assocVal) assocVal]
      [#t #f])))

; Problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n)]
           [position 0]
           [f (lambda (x)
                (let* ([val (vector-assoc x cache)])
                  (if val
                      val
                      (let* ([ls-val (assoc x xs)])
                        (if ls-val
                            (begin
                              (vector-set! cache position ls-val)
                              (cond [(= position (- n 1)) (set! position 0)]
                                    [#t (+ position 1)])
                              ls-val)
                            #f)))))])
    f))

#lang racket

(provide (all-defined-out)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All 3 arguments are assumed to be numbers, and the argument stride is assumed
;; to be positive. This function will produce a list of nubmers form 'low' to
;; 'high' separated by stride and in sorted order.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sequence low high stride)
  (if (< high low)
      null
      (cons low (sequence (+ low stride) high stride))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function string-append-map that takes a list of strings xs and a
;; string suffix and returns a list of strings. Each element of the output should
;; be the corresponding element of the input appended with suffix (with no extra
;; space between the element and suffix). map and string-append are Racket library
;; functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function list-nth-mod that takes a list xs and a number n. If the number
;; is negative, terminate the computation with (error "list-nth-mod: negative number").
;; Else if the list is empty, terminate the computation with (error "list-nth-mod: empty
;; list"). Else return the ith element of the list where we count from zero and i is the
;; remainder produced when dividing n by the list’s length. Library functions length,
;; remainder, car, and list-tail are all useful – see the Racket documentation.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (letrec ([ith (remainder n (length xs))])
              (if (= ith 0)
                  (car xs)
                  (list-nth-mod (cdr xs) (- n 1))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function stream-for-n-steps that takes a stream s and a number n. It
;; returns a list holding the first n values produced by s in order. Assume n is
;; non-negative.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a stream funny-number-stream that is like the stream of natural numbers
;; (i.e., 1, 2, 3, ...) except numbers divisble by 5 are negated
;; (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream is a thunk
;; that when called produces a pair. Here the car of the pair will be a number and
;; the cdr will be another stream.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define funny-number-stream
  (letrec ([fn (lambda (y) (cond [(= (remainder (+ y 1) 5) 0)(* -1 (+ y 1))]
                                 [(< y 0) (+ 1 (* -1 y))]
                                 [(< (+ y 1) 5)(+ y 1)]
                                 [#t (+ y 1)]))]
           [f (lambda (x) (cons x (lambda () (f (fn x)))))])
    (lambda () (f 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a stream dan-then-dog,where the elements of the stream alternate between the
;; strings"dan.jpg" and "dog.jpg" (starting with "dan.jpg"). More specifically,
;; dan-then-dog should be a thunk that when called produces a pair of "dan.jpg" and
;; a thunk that when called produces a pair of "dog.jpg" and a thunk that when called...
;; etc.(define dan-then-dog
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define dan-then-dog
  (let ([fn (lambda() (cons "dog.jpg" dan-then-dog))])
    (lambda () (cons "dan.jpg" fn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function stream-add-zero that takes a stream s and returns another stream.
;; If s would produce v for its ith element, then (stream-add-zero s) would produce
;; the pair (0 . v) for its ith element.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (stream-add-zero s) (lambda () (cons (cons 0 (car (s))) (lambda () (stream-add-zero (cdr s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function cycle-lists that takes two lists xs and ys and returns a stream.
;; The lists may or may not be the same length, but assume they are both non-empty.
;; The elements produced by the stream are pairs where the first part is from xs and
;; the second part is from ys. The stream cycles forever through the lists. For
;; example, if xs is ’(1 2 3) and ys is ’("a" "b"), then the stream would produce,
;; (1 . "a"), (2 . "b"), (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"),
;; (2 . "b"), etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (cycle-lists xs ys)(letrec ([rec-help (lambda (n arr) (cond
                                                                [(= n (length arr)) 0]
                                                                [#t (+ n 1)]))]
                                    [f (lambda (m n x-list y-list)(letrec ([x-inc (rec-help m x-list)]
                                                                           [y-inc (rec-help n y-list)]
                                                                           [xs-val (list-nth-mod x-list x-inc)]
                                                                           [ys-val (list-nth-mod y-list y-inc)])
                                                                    (lambda ()(cons (cons xs-val ys-val) (f x-inc y-inc x-list y-list)))))])
                             (f (length xs) (length ys) xs ys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function vector-assoc that takes a value v and a vector vec. It should
;; behave like Racket’s assoc library function except (1) it processes a vector
;; (Racket’s name for an array) instead of a list, (2) it allows vector elements
;; not to be pairs in which case it skips them, and (3) it always takes exactly
;; two arguments. Process the vector elements in order starting from 0. You must
;; use library functions vector-length, vector-ref, and equal?. Return #f if no
;; vector element is a pair with a car field equal to v, else return the first pair
;; with an equal car field.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Write a function cached-assoc that takes a list xs and a number n and returns a
;; function that takes one argument v and returns the same thing that (assoc v xs)
;; would return. However, you should use an n-element cache of recent results to
;; possibly make this function faster than just calling assoc (if xs is long and a
;; few elements are returned often). The cache must be a Racket vector of length n
;; that is created by the call to cached-assoc (use Racket library function vector
;; or make-vector) and used-and-possibly-mutated each time the function returned by
;; cached-assoc is called. Assume n is positive.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
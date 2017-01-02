#lang racket

(provide (all-defined-out))

; Produce a list of numbers from low (inclusive) to high (possibly inclusive)
; separated by stride and in sorted order.
; contract: int * int * int -> int list
(define (sequence low high stride)
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride))))

; Produce a list of strings where each string is the corresponding string of xs
; appended with suffix.
; contract: string list * string -> string list
(define (string-append-map xs suffix)
    (map (lambda(x) (string-append x suffix)) xs))

; Return i element of list xs where i = n % length xs
; contract: a' list * int -> a'
(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; Produce a list of the first n values of the stream s in order.
; contract: lambda * int -> a' list
(define (stream-for-n-steps s n)
  (let ([es (car(s))])
    (if (= 0 n)
      null
      (cons es (stream-for-n-steps es (- n 1))))))

; Create a stream that is like the stream of natural numbers (1, 2, 3, ...)
; except numbers divisible by 5 are negated.
; Contract: () -> lambda
(define funny-number-stream
    (letrec ([f (lambda(x)
                  (let ([y (if (= 0 (remainder x 5)) (* -1 x) x)])
                  (cons y (lambda() (f (+ x 1))))))])
      (lambda() (f 1))))

; Create a stream whose elements alternate between the strings "dan.jpg" and "dog.jpg".
; contract: () -> lambda
(define dan-then-dog
  (letrec ([f (lambda(dan)
                (cons (if dan "dan.jpg" "dog.jpg") (lambda() (f (not dan)))))])
    (lambda() (f #t))))

; Create a stream from the stream s where each element is the corresponding element
; of s paired with 0. e.g. if v is element i of s then (0 . v) is element i of this function.
; contract: lambda -> lambda
(define (stream-add-zero s)
  (letrec ([f (lambda(s)
                (let ([es (s)])
                (cons (cons  0 (car es)) (lambda() (f (cdr es))))))])
    (lambda() (f s))))

; Create a stream whose elements are pairs created from the lists xs and ys.
; The stream cycles forever through the lists.
; contract: a' list * b' list -> lambda
(define (cycle-lists xs ys)
  (letrec ([f (lambda(n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda() (f (+ n 1)))))])
    (lambda() (f 0))))

; Return the first pair of vec whose car field equals v. Return #f if no vector
; element is a pair with a car field equal to v. Ignore non-pair fields of vec.
; contract: a' * vector -> pair
(define (vector-assoc v vec)
  (letrec ([f (lambda(n)
                (cond [(= n (vector-length vec)) #f]
                      [(let ([vn (vector-ref vec n)])
                          (and (pair? vn) (equal? (car vn) v)) vn)]
                      [#t (f (+ n 1))]))])
    (f 0)))

; Produce a function that behaves like an associative cache, i.e. takes one argument v
; and returns the value associated with v, or #f if there is no such association. The
; function uses a cache which is a vector whose size is specified by the argument n.
; The cache starts empty (all elements #f). When the function returned by cached-assoc
; is called, it first checks the cache for the answer. If it is not there, it uses assoc
; and xs to get the answer and if the result is not #f, it adds the pair to the cache.
; Cache slots are used in a round robin fashion.
(define (cached-assoc xs n)
  (let* ([cache (make-vector n #f)]
         [idx 0]
         [f (lambda(v)
              (let ([cache-ans (vector-assoc v cache)])
                (if cache-ans cache-ans
                    (let ([assoc-ans (assoc v xs)])
                      (if assoc-ans
                          (begin
                            (vector-set! cache idx assoc-ans)
                            (set! idx (remainder (+ 1 idx) n))
                            assoc-ans) #f)))))])
    f))

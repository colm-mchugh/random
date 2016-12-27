#lang racket

(provide (all-defined-out))

; 1
(define (sequence low high stride)
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride))))

; 2
(define (string-append-map xs suffix)
    (map (lambda(x) (string-append x suffix)) xs))

; 3
(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; 4
(define (stream-for-n-steps s n)
  (if (= 0 n)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; 5
(define funny-number-stream
    (letrec ([f (lambda(x)
                  (let ([y (if (= 0 (remainder x 5)) (* -1 x) x)])
                  (cons y (lambda() (f (+ x 1))))))])
      (lambda() (f 1))))

; 6
(define dan-then-dog
  (letrec ([f (lambda(dan)
                (cons (if dan "dan.jpg" "dog.jpg") (lambda() (f (not dan)))))])
    (lambda() (f #t))))

; 7
(define (stream-add-zero s)
  (letrec ([f (lambda(s)
                (cons (cons  0 (car (s))) (lambda() (f (cdr(s))))))])
    (lambda() (f s))))

; 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda(n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                      (lambda() (f (+ n 1)))))])
    (lambda() (f 0))))

; 9
(define (vector-assoc v vec)
  (letrec ([f (lambda(n)
                (cond [(= n (vector-length vec)) #f]
                      [(and (pair? (vector-ref vec n)) (equal? (car (vector-ref vec n)) v))
                       (vector-ref vec n)]
                      [#t (f (+ n 1))]))])
    (f 0)))

; 10
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

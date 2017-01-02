;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs 
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

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

; Take a Racket list of MUPL values and produce an analagous MUPL list of the same elements
; in the same order
(define (racketlist->mupllist rl)
  (if (null? rl)
      (aunit)
      (apair (car rl) (racketlist->mupllist (cdr rl)))))

; Take a MUPL list and produce a Racket list of the same elements in the same order.
(define (mupllist->racketlist ml)
  (if (aunit? ml)
      null
      (cons (apair-e1 ml) (mupllist->racketlist (apair-e2 ml)))))

; Return the variable associated with the given string in the given environment,
; or an error if no (str -> var) binding exists in env.
; contract: (string . var) list * string -> var | error
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

; evaluate the MUPL expression e in the environment env
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
        [(mlet? e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(fun? e)
         (closure env e)]
        [(call? e)
         (let ([f (eval-under-env (call-funexp e) env)]
               [arg (eval-under-env (call-actual e) env)])
           (if (closure? f)
               (let* ([fclsr (closure-fun f)]
                      [fname (fun-nameopt fclsr)]
                      [fenv (closure-env f)]
                      [fenv-ext (cons (cons (fun-formal fclsr) arg) (if (eq? #f fname) fenv (cons (cons fname f) fenv)))])
                 (eval-under-env (fun-body fclsr) fenv-ext))
               (error "MUPL call applied to non-function")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env) (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v) (apair-e1 v) (error "MUPL fst applied to non-list")))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v) (apair-e2 v) (error "MUPL snd applied to non-list")))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v) (int 1) (int 0)))]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

; evaluate the MUPL expression in an empty environment.
(define (eval-exp e)
  (eval-under-env e null))
        
; Return a MUPL expression which when run evaluates e1 and if the result is MUPL aunit
; then it evaluates and returns e2, else it evaluates and returns e3
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

; mlet* takes a racket list of racket pairs of (string . MUPL expression) and a MUPL expression e2
; and returns a MUPL expression whose value is e2 evaluated in an environment where each pair of lstlst
; is evaluated sequentially and the MUPL expression result bound to its pair'd string. 
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([e (car lstlst)])
        (mlet (car e) (cdr e) (mlet* (cdr lstlst) e2)))))

; Return a MUPL expression that takes four MUPL expressions e1, e2, e3, e4 and if e1 and e2 are equal
; integers then return e3 else return e4
(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1 (mlet "_y" e2
                      (ifgreater (var "_x") (var "_y") e4
                                                     (ifgreater (var "_y") (var "_x") e4 e3)))))

; mupl-map is a curried function that acts like map (in ML); it takes a MUPL function mapper and
; returns a MUPL function that takes a MUPL list and applies mapper to every element of the list
; returning a new MUPL list. A MUPL list is aunit or a pair where the second component is a MUPL list.
(define mupl-map (fun #f "mapper"
                      (fun "list-loop" "xs"
                           (ifaunit (var "xs")
                                    (aunit)
                                    (apair (call (var "mapper") (fst (var "xs")))
                                           (call (var "list-loop") (snd (var "xs"))))))))

; Bind to mupl-mapAddN a MUPL function that takes a MUPL integer i and returns a MUPL function that
; takes a MUPL list of MUPL integers and returns a new MUPL list of MUPL integers that adds i to every
; element of the list.
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun #f "i"
             (call (var "map")
                   (fun #f "arg" (add (var "i") (var "arg")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

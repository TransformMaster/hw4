#lang racket
(require "ast.rkt")
(provide intern)

;; Expr -> Expr
(define (intern e)
  ;; TODO
  (let ((xs (intern-list e))) (let ((xs1 (intern-dict xs '()))) (intern-change e xs1 xs1)))
  )

(define (intern-list e)
  (match e
    [(Int i)  '()]
    [(Bool b) '()]
    [(Char c) '()]
    [(Eof)    '()]
    [(Empty)  '()]
    [(Var x)  '()]
    [(Str s)  (list s)] 
    [(Prim0 'void) '()]
    [(Prim0 'read-byte) '()]
    [(Prim0 'peek-byte) '()]
    [(Prim1 p e)
     (intern-list e)]
    [(Prim2 p e1 e2)
     (append (intern-list e1) (intern-list e2))]
    [(Prim3 p e1 e2 e3)
     (append (append (intern-list e1) (intern-list e2)) (intern-list e3))]
    [(If p e1 e2)
     (append (append (intern-list p) (intern-list e1)) (intern-list e2))]
    [(Begin e1 e2)
     (append (intern-list e1) (intern-list e2))]
    [(Let x e1 e2)
     (append (intern-list e1) (intern-list e2))])
  )

(define (intern-dict x1 x2)
  (match x1
    ['() '()]
    [(cons x xs) (if (member x x2) (intern-dict xs x2) (cons (cons x (gensym)) (intern-dict xs (cons x x2))))]
    )
  )

(define (intern-change e xs1 xs2)
  (match xs1
    ['() (intern-function e xs2)]
    [(cons x xs) (match x
                   [(cons a b) (Let b (Str a) (intern-change e xs xs2))]
                   )]
    )
  )

(define (intern-function e xs)
  (match e
    [(Int i)  (Int i)]
    [(Bool b) (Bool b)]
    [(Char c) (Char c)]
    [(Eof)    (Eof)]
    [(Empty)  (Empty)]
    [(Var x)  (Var x)]
    [(Str s)  (Var (dict-ref xs s))] 
    [(Prim0 'void) (Prim0 'void)]
    [(Prim0 'read-byte) (Prim0 'read-byte)]
    [(Prim0 'peek-byte) (Prim0 'peek-byte)]
    [(Prim1 p e)
     (Prim1 p (intern-function e xs))]
    [(Prim2 p e1 e2)
     (Prim2 p (intern-function e1 xs) (intern-function e2 xs))]
    [(Prim3 p e1 e2 e3)
     (Prim3 p (intern-function e1 xs) (intern-function e2 xs) (intern-function e3 xs))]
    [(If p e1 e2)
     (If (intern-function p xs) (intern-function e1 xs) (intern-function e2 xs))]
    [(Begin e1 e2)
     (Begin (intern-function e1 xs) (intern-function e2 xs))]
    [(Let x e1 e2)
     (Let x (intern-function e1 xs) (intern-function e2 xs))])
  )
#lang plai-typed

(define-type ArithS
  [numS (n : number)]
  [uminusS (e : ArithS)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)])

; parser from chapter 2
(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (cond ((eq? (length sl) 2) (uminusS (parse (second sl))))
                    (else (bminusS (parse (second sl)) (parse (third sl)))))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    ;[uminusS (e) (desugar (bminusS (numS 0) e))]
    [uminusS (e) (multC (numC -1) (desugar e))] ;another way to desugar -n
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))

(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (s) ]
    [appC (func args) ]
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]))

(test (interp (desugar (parse '(+ (* 1 2) (+ 2 3))))) 7)
(test (interp (desugar (parse '(- (+ 1 2))))) -3)
(test (interp (desugar (parse '(- (- (* 3 3)))))) 9)

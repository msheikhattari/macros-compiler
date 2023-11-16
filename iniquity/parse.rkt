#lang racket
(provide parse parse-define parse-e naive-parse)
(require "ast.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match (naive-parse s)
    [(Prog ds e) (Prog (cleanup ds) (substitute-e ds e))]))

(define (naive-parse s)
  (match s
    [(or (cons (and (cons 'define-macro _) d) s) (cons (and (cons 'define _) d) s))
      (match (parse s)
        [(Prog ds e)
          (Prog (cons (parse-define d) ds) e)])]
    [(cons e '()) (Prog '() (parse-e e))]
    [_ (error "program parse error")]))

(define (substitute-e ds e)
  (match e
    [(Prim1 p e1) (Prim1 p (substitute-e ds e1))]
    [(Prim2 p e1 e2) (Prim2 p (substitute-e ds e1) (substitute-e ds e2))]
    [(Prim3 p e1 e2 e3) (Prim3 p (substitute-e ds e1) (substitute-e ds e2) (substitute-e ds e3))]
    [(Begin e1 e2) (Begin (substitute-e ds e1) (substitute-e ds e2))]
    [(If e1 e2 e3) (If (substitute-e ds e1) (substitute-e ds e2) (substitute-e ds e3))]
    [(Let x e1 e2) (Let x (substitute-e ds e1) (substitute-e ds e2))]
    [(App f es) 
      (match (defns-lookup ds (string->symbol (string-append "macro" (symbol->string f)))) 
        [#f (App f (map (curry substitute-e ds) es))]
        [(Defn f xs e) (if (= (length xs) (length es))
                        (substitute (zip xs es) e)
                        'err)])]
    [(Defn f xs e) (Defn f xs (substitute-e ds e))]
    [_ e]))

(define (substitute vs e)
  (match e
    [(Prim1 p e1) (Prim1 p (substitute vs e1))]
    [(Prim2 p e1 e2) (Prim2 p (substitute vs e1) (substitute vs e2))]
    [(Prim3 p e1 e2 e3) (Prim3 p (substitute vs e1) (substitute vs e2) (substitute vs e3))]
    [(Begin e1 e2) (Begin (substitute vs e1) (substitute vs e2))]
    [(If e1 e2 e3) (If (substitute vs e1) (substitute vs e2) (substitute vs e3))]
    [(Let x e1 e2) (Let x (substitute vs e1) (substitute vs e2))]
    [(App f es) (App f (map (curry substitute vs) es))] 
    [(Var x) (match (vars-lookup vs x)
      [(cons x1 e1) (car e1)]
      [#f (Var x)])]
    [_ e]))

(define (cleanup ds)
  (match (map (curry substitute-e ds) ds)
    [(cons (Defn f xs e) d2) (if (string-prefix? (symbol->string f) "macro") (cleanup d2) ds)]
    ['() '()]))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (Defn f xs (parse-e e))
         (error "parse definition error"))]
    [(list 'define-macro (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (Defn (string->symbol (string-append "macro" (symbol->string f))) xs (parse-e e))
         (error "parse definition error"))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? exact-integer?)            (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    [(? string?)                   (Str s)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? (op? op3) p3) e1 e2 e3)
     (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons (? symbol? f) es)
     (App f (map parse-e es))]
    [_ (error "Parse error" s)]))

(define op0
  '(read-byte peek-byte void))

(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length))
(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref))
(define op3
  '(vector-set!))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))

(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (vars-lookup vs x)
  (findf (match-lambda [(cons x1 e1) (eq? x1 x)])
         vs))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))
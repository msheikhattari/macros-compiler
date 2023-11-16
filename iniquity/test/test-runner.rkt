#lang racket
(provide test-runner test-runner-io)
(require rackunit)

(define (test-runner run)
  ;; Iniquity tests
  (check-equal? (run
                 '(define (f x) x)
                 '(f 5))
                5)
  (check-equal? (run
                 '(define (tri x)
                    (if (zero? x)
                        0
                        (+ x (tri (sub1 x)))))
                 '(tri 9))
                45)

  (check-equal? (run
                 '(define (even? x)
                    (if (zero? x)
                        #t
                        (odd? (sub1 x))))
                 '(define (odd? x)
                    (if (zero? x)
                        #f
                        (even? (sub1 x))))
                 '(even? 101))
                #f)

  (check-equal? (run
                 '(define (map-add1 xs)
                    (if (empty? xs)
                        '()
                        (cons (add1 (car xs))
                              (map-add1 (cdr xs)))))
                 '(map-add1 (cons 1 (cons 2 (cons 3 '())))))
                '(2 3 4))
  (check-equal? (run '(define (f x y) y)
                     '(f 1 (add1 #f)))
                'err)

   ;; Macros tests      
   (check-equal? (run '(define-macro (and a b) (if a (if b b #f) #f)) 
                      '(define (double x) (+ x x)) 
                      '(+ (double 3) (and #t 42)))
                  48)           
   (check-equal? (run '(define-macro (double x) (+ x x)) 
                      '(if (double 2) (double 3) (double 4))) 6)

   (check-equal? (run '(define-macro (and a b) (if a (if b b #f) #f)) 
                      '(define-macro (double x) (+ x x)) 
                      '(if (and 1 #f) (and 2 3) (double 3))) 6)

   (check-equal? (run '(define-macro (g x y) (+ x y)) 
                      '(define (f x y) (g x y)) 
                      '(f 1 2)) 3)

   (check-equal? (run '(define-macro (g x y) (f x y)) 
                      '(define (f x y) (+ x y)) 
                      '(g 1 2)) 3)   

   (check-equal? (run '(define-macro (g x y) (+ x y)) 
                      '(add1 (g 1 2))) 4)   

   (check-equal? (run '(define-macro (g x y) (+ x y)) 
                      '(+ (g 1 2) (g 1 2))) 6)   

   (check-equal? (run '(define-macro (g x y) (+ x y)) 
                      '(begin (g 1 2) (g 1 2))) 3)

   (check-equal? (run '(define-macro (g x y) (+ x y)) 
                      '(let ((x (g 1 2))) (g x 2))) 5)
   
   (check-equal? (run '(define-macro (g x y) (+ x y)) 
                      '(define (f x y) (+ x y)) 
                      '(f (g 1 2) (g 1 2))) 6)   

   (check-equal? (run '(define-macro (g x) (add1 x)) 
                      '(add1 (g 1))) 3) 

   (check-equal? (run '(define-macro (g x y) (begin x y)) 
                      '(add1 (g 1 2))) 3)

   (check-equal? (run '(define-macro (g x) (let ((y 1)) (+ x y)))
                      '(add1 (g 1))) 3))

(define (test-runner-io run)
  ;; Evildoer examples
  (check-equal? (run "" 7) (cons 7 ""))
  (check-equal? (run "" '(write-byte 97)) (cons (void) "a"))
  (check-equal? (run "a" '(read-byte)) (cons 97 ""))
  (check-equal? (run "b" '(begin (write-byte 97) (read-byte)))
                (cons 98 "a"))
  (check-equal? (run "" '(read-byte)) (cons eof ""))
  (check-equal? (run "" '(eof-object? (read-byte))) (cons #t ""))
  (check-equal? (run "a" '(eof-object? (read-byte))) (cons #f ""))
  (check-equal? (run "" '(begin (write-byte 97) (write-byte 98)))
                (cons (void) "ab"))

  (check-equal? (run "ab" '(peek-byte)) (cons 97 ""))
  (check-equal? (run "ab" '(begin (peek-byte) (read-byte))) (cons 97 ""))
  ;; Extort examples
  (check-equal? (run "" '(write-byte #t)) (cons 'err ""))

  ;; Fraud examples
  (check-equal? (run "" '(let ((x 97)) (write-byte x))) (cons (void) "a"))
  (check-equal? (run ""
                     '(let ((x 97))
                        (begin (write-byte x)
                               x)))
                (cons 97 "a"))
  (check-equal? (run "b" '(let ((x 97)) (begin (read-byte) x)))
                (cons 97 ""))
  (check-equal? (run "b" '(let ((x 97)) (begin (peek-byte) x)))
                (cons 97 ""))

  ;; Hustle examples
  (check-equal? (run ""
                     '(let ((x 1))
                        (begin (write-byte 97)
                               1)))
                (cons 1 "a"))

  (check-equal? (run ""
                     '(let ((x 1))
                        (let ((y 2))
                          (begin (write-byte 97)
                                 1))))
                (cons 1 "a"))

  (check-equal? (run ""
                     '(let ((x (cons 1 2)))
                        (begin (write-byte 97)
                               (car x))))
                (cons 1 "a"))
  ;; Iniquity examples
  (check-equal? (run ""
                     '(define (print-alphabet i)
                        (if (zero? i)
                            (void)
                            (begin (write-byte (- 123 i))
                                   (print-alphabet (sub1 i)))))
                     '(print-alphabet 26))
                (cons (void) "abcdefghijklmnopqrstuvwxyz"))

  (check-equal? (run ""
                     '(define (f x)
                        (write-byte x))
                     '(f 97))
                (cons (void) "a"))  
  (check-equal? (run ""
                     '(define (f x y)
                        (write-byte x))
                     '(f 97 98))
                (cons (void) "a"))  
  (check-equal? (run ""
                     '(define (f x)
                        (let ((y x))
                          (write-byte y)))
                     '(f 97))
                (cons (void) "a"))
  (check-equal? (run ""
                     '(define (f x y)
                        (let ((y x))
                          (write-byte y)))
                     '(f 97 98))
                (cons (void) "a"))  
  (check-equal? (run ""
                     '(define (f x)
                        (write-byte x))
                     '(let ((z 97))
                        (f z)))
                (cons (void) "a"))  
  (check-equal? (run ""
                     '(define (f x y)
                        (write-byte x))
                     '(let ((z 97))
                        (f z 98)))
                (cons (void) "a")))

   
  
  

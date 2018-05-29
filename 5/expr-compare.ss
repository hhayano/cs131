; Checks if the argument is an atom or not
; This helps determine if we can use car/cdr or not
(define (atom? x)
  (not (or (pair? x) (null? x))))

(define (expr-compare x y)
  (if (equal? x y) 
    ; If equal, return immediately
    x
    ; If not, start comparing the expressions.
    (create-diff x y)))

; Create the diff expression.
(define (create-diff x y)
  (cond 
    [(and (boolean? x) (boolean? y))
      (diff-bool x y)]
    [(and (not (atom? x)) (not (atom? y)))
      (diff-list x y)]
    [else
      (diff-atom x y)]))

; Special case for bool atoms
(define (diff-bool x y)
  (if x '% '(not %)))

; Special case for when either x or y is an atom
(define (diff-atom x y)
  (list 'if '% x y))

; Meat of the project. The possible cases are listed below
; 
; Cases
;   1. constant literals
;   2. variable references
;   3. procedure calls
;   4. special forms
;     - (quote datum)
;     - (lambda formals body)
;     - (let bindings body)
;     - (if expr expr)
;
; If 3, we must make sure that the procedure calls match
; If 4, we must make sure that the special forms match
; Assume that the inputs are always lists
(define (diff-list x y)
  (if (or (null? x) (null? y))
    (list 'if '% x y)
    (let ([x_head (car x)] [y_head (car y)] [x_tail (cdr x)] [y_tail (cdr y)])
      (if (and (equal? x_head y_head) (= (length x) (length y)))
        (cond 
          [(equal? x_head 'quote)
            (diff-quote x y)]
          [(or (equal? x_head 'lambda) (equal? x_head 'let))
            (diff-bound x_tail y_tail)]
          [else
            (cons x_head (diff-expr x_tail y_tail))])
        (list 'if '% x y)))))

; diff-quote simply compares the two arguments
(define (diff-quote x y)
  (let ([x_h (car x)] [y_h (car y)])
    (if (equal? x_h y_h)
      x_h 
      (list 'if '% x_h y_h))))



; Assumptions
;   - All inputs are either one symbol long or a list of symbols and inner functions
;   - Each function will be encased in parenthesis
;   - Each function will therefore be a list of symbols, first one representing the type of function
;     - need to watch out for different syntactic forms
;   - The first symbol of each function
; Two possibilities
;   1. Input is a list of sorts
;   2. Input is one symbol
; 
; Case 1. 
;   Take the head (car) of the list. If it is a symbol, check if its one of the special forms. If not, then it must be a procedure call. 
;
;
;
; Possibilities:
;   - constant literals
;   - variable references
;   - procedure calls
;   - special forms
;     - (quote datum)
;     - (lambda formals body)
;     - (let bindings body)
;     - (if expr expr)

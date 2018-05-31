; xor
(define (xor x y)
  (or 
    (and x (not y))
    (and y (not x))))

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
  (if (and (not (atom? x)) (not (atom? y)))
    (diff-list x y)
    (diff-atom x y)))

; Special case for when either x or y is an atom
(define (diff-atom x y)
  (if (and (boolean? x) (boolean? y))
    (if x '% '(not %))
    (list 'if '% x y)))

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
;
; Returns a list corresponding to the diff of the two lists
(define (diff-list x y)
  ; First check if both the inputs are null or not
  ;   if so, return null
  (if (and (null? x) (null? y))
    '()

    ; Second check if either of the inputs are null or not
    ;   if so, show the fact that one expression is null
    (if (or (null? x) (null? y))
      (list 'if '% x y)
      (let ([x_head (car x)] [y_head (car y)] [x_tail (cdr x)] [y_tail (cdr y)])
        ; Next compare the procedure/syntax names.
        ;   If they are equal, look to see if it is a special form
        (if (and (equal? x_head y_head) (= (length x) (length y)))
          (cond 
            [(equal? x_head 'quote)
              (diff-quote x y)]
;            [(or (equal? x_head 'lambda) (equal? x_head 'let))
;              (cons x_head (diff-bound x_tail y_tail))]
            [else
              (cons x_head (diff-expr x_tail y_tail))])

          ; If they are not equal, check if either call is a special form or if the list length does not match up
          ;   if so, we must encase the entire expression in an if
          (if (or (special? x_head y_head) (not (= (length x) (length y))))
            (list 'if '% x y)
            
            ; otherwise we can just if the starting argument and see if the rest of the arguments differ
            (cons (list 'if '% x_head y_head) (diff-expr x_tail y_tail))))))))
           
; special? will determine if the expressions are special forms or not
(define (special? x y)
  (or
    (equal? x 'quote)
    (equal? y 'quote)
    (equal? x 'lambda)
    (equal? y 'lambda)
    (equal? x 'let)
    (equal? y 'let)
    (equal? x 'if)
    (equal? y 'if)))

; diff-quote simply compares the two arguments
(define (diff-quote x y)
  (if (equal? x y)
    x 
    (list 'if '% x y)))

; diff-expr will be recursively called to compare all of the arguments of the procedure call
; Two cases
;   - the arguments are a list, suggesting that they are another procedure call/special form
;   - the arguments are symbols
;
; Base case
;   - if both args are null, return null. If either x or y only has one element left while the other has more than one element left, encase the entire expression in an if block.
;
; Assume that the inputs are in list form
; 
; Returns a list corresponding to the diff of the two argument lists
(define (diff-expr x_list y_list)
  (if (and (null? x_list) (null? y_list))
    '()
    (let ([x (car x_list)] [y (car y_list)] [x_t (cdr x_list)] [y_t (cdr y_list)])
      (if (equal? x y)
        (cons x (diff-expr x_t y_t))

        ; Two cases
        ;   - x and y are lists
        ;   - x and y are just symbols
        (if (and (list? x) (list? y) (= (length x) (length y)))
          ; If they are lists, use diff-list
          (cons (diff-list x y) (diff-expr x_t y_t))

          ; In any other case, if either x or y is a list, they don't match
          (if (or (list? x) (list? y))
            (cons (list 'if '% x y) (diff-expr x_t y_t))

            ; If neither x or y is a list, they both must be symbols
            (cons (diff-atom x y) (diff-expr x_t y_t))))))))

; diff-bound will deal with the variable binding in let and lambda
; first lambda
;
; assume input is correct
;
; returns a list
(define (diff-lambda x_list y_list)
  ; expecting the first element in both lists to be the bounding list
  (let ([x_bounds (car x_list)] [y_bounds (car y_list)] [x_body (cadr x_list)] [y_body (cadr y_list)])
    (cons (diff-lambda-bounds x_bounds y_bounds) (diff-lambda-body x_body y_body (bounds-list x_bounds y_bounds)))))

; diff-lambda-bounds
(define (diff-lambda-bounds x_list y_list)
  (if (and (null? x_list) (null? y_list))
    '()
    (let ([x (car x_list)] [y (car y_list)] [x_t (cdr x_list)] [y_t (cdr y_list)])
      (if (equal? x y)
        (cons x (diff-lambda-bounds x_t y_t))
        (cons 
          (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
          (diff-lambda-bounds x_t y_t))))))

(define (bounds-list x_list y_list)
  (if (and (null? x_list) (null? y_list))
    '()
    (let ([x (car x_list)] [y (car y_list)] [x_t (cdr x_list)] [y_t (cdr y_list)])
      (if (equal? x y)
        (bounds-list x_t y_t)
        (cons 
          (list x y (string->symbol (string-append (symbol->string x) "!" (symbol->string y))))
          (bounds-list x_t y_t))))))

;(define (diff-lambda-body x_list y_list bounds)






(define (true expr)
  (eval `(let ([% #t]), expr)))

(define (false expr)
  (eval `(let ([% #f]), expr)))

(define (test-expr-compare x y)
  (let ([comp_res (expr-compare x y)])
    (and 
      ; first test x
      (equal?
        (eval x)
        (true comp_res))

      ; next test y
      (equal?
        (eval y)
        (false comp_res)))))

(define (test test_cases)
  (if (null? test_cases)
    '()
    (let ([first_test_case (car test_cases)] [rest (cdr test_cases)])
      (let ([x (car first_test_case)] [y (cadr first_test_case)])
        (cons (test-expr-compare x y) (test rest))))))

(define test_cases
  (list
    (list 12 20)
    (list #t #f)
    (list #f #t)
    (list 'a '(cons a b))
    (list '(cons a b) '(cons a c))
    (list 
      '(cons (cons a b) (cons b c))
      '(cons (cons a c) (cons a c)))
    (list '(cons a b) '(list a b))
    (list '(list) '(list a))
    (list ''(a b) ''(a c))
    (list '(quote (a b)) '(quote (a c)))
    (list '(quoth (a b)) '(quoth (a c)))
    (list '(if x y z) '(if x z z))
    (list '(if x y z) '(g x y z))))

(define test-expr-x
  '(+ 3 (let ((a 1) (b 2)) (list a b))))

(define test-expr-y
  '(+ 2 (let ((a 1) (c 2)) (list a c))))

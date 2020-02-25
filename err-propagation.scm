(use-modules (ice-9 format)
             (ice-9 match))

(define (same-variable? a b)
  (and (symbol? b) (eq? a b)))

(define (num-eq? var num)
  (and (number? var) (= var num)))

(define (both-num? a b)
  (and (number? a) (number? b)))

(define ^ expt)
(define e (exp 1))

(define (+! a b)
  (cond ((num-eq? a 0) b)
        ((num-eq? b 0) a)
        ((both-num? a b) (+ a b))
        (else (list '+ a b))))

(define (-! a b)
  (cond ((num-eq? a 0) (if (number? b) (- b) (list '- b)))
        ((num-eq? b 0) a)
        ((both-num? a b) (- a b))
        (else (list '- a b))))

(define (*! a b)
  (cond ((or (num-eq? a 0) (num-eq? b 0)) 0)
        ((num-eq? a 1) b)
        ((num-eq? b 1) a)
        ((both-num? a b) (* a b))
        (else (list '* a b))))

(define (/! a b)
  (cond ((num-eq? a 0) 0)
        ((num-eq? b 1) a)
        ((both-num? a b) (/ a b))
        (else (list '/ a b))))

(define (^! a b)
  (cond ((num-eq? a 1) 1)
        ((num-eq? b 0) 1)
        ((num-eq? b 1) a)
        ((both-num? a b) (^ a b))
        (else (list '^ a b))))

(define (log! a)
  (cond ((num-eq? a 1) 0)
        ((num-eq? a e) 1)
        ((number? a) (log a))
        (else (list 'log a))))

(define (sin! x)
  (cond ((number? x) (sin x))
        (else (list 'sin x))))

(define (cos! x)
  (cond ((number? x) (cos x))
        (else (list 'cos x))))

(define (derive expr var)
  ;; Make a derivative of expr with respect to var.
  (match expr
    ((? number? _) 0)
    ((? symbol? _) (if (same-variable? expr var) 1 0))

    (('+ a b) (+! (derive a var)
                  (derive b var)))
    (('- a b) (-! (derive a var)
                  (derive b var)))

    (('* a b) (+! (*! a (derive b var))
                  (*! (derive a var) b)))
    (('/ a b) (/! (-! (*! (derive a var) b)
                      (*! a (derive b var)))
                  (^! b 2)))

    (('exp x) (*! (derive x var) expr))
    (('log x) (/! (derive x var) x))
    (('^ a b) (*! (+! (*! (derive a var) (/! b a))
                      (*! (derive b var) (log! a)))
                  expr))

    (('sin x) (*! (derive x var) (cos! x)))
    (('cos x) (*! (derive x var) (-! 0 (sin! x))))

    (_ (error "unknown expression type" expr))))

(define (err-propagate expr vars)
  ;; Make an error propagation formula for the given expression and variables.
  ;; Usage: (err-propagate <expr> '((<var> <var-error>) ...))

  (define (square-sum elems varl)
    ;; Loop over all variables in varl.
    (if (null? varl)
        ;; If the variable list is empty, turn the list of squares into a sum and return.
        (cons '+ elems)
        ;; Otherwise, add (derivative * var-error) ^ 2 to the list
        (let* ((var (caar varl))        ; Symbol of the current variable
               (var-error (cadar varl)) ; Error for current variable
               (expr-by-var (derive expr var))) ; Derivative of expr with respect to var
          (square-sum (cons (^! (*! expr-by-var var-error) 2) elems) (cdr varl)))))
  ;; The result is the square root of the sum.
  (list 'sqrt (square-sum '() vars)))

(define (expr-to-infix-str expr)
  ;; Display an expression in infix notation.
  (define default-name-map '((exp . "EXP") (log . "LOG") (sin . "SIN") (cos . "COS") (sqrt . "SQRT")))
  (define op-precedence-map '((+ . 1) (- . 1) (* . 2) (/ . 2) (^ . 3)))

  (define (exact-fract? x) (and (number? x) (not (integer? x)) (exact? x)))

  (define (wrap-in-parens x) (format #f "(~a)" x))

  (define (subexpr-to-str part order)
    ;; Convert subexpression into a string
    (let ((part-str (expr-to-infix-str part)))
      (match part
        ((or (? number? _) (? symbol? _)
             ((or 'exp 'log 'sin 'cos 'sqrt) _)) part-str)
        ;; Put parens around part if it has a binary operator
        ;; and the previous operator has higher precedence
        ((op _ _) (if (<= (assq-ref op-precedence-map op) order)
                      (wrap-in-parens part-str)
                      part-str))
        (_ (error "unknown expression type" part)))))

  (match expr
    ((? exact-fract? x) (wrap-in-parens x)) ; Fractions
    ((? number? x) (number->string x))      ; Floats and integers
    ((? symbol? x) (symbol->string x))      ; Variable names

    (((and (or 'exp 'log 'sin 'cos 'sqrt) op) x)  ; Single-argument functions
     (format #f "~a(~a)" (assq-ref default-name-map op) (expr-to-infix-str x)))

    (((and (or '+ '- '* '/ '^) op) a b)
     (let* ((op-order (assq-ref op-precedence-map op))
            (a-str (subexpr-to-str a op-order))
            (b-str (subexpr-to-str b op-order)))
       (format #f "~a ~a ~a" a-str op b-str))) ; Two-argument infix operators

    (_ (error "unknown expression type" expr))))

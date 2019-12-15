(define (variable? x) (symbol? x))

(define (same-variable? a b)
  (and (variable? a) (variable? b) (eq? a b)))

(define (num-eq? var num)
  (and (number? var) (= var num)))

(define (both-num? a b)
  (and (number? a) (number? b)))

(define ^ expt)
(define e (exp 1))

(define (make-sum a b)
  (cond ((num-eq? a 0) b)
        ((num-eq? b 0) a)
        ((both-num? a b) (+ a b))
        (else (list '+ a b))))

(define (make-diff a b)
  (cond ((num-eq? a 0) (list '- b))
        ((num-eq? b 0) a)
        ((both-num? a b) (- a b))
        (else (list '- a b))))

(define (make-product a b)
  (cond ((or (num-eq? a 0) (num-eq? b 0)) 0)
        ((num-eq? a 1) b)
        ((num-eq? b 1) a)
        ((both-num? a b) (* a b))
        (else (list '* a b))))

(define (make-fraction a b)
  (cond ((num-eq? a 0) 0)
        ((num-eq? b 1) a)
        ((both-num? a b) (/ a b))
        (else (list '/ a b))))

(define (make-power a b)
  (cond ((num-eq? a 1) 1)
        ((num-eq? b 0) 1)
        ((num-eq? b 1) a)
        ((both-num? a b) (^ a b))
        (else (list '^ a b))))

(define (make-log a)
  (cond ((num-eq? a 1) 0)
        ((num-eq? a e) 1)
        ((number? a) (log a))
        (else (list 'log a))))

(define first-arg cadr)
(define second-arg caddr)

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (deriv-sum expr var)
  (make-sum (deriv (first-arg expr) var)
            (deriv (second-arg expr) var)))

(define (diff? x) (and (pair? x) (eq? (car x) '-)))
(define (deriv-diff expr var)
  (make-diff (deriv (first-arg expr) var)
             (deriv (second-arg expr) var)))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (deriv-product expr var)
  (make-sum
   (make-product (first-arg expr)
                 (deriv (second-arg expr) var))
   (make-product (deriv (first-arg expr) var)
                 (second-arg expr))))

(define (fraction? x) (and (pair? x) (eq? (car x) '/)))
(define (deriv-fraction expr var)
  (make-fraction
   (make-diff (make-product (deriv (first-arg expr) var)
                            (second-arg expr))
              (make-product (first-arg expr)
                            (deriv (second-arg expr) var)))
   (make-power (second-arg expr) 2)))

(define (exp? x) (and (pair? x) (eq? (car x) 'exp)))
(define (deriv-exp expr var)
  (make-product (deriv (second-arg expr) var) expr))

(define (log? x) (and (pair? x) (eq? (car x) 'log)))
(define (deriv-log expr var)
  (make-fraction (deriv (first-arg expr) var) (first-arg expr)))

(define (power? x) (and (pair? x) (eq? (car x) '^)))
(define (deriv-power expr var)
  (make-product (make-sum (make-product (deriv (first-arg expr) var)
                                        (make-fraction (second-arg expr)
                                                       (first-arg expr)))
                          (make-product (deriv (second-arg expr) var)
                                        (make-log (first-arg expr))))
                 expr))

(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr) (if (same-variable? expr var) 1 0))

        ((sum? expr) (deriv-sum expr var))
        ((diff? expr) (deriv-diff expr var))

        ((product? expr) (deriv-product expr var))
        ((fraction? expr) (deriv-fraction expr var))

        ((exp? expr) (deriv-exp expr var))
        ((log? expr) (deriv-log expr var))

        ((power? expr) (deriv-power expr var))

        (else
         (error "unknown expression type" expr))))

(define (err-propagate expr vars)
  (define (square-sum elems varl)
    (if (null? varl)
        (cons '+ elems)
        (square-sum (cons (list '^ (list '* (deriv expr (caar varl))
                                            (cadar varl))
                                2)
                          elems)
                    (cdr varl))))
  (list 'sqrt (square-sum '() vars)))

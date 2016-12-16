#|(load "pattern-matcher.scm")|#

#| .:: code from Mayer's expand-qq file ::. ______________________________________________________________________________________________________________________________________________________|#

#|(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
           (eq? (car e) tag)
           (pair? (cdr e))
           (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
         (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
           simple-sexprs-predicates)
          (quote? e)))))|#

#| .:: tools and constants ::. ______________________________________________________________________________________________________________________________________________________|#

#|(define not-const? (lambda (e) (not (const? e))))
(define id (lambda (e) e))|#
(define not-list? (lambda (s) (not (list? s))))
#|(define *reserved-words* '(and begin cond define do else if lambda let let* letrec or quasiquote unquote unquote-splicing quote set!))|#


#|(define *optimizable-operators* '(+ - * / append and begin cond define do else if lambda let let* letrec or quasiquote unquote quote set!))

(define optimizable-op?
  (lambda (x)
    (member x *optimizable-operators*)))

(define not-optimizable-op?
  (lambda (x)
    (not (optimizable-op? x))))
|#

(define optimizable-op?
  (lambda (x)
    (not (number? x))))

(define not-optimizable-op?
  (lambda (x)
    (not (optimizable-op? x))))

#| .:: BACKEND ::. ______________________________________________________________________________________________________________________________________________________|#

(define get-sub-name (lambda (sub) (car sub)))
(define get-sub-value (lambda (sub) (cdr sub)))
(define initial-instance-count 1)
(define enough-to-optimize (+ initial-instance-count 1))
(define make-sub (lambda (expr) (cons expr 1)))
(define count-sub (lambda (sub) (cons (get-sub-name sub) (+ 1 (get-sub-value sub)))))

(define modify-subs
  (lambda (subs expr)
    (if (not-optimizable-op? (car expr))
        subs
        (cond ((null? subs) `(,(make-sub expr)))
              ((equal? (get-sub-name (car subs)) expr) (cons (count-sub (car subs)) (cdr subs)))
              (else (cons (car subs) (modify-subs (cdr subs) expr)))))))

(define expand-subs
  (lambda (subs e)
    (cond ((null? e) subs)
          ((not-list? e) subs)
          ((list? e)
           (let ((subs (modify-subs subs e)))
             (fold-left
              expand-subs ; add e as paren
              subs
              e))))))

#|(define expand-subs
  (lambda (subs e)
    (cond ((null? e) subs)
          ((not-list? e) subs)
          (else (modify-subs subs e)))))|#

(define clean-small-subs
  (lambda (subs)
    (cond ((null? subs) subs)
          ((>= (get-sub-value (car subs)) enough-to-optimize) (cons (car subs) (clean-small-subs (cdr subs))))
          (else (clean-small-subs (cdr subs))))))

(define is-sub-necessary?
  (lambda (sub parent)
    (display-green sub)
    (display-red parent)
    (> (get-sub-value sub) (get-sub-value parent))))

(define remove-unnecessary-subs
  (lambda (subs)
    (if (< (length subs) 2)
        subs
        (let* ((first-sub (car subs))
               (second-sub (cadr subs))
               (rest-subs (cddr subs)))
          
          (if (is-sub-necessary? second-sub first-sub)
              `(,first-sub ,@(remove-unnecessary-subs (cons second-sub rest-subs)))
              (remove-unnecessary-subs (cons first-sub rest-subs)))))))
          


(define create-optimizable-subs
  (lambda (expr)
    (let ((subs (expand-subs '() expr)))
      (clean-small-subs (remove-unnecessary-subs subs)))))

(define generate-let-sub
  (lambda (sub)
    `(,(gensym) ,(get-sub-name sub))))

(define rename-subs
  (lambda (subs)
    (if (null? subs)
        subs
        (map generate-let-sub subs))))

(define reverse-list
  (lambda (s)
    (if (null? s) s (append (reverse-list (cdr s)) `(,(car s))))))

(define create-let*-body
  (lambda (expr)
    (rename-subs (create-optimizable-subs expr))))

(define get-rename car)
(define get-expr cadr)
(define apply-sub-to-expr
  (lambda (sub)
    (letrec ((func (lambda (expr)
                     (cond ((null? expr) expr)
                           ((not-list? expr) expr)
                           ((equal? expr (get-expr sub)) (get-rename sub))
                           (else (map func expr))
                           ))))
      func)))

(define my-apply (lambda (x f) (f x)))
(define apply-all-subs-to-expr
  (lambda (subs expr)
    (let ((appliers (map apply-sub-to-expr subs)))
      (fold-left my-apply expr appliers))))

(define apply-sub-on-itself
  (lambda (subs)
    (let ((current-sub (car subs))
          (rest-subs (cdr subs)))
      (if (null? rest-subs)
          subs
          (cons (apply-all-subs-to-expr rest-subs current-sub)
                (apply-sub-on-itself rest-subs))
          ))))

#| .:: CSE INTERFACE ::. ______________________________________________________________________________________________________________________________________________________|#

(define cse
  (lambda (expr)
    (let ((let*-body (create-let*-body expr)))
      (if (null? let*-body)
          expr
          (let ((final-let*-body (reverse-list (apply-sub-on-itself let*-body)))
                (let-op (if (equal? 1 (length let*-body)) 'let 'let*)))
            `(,let-op ,final-let*-body ,(apply-all-subs-to-expr let*-body expr)))
          ))))







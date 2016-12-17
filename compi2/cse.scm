(load "pattern-matcher.scm")

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
    (and (not (number? x))
         (not (equal? 'quote x)))))

(define not-optimizable-op?
  (lambda (x)
    (not (optimizable-op? x))))

#| .:: BACKEND ::. ______________________________________________________________________________________________________________________________________________________|#
(define expr-root 'root!)
(define get-sub-name (lambda (sub) (car sub)))
(define get-sub-value (lambda (sub) (cadr sub)))
(define get-sub-parents (lambda (sub) (caddr sub)))
(define first-parent car)
(define rest-parents cdr)
(define initial-instance-count 1)
(define enough-to-optimize (+ initial-instance-count 1))
(define make-sub (lambda (expr parent) (list expr 1 `(,parent))))
(define add-sub-parent
  (lambda (sub parent)
    (let ((sub-parents (get-sub-parents sub)))
      (if (member parent sub-parents)
          sub-parents
          `(,parent ,@sub-parents)))))

(define count-sub
  (lambda (sub parent)
    (list (get-sub-name sub)
          (+ 1 (get-sub-value sub))
          (add-sub-parent sub parent))))

(define modify-subs
  (lambda (subs expr parent)
    (cond ((not-list? expr) subs)
          ((not-optimizable-op? (car expr)) subs)
          ((null? subs) `(,(make-sub expr parent)))
          ((equal? (get-sub-name (car subs)) expr) (cons (count-sub (car subs) parent) (cdr subs)))
          (else (cons (car subs) (modify-subs (cdr subs) expr parent))))))

(define not-reserved-word? (lambda (foo) #t))

(define <application-rule>
  (lambda (parent)
  (lambda (subs e)
    ((pattern-rule
      `(,(? 'foo not-reserved-word?) . ,(? 'args))
      (lambda (foo . args)
        (let* ((subs (modify-subs subs e parent))
               (parent `(,foo ,@(car args)))
               (subs (modify-subs subs foo parent)))
          (fold-left
           (lambda (acc e) ((<application-rule> parent) acc e))
           subs
           (car args))))) e (lambda () subs)))))

;;(define tag-parse
;;  (let ((run
;;         (compose-patterns
;;          <application-rule>
;;          )
;;         ))
;;    (lambda (sexpr)
;;      (run sexpr *error-continuation*))))

;;(define parse tag-parse)

(define expand-subs
  (letrec
      ((expand-subs-with-parent
        (lambda (parent)
          (lambda (subs e)
            (cond ((null? e) subs)
                  ((not-list? e) subs)
                                        ;((equal? 'lambda (car e)) subs)
                  ((equal? 'quote (car e)) subs)
                  ((list? e)
                   (let ((subs (fold-left (expand-subs-with-parent e) subs e)))
                     (modify-subs subs e parent))))))))
    (<application-rule> expr-root)))
#|
(define not-reserved-word? (lambda (foo) #t))
(define expand-subs
  (letrec
      ((expand-subs-with-parent
        (lambda (parent)
          (lambda (subs e)
            (cond ((null? e) subs)
                  ((not-list? e) subs)
                  ((list? e)
                   (let ((subs (modify-subs subs e parent)))
                     (fold-left
                      (expand-subs-with-parent e) ; add parent! e
                      subs
                      e)))))))
       
       (<application-rule>
        (lambda (subs)
          (pattern-rule
           `(,(? 'foo not-reserved-word?) . ,(? 'args))
           (lambda (foo . args)
             (let* ((parse (expand-subs-with-parent `(,foo ,args)))
                    (parent `(,foo ,args))
                    (subs (if (not (list? foo)) subs (modify-subs subs foo parent))))
               (begin (display-colored-BIG (car args))
               (fold-left 
                (lambda(acc e) (parse acc e))
                subs
                (car args)))))))))|#

(define clean-small-subs
  (lambda (subs)
    (cond ((null? subs) subs)
          ((>= (get-sub-value (car subs)) enough-to-optimize) (cons (car subs) (clean-small-subs (cdr subs))))
          (else (clean-small-subs (cdr subs))))))

(define find-sub-by-name
  (lambda (subs sub-name)
    (cond ((null? subs) (format "couldn't find sub named: ~s" sub-name))
          ((equal? (get-sub-name (car subs)) sub-name) (car subs))
          (else (find-sub-by-name (cdr subs) sub-name)))))

(define ^is-sub-necessary?
  #|(lambda (sub)
    (display-green sub)
    (display-red parent)
    (> (get-sub-value sub) (get-sub-value parent))))|#
  (lambda (subs)
    (lambda (sub)
      (if (< (get-sub-value sub) enough-to-optimize)
          #f
          (cond
           ; expression has several parents
           ((> (length (get-sub-parents sub)) 1) #t)
           
           ; specific case: parent is root and the expression has several instances
           ((equal? expr-root (first-parent (get-sub-parents sub))) #t)

           ; similar to the previous case: expression has several instances inside it's parent expression
           ((> (get-sub-value sub) (get-sub-value (find-sub-by-name subs (first-parent (get-sub-parents sub))))) #t)
           
           ; in any other case the substitution is unnecessary
           (else #f)
           )))))

(define remove-unnecessary-subs
  (lambda (subs)
    (let ((necessary? (^is-sub-necessary? subs)))
      (fold-left (lambda (acc sub)
                   (if (necessary? sub) (append acc `(,sub)) acc)) '() subs))))

    #|(if (< (length subs) 2)
        subs
        (let* ((first-sub (car subs))
               (second-sub (cadr subs))
               (rest-subs (cddr subs)))
          
          (if (^is-sub-necessary? second-sub first-sub)
              `(,first-sub ,@(remove-unnecessary-subs (cons second-sub rest-subs)))
              (remove-unnecessary-subs (cons first-sub rest-subs)))))))|#



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
          (let ((final-let*-body (apply-sub-on-itself let*-body))
                (let-op (if (equal? 1 (length let*-body)) 'let 'let*)))
            `(,let-op ,final-let*-body ,(apply-all-subs-to-expr let*-body expr)))
          ))))







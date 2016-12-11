(define <define-rule>
  (pattern-rule
   `(define ,(? 'var) ,(? 'val))
   (lambda (var val) `(def ,(parse var) ,(parse val)))))

(define <define-mit-rule>
  (pattern-rule
   `(define ,(? 'f-args) ,(? 'body))
   (lambda (f-args body)
   (let ((f (car f-args)) (args (cdr f-args)))
     `(def f ,(parse `(lambda ,args ,body)))))))

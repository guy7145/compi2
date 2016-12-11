(define <define-rule>
  (pattern-rule
   `(define ,(? 'var) ,(? 'val))
   (lambda (var val) `(def ,(parse var) ,(parse val)))))

(define <define-mit-rule>
  (pattern-rule
   `(define (,(? 'f) . ,(? 'args)) ,(? 'body))
   (lambda ((f . args) body)
     `(def ,(parse f) ,(parse `(lambda ,args ,body))))))

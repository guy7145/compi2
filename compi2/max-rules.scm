(define <define-rule>
  (pattern-rule
   `(define ,(? 'var) ,(? 'val))
   (lambda (var val) `(def ,(parse var) ,(parse val)))))

(define <define-rule>
  (pattern-rule
   `(define ,(? 'var) ,(? 'val) . ,(? 'val-rest))
   (lambda (var val . val-rest)
     (if (null? val-rest)
	 `(def ,(parse var) ,(parse val))
	 `(def ,(parse var) ,@(map parse (list val (car val-rest))))))))

(define <define-mit-rule>
  (pattern-rule
   `(define ,(? 'f list? (lambda (lst) (> (list-length lst) 0))) ,(? 'body))
   (lambda (f body)
     `(def ,(parse (car f)),(parse `(lambda ,(cdr f) ,body))))))

(define <assignment-rule>
  (pattern-rule
   `(set! ,(? 'var var?) ,(? 'val))
   (lambda (var val)
     `(set ,(parse var) ,(parse val)))))

(define <application-rule>
  (pattern-rule
   `(,(? 'foo) . ,(? 'args))
   (lambda (foo . args)
     `(applic ,(parse foo) (,@(map parse (car args)))))))

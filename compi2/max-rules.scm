(define <let-no-args-rule>
  (pattern-rule
   `(let () ,(? 'body) . ,(? 'rest))
   (lambda (body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       (parse `((lambda () ,body)))))))

(define <let-rule>
  (lambda (e fail-cont)
    ((pattern-rule
   `(let (,(? 'args) . ,(? 'rest)) ,(? 'body) . ,(? 'rest))
   (lambda (args-head rest body . rest-body)
     (let ((args (if (null? rest)
		     `(,args-head)
		     `(,args-head ,@rest)))
	   (body (if (null? rest-body)
		     `(,body)
		     `(,body ,@(car rest-body)))))
       (if (list-is-duplicative? (map car args))
	   (fail-cont)
	   (parse `((lambda ,(map car args) ,@body) ,@(map cadr args))))))) e fail-cont)))

(define <let*-no-args-rule>
  (pattern-rule
   `(let* () ,(? 'body) . ,(? 'rest))
   (lambda (body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       (parse `((lambda () ,body)))))))

(define <let*-rule>
  (pattern-rule
   `(let* (,(? 'args) . ,(? 'rest)) ,(? 'body) . ,(? 'rest))
   (lambda (args-head rest head-body . rest-body)
     (letrec ((args (if (null? rest)
			`(,args-head)
			`(,args-head ,@rest)))
	      (body (if (null? rest-body)
			`(,head-body)
			`(,head-body ,@(car rest-body))))
	      (let*->encapsulated-lambdas (lambda (args body)
			       (if (null? args)
				   body
				   `(((lambda (,(caar args))
					,@(let*->encapsulated-lambdas (cdr args) body))
				      ,@(cdar args)))))))
       (parse (car (let*->encapsulated-lambdas args body)))))))

(define <letrec-no-args-rule>
  (pattern-rule
   `(letrec () ,(? 'body) . ,(? 'rest))
   (lambda (body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       (parse `((lambda () ((lambda () ,body)))))))))

(define <letrec-rule>
  (lambda (e fail-cont)
    ((pattern-rule
   `(letrec (,(? 'args) . ,(? 'rest)) ,(? 'body) . ,(? 'rest))
   (lambda (args-head rest head-body . rest-body)
     (letrec ((args (if (null? rest)
			`(,args-head)
			`(,args-head ,@rest)))
	      (body (if (null? rest-body)
			`(,head-body)
			`(,head-body ,@(car rest-body))))
	      (args->set (lambda (lst)
			   (if (null? (cdr lst))
			       `((set! ,(caar lst) ,@(cdar lst)))
			       `((set! ,(caar lst) ,@(cdar lst)) ,@(args->set (cdr lst)))))))
       (if (list-is-duplicative? (map car args))
	   (fail-cont)
	   (parse `((lambda ,(map car args) ,@(append (args->set args) `(((lambda () ,@body))))) ,@(map (lambda (x) #f) args))))))) e fail-cont)))

(define <let-rules>
  (compose-patterns
   <let-no-args-rule>
   <let-rule>
   <let*-no-args-rule>
   <let*-rule>
   <letrec-no-args-rule>
   <letrec-rule>
   ))

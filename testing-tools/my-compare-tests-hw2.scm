; Change to your own location
(load "compiler.scm")
(define my-parse-func parse)

(load "tagparser.so")
(define staff-parse-func parse)

(define try-catch
  (lambda (try-thunk catch-thunk)
    (guard (c (else (catch-thunk)))
     (try-thunk))))

(define testVSstaff
	(lambda (input)
		(let* ((my-res (try-catch (lambda () (my-parse-func input)) (lambda () "Exception thrown")))
		      (staff-res (try-catch (lambda () (staff-parse-func input)) (lambda () (display "\033[1;34m !!Negative Test!! \033[0m ") "Exception thrown"))))
			(display (format "~s:" input))
			;(display my-res)
			(cond ((equal? my-res staff-res)
				(display (format "\033[1;32m Success! ☺ \033[0m \n")) #t)
				(else 
				(display (format "\033[1;31m Failed! ☹\033[0m , Expected: ~s, Actual: ~s \n" staff-res my-res)) #f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "=============")
	(newline)
	(let ((results (map testVSstaff lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)	
		(display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)		
		(else
		(display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n"))
		(else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n")))
		(newline))
))

(define MyTests
  (list
    ''()
))

(runAllTests
  (list
      (cons "MyTests" MyTests)
))



















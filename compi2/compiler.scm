(load "pc.scm")

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

;; ***********************************************
(define <delayed-infix-comment>
  (new (*delayed (lambda () <InfixComment>) ) done))
;; ***********************************************


(define <comment>
  (disj <line-comment>
        <delayed-infix-comment>
	<sexpr-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

;;;;;;;; END OF SKIPPED WHITE SPACE ;;;;;;;;;;;;;;

(define <Boolean>
  (new

   (*parser (char-ci #\#))
   (*parser (char-ci #\f))
   (*parser (char-ci #\t))

   (*disj 2)
   (*caten 2)

   (*pack-with
    (lambda (_ t)
      (eq? t #\t)))

   done))

(define <CharPrefix>
  (new
   
   (*parser (char #\#))
   (*parser (char #\\))
   (*caten 2)
   
   done))

(define <VisibleSimpleChar>
  (new

   (*parser (const
	     (lambda (ch)
	       (char<? #\space ch))))

   done))

(define <NamedChar>
  (new

   (*parser (word-ci "lambda"))
   (*parser (word-ci "newline"))
   (*parser (word-ci "nul"))
   (*parser (word-ci "page"))
   (*parser (word-ci "return"))
   (*parser (word-ci "space"))
   (*parser (word-ci "tab"))

   (*disj 7)

   (*pack (lambda (x)
	    (let ((x (list->string x)))
	      (cond ((string-ci=? x "lambda")  #\x3bb)
		    ((string-ci=? x "newline") #\newline)
		    ((string-ci=? x "nul")     #\nul)
		    ((string-ci=? x "page")    #\page)
		    ((string-ci=? x "return")  #\return)
		    ((string-ci=? x "space")   #\space)
		    ((string-ci=? x "tab")     #\tab)
		    ))))

   done))

(define <HexChar>
  (let ((zero (char->integer #\0))
	(lc-a (char->integer #\a))
	(uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
	 (*pack
	  (lambda (ch)
	    (- (char->integer ch) zero)))

	 (*parser (range #\a #\f))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) lc-a))))

	 (*parser (range #\A #\F))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) uc-a))))

	 (*disj 3)
	 done)))

(define <HexUnicodeChar>
  (letrec ((hex->int (lambda (lst c)
			 (if (null? lst)
			     c
			     (hex->int (cdr lst) (+ (car lst) (* 16 c))))
			 )))
    (new
     (*parser (char #\x))
     (*parser <HexChar>) *plus
     (*caten 2)

     (*pack-with
      (lambda (x hc)
	(integer->char (hex->int `(,@hc) 0))))
     
     done)))

(define <Char>
  (new
   (*parser <CharPrefix>)
   (*parser <NamedChar>)
   (*parser <HexUnicodeChar>)
   (*parser <VisibleSimpleChar>)

   (*disj 3)
   (*caten 2)

   (*pack-with
    (lambda (p cs)
      cs;(string->symbol (string cs))
      ))

   done))

(define <Natural>
  (let ((char->int (lambda (c)
		     (- (char->integer c)
			(char->integer #\0)))))
    (new
     (*parser (range #\0 #\9)) *plus
     (*pack
      (lambda (lst)
	(letrec ((lst->int (lambda (lst c)
			     (if (null? lst)
				 c
				 (lst->int (cdr lst) (+ (* c 10) (char->int (car lst))))))))
	  (lst->int lst 0))))
     done)))

(define <Integer>
  (new
   (*parser (char #\+))
   (*parser (char #\-))
   (*disj 2) *maybe
   (*parser <Natural>)
   (*caten 2)
   (*pack-with
    (lambda (s n)
      (if (and (car s) (equal? (cadr s) #\-))
	  (- n)
	  n)))
   done))

(define <Fraction>
  (new
   (*parser <Integer>)
   (*parser (char #\/))
   (*parser <Natural>)
   (*caten 3)
   (*pack-with
    (lambda (int _ nat)
      (/ int nat)))
   done))

(define <Number>
  (new
   (*parser <Fraction>)
   (*parser <Integer>)
   (*disj 2)
   (*delayed (lambda () <SymbolChar>))
   (*parser (range #\0 #\9))
   *diff
   *not-followed-by
   done))

(define <StringVisibleChar>
  (new
   (*parser (const
	      (lambda (ch)
	       (char<=? #\space ch))))
   done))

(define <StringMetaChar>
  (new
   (*parser (word-ci "\\\\"))
   (*parser (word-ci "\\\""))
   (*parser (word-ci "\\t"))
   (*parser (word-ci "\\f"))
   (*parser (word-ci "\\n"))
   (*parser (word-ci "\\r"))
   (*disj 6)
   (*pack (lambda (x)
	    (let ((c (cadr x)))
	      (cond ((char-ci=? c #\t) #\tab)
		    ((char-ci=? c #\f) #\x0c)
		    ((char-ci=? c #\n) #\newline)
		    ((char-ci=? c #\r) #\return)
		    (else c)))))
   done))

(define <StringHexChar>
  (letrec ((hex->int (lambda (lst c)
			 (if (null? lst)
			     c
			     (hex->int (cdr lst) (+ (car lst) (* 16 c))))
			 )))
    (new
     (*parser (word-ci "\\x"))
     (*parser <HexChar>) *star
     (*parser (char #\;))
     (*caten 3)
     (*pack-with
      (lambda (x lst _)
	(integer->char (hex->int lst 0))))
     done)))

(define <StringChar>
  (new
   (*parser <StringHexChar>)
   (*parser <StringMetaChar>)
   (*parser <StringVisibleChar>)
   (*disj 3)
   done))

(define <String>
  (new
   (*parser (char #\"))
   (*parser <StringChar>)
   (*parser (char #\"))
   *diff
   *star
   (*parser (char #\"))
   (*caten 3)
   (*pack-with
    (lambda (< lst >)
      (list->string lst)))
   done))

(define <SymbolChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range #\a #\z))
   (*parser (range #\A #\Z))
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\-))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\+))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*parser (char #\/))
   (*disj 15)
   (*pack char-downcase)
   done))

(define <Symbol>
  (new
   (*parser <SymbolChar>) *plus
   (*pack (lambda (lst)
	    (string->symbol
	     (list->string lst))))
   done))

(define <ProperList>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>)) *star
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (_ lst __) lst))
   done))

(define <ImproperList>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>)) *plus
   (*parser (char #\.))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\)))
   (*caten 5)
   (*pack-with
    (lambda (_ lst __ itm ___)
      (append lst itm)))
   done))

(define <Vector>
  (new
   (*parser (word "#("))
   (*delayed (lambda () <sexpr>)) *star
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (_ lst __)
      (list->vector lst)))
   done))

(define <Quoted>
  (new
   (*parser (char #\'))
   (*delayed (lambda () <sexpr>))
   (*caten 2)   
   (*pack-with
    (lambda (_ exp)
      (list 'quote exp)))
   done))

(define <QuasiQuoted>
  (new
   (*parser (char #\`))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'quasiquote exp)))
   done))

(define <Unquoted>
  (new
   (*parser (char #\,))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'unquote exp)))
   done))

(define <UnquoteAndSpliced>
  (new
   (*parser (word ",@"))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'unquote-splicing exp)))
   done))

; ************************************************************************************************************************************************************************************


(define *<InfixExpressionDelayed>
  (*delayed (lambda () <InfixExpression>)))

(define <InfixPrefixExtensionPrefix>
  (^<skipped*>
   (new
    (*parser (char #\#))
    (*parser (char #\#))
    (*caten 2)
    (*parser (char #\#))
    (*parser (char #\%))
    (*caten 2)
    (*disj 2)
    (*pack-with
     (lambda (x1 x2) (list->string `(,x1 (,@x2)))))
    done)))

(define <InfixExtension>
  (new
   (*parser <InfixPrefixExtensionPrefix>)
    *<InfixExpressionDelayed>
   (*caten 2)
   (*pack-with
    (lambda (_ expr) expr))
   done))

(define <PowerSymbol>
  (new
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\*))
   (*caten 2)
   (*pack-with
    (lambda (x1 x2) (list->string `(,x1 (,@x2)))))
   (*disj 2)
   done))

;; Auxilary Lambdas
;; 

; for backward compatibility (;
(define build-op-formula
  (lambda (<op>)
    (lambda (l r)
      `(,<op> ,l ,r))))

(define <operator-2ops>
  (lambda (<folding> <formula-builder>)
    (lambda (<op-char-parser> <fold-operation> <next-parser>)
      (new
       (*parser <next-parser>)
       (*parser (^<skipped*> <op-char-parser>))
       (*caten 2)
       (*pack-with
        (lambda (expr op-char) expr))
       *plus
       (*parser <next-parser>)
       (*caten 2)
       (*pack-with
        (let ((formula-builder (<formula-builder> <fold-operation>)))
          (lambda (list element)
            (<folding> formula-builder element list))))
       (*parser <next-parser>)
       (*disj 2)
       done))))

(define <operator-2ops-right-lr> (<operator-2ops> fold-right build-op-formula))

;; Level
;;
(define <InfixSymbol>
  (new
   (*parser <SymbolChar>)
   (*parser (char #\+))
   (*parser (char #\-))
   (*parser (char #\*))
   (*parser (char #\/))
   (*parser <PowerSymbol>)
   (*disj 5)
   *diff
   *plus
   (*pack (lambda (lst)
	    (string->symbol
	     (list->string lst))))
   done))

(define <InfixSexprEscape>
  (new
   (*parser <InfixPrefixExtensionPrefix>)
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (prefix sexpr) sexpr))
   done))

(define <infix-number> (not-followed-by (disj <Fraction> <Integer>) <InfixSymbol>))

(define <InfixParen>
  (new
   (*parser (char #\())
    *<InfixExpressionDelayed>
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (lp expr rp) expr))
   done))

(define <Primitives-and-such> (disj <infix-number> <InfixSexprEscape> <InfixSymbol>))
(define <Level-Paren-&-Friends> (disj <InfixParen> <Primitives-and-such>))

;; Level
;;
(define <InfixArgList>
  (^<skipped*> (new
                ;; args ;;
                ; first arg
                *<InfixExpressionDelayed>
                ; rest args (some or none)
                (*parser (char #\,))
                *<InfixExpressionDelayed>
                (*caten 2)
                (*pack-with (lambda (_ expr) expr))
                *star
                ; catenate
                (*caten 2)
                (*pack-with
                 (lambda (first-arg rest-args) `(,first-arg ,@rest-args)))
                ;; or no args ;;
                (*parser <epsilon>)
                ;; args | no args ;;
                (*disj 2)
                done)))

(define <InfixFuncall>
  (new
   (*parser (^<skipped*> <Level-Paren-&-Friends>))
   (*parser (char #\())
   (*parser <InfixArgList>)
   (*parser (char #\)))
   (*caten 4)
   (*pack-with
    (lambda (function lp args rp)
      `(,function ,@args)))
   done))

(define <InfixArrayGet> (^<skipped*> 
  (new
   (*parser (^<skipped*> <Level-Paren-&-Friends>))
   (*parser (char #\[))
   (*delayed (lambda () <InfixArrayGet>))
   (*delayed (lambda () <InfixExpression>))
   (*disj 2)
   (*parser (char #\]))
   (*caten 3) 
   (*pack-with 
    (lambda (lp expr rp) expr))
   *plus
   (*caten 2)
   (*pack-with
    (let ((expr-builder (build-op-formula 'vector-ref)))
    (lambda (arr is)
      (fold-left expr-builder arr is))))
   done)))
 
(define <Level-ArrFun> (disj <InfixArrayGet> <InfixFuncall> <Level-Paren-&-Friends>))

;; Level
;;

(define <InfixNeg>
  (new (*parser (let ((<next-level> (new (*delayed (lambda () <InfixNeg>))
                                (*parser <Level-ArrFun>)
                                (*disj 2)
                                done)))
         (new
              ;; (without parentesis) 
              ;; no spaces between '-' and the following expression 
              (*parser (char #\-))
              (*parser <next-level>)
              (*caten 2)
              (*pack-with
               (lambda (minus element)
                 (cond ((number? element) (- element))
                       (else `(- ,element)))))
              ;; (with parentesis) 
              ;; spaces between '-' and the following expression
              (*parser (^<skipped*> (char #\-)))
              (*parser <next-level>)
              (*caten 2)
              (*pack-with
               (lambda (minus element) `(- ,element)))
              (*disj 2)
              done)))
       (*parser <Level-ArrFun>)
       (*disj 2)
       done))

;; Level
;; 
(define <InfixPow> (<operator-2ops-right-lr> <PowerSymbol> (string->symbol "expt") <InfixNeg>))

;; Level
;; 
(define build-op-formula-op1-op2
    (lambda (element op-expr-pair)
      (let ((op (car op-expr-pair)) (expr (cdr op-expr-pair)))
      `(,op ,element ,@expr))))

(define <level-op1-op2>
  (lambda (<next-level> op-char1 op-char2 op-str1 op-str2)
    (new (*parser <next-level>)
         (*parser (^<skipped*> (char op-char1)))
         (*parser (^<skipped*> (char op-char2)))
         (*disj 2)
         (*pack (lambda (op) (cond ((char=? op op-char1) (string->symbol op-str1))
                                   (else (string->symbol op-str2)))))
         (*parser <next-level>)
         (*caten 2)
         *plus
         (*caten 2)
         (*pack-with
          (lambda (element list)
            (fold-left build-op-formula-op1-op2 element list)))
         (*parser <next-level>)
         (*disj 2)
         done)))

;; Level
;; 
(define <Level-MulDiv> (<level-op1-op2> <InfixPow> #\/ #\* "/" "*"))

;; Level
;;
(define <Level-AddSub> (<level-op1-op2> <Level-MulDiv> #\+ #\- "+" "-")) 

;; Entry Point
;; 
(define <InfixExpression> (^<skipped*> <Level-AddSub>))

;; Comments
;; 
(define <InfixComment> 
  (new (*parser (word "#;"))
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with (lambda (_1 _2) (void)))
       done))

; ************************************************************************************************************************************************************************************

(define <sexpr>
  (^<skipped*>
   (disj <Boolean>
         <InfixComment>
         <InfixExtension>
         <Char>
         <String>
         <Number>
         <Symbol>
         <ProperList>
         <ImproperList>
         <Vector>
         <Quoted>
         <QuasiQuoted>
         <Unquoted>
         <UnquoteAndSpliced>
         )))

;*************************************************************************************************

(load "pattern-matcher.scm")

;; __________________________________________________________________________________________
;; __predicats and constants ________________________________________________________________
;; __________________________________________________________________________________________

(define *void-object* (void))

(define *reserved-words*
  '(and begin cond define do else if lambda let let* letrec or quasiquote unquote unquote-splicing quote set!))

(define *error-text* "error! (change to 'ERROR'...)") ;"ERROR!")

(define *error-continuation*
  (lambda () *error-text*))

(define reserved-word?
  (lambda (x)
    (member x *reserved-words*)))

(define var?
  (lambda (x)
    (and (symbol? x)
         (not (reserved-word? x)))))


(define simple-const?
  (let ((preds (list boolean? char? number? string?)))
    (lambda (e) 
      (ormap (lambda (p?) (p? e)) preds))))


;; __________________________________________________________________________________________
;; __rules __________________________________________________________________________________ 
;; __________________________________________________________________________________________

(define <const-rule>
  (pattern-rule
   (? 'c simple-const?)
   (lambda (c) `(const ,c))))

(define <quote-rule>
  (pattern-rule
   `(quote ,(? 'c))
   (lambda (c) `(const ,c))))

(define <var-rule>
  (pattern-rule
   (? 'var var?)
   (lambda (var) `(var ,var))))

;(load "guy-rules.scm")
(load "../compi2/max-rules.scm")

(define tag-parse
  (let ((run
         (compose-patterns
          <const-rule>
          <quote-rule>
          <var-rule>
	  <define-rule>
;	  <define-mit-rule>
          )
         ))
    (lambda (sexpr)
      (run sexpr *error-continuation*))))

(define parse tag-parse)














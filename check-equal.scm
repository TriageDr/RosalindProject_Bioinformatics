;;; Simple Scheme test macro that is compatible with Racket's
;;; 'check-equal?'  syntax (but without supporting all of Racket's
;;; fancy testing infrastructure).
;;;
;;; This macro is adopted from an old test macro originally written by
;;; Oleg Kiselyov for Kanren, and modified by Dan Friedman and Will
;;; Byrd over the years.

(define-syntax check-equal?
  (syntax-rules ()
    ((_ name tested-expression expected-result)
     (let* ((expected expected-result)
            (produced tested-expression))
       (if (equal? expected produced)
	   (printf "Test ~s passed\n" name)
           (printf "\ncheck-equal? failed for expression:\n~a\n Expected value:\n~a\n Computed value:\n~a\n\n"
                   'tested-expression expected produced)))))) 

;;~% is same \n
;;_ = check-equal?
;; becuase we've define-syntax check-equal? we can use _

;; (expand '(check-equal? (+ 2 3) (* 5 2)
;; --> gives gensymbols, which are notations of the variable
;; use (print-gensym #f) to turn of gensymbols 

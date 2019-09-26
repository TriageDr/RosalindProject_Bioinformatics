(load "../common/pmatch.scm")
(load "../common/check-equal.scm")
(load "../common/prelude.scm")

#|

  Problem BA1a 
  _______________________________________________________________________
 
  Define a function pattern-count that takes an argument string (or list)
  of nucleotides and returns the number of repeats for a given pattern 
  found at the bottom of the rosalind_ba1a.txt file.  
   
  Sample input: 
  GCGCG   sequence string/list
  GCG     pattern

  Sample output: 
  2       # of pattern repeats in the string

  http://rosalind.info/problems/ba1a/
  _______________________________________________________________________

|#

;; define input file 
(define in (open-input-file "../001_BA1a_HiddenMessageProblem/rosalind_ba1a.txt"))

;; function reads in character list from input file 
(define port->list/read-char
  (lambda (port)
    (let ((next-char (read-char port)))
      (if (eof-object? next-char)
	  '()
	  (cons next-char
		(port->list/read-char port))))))

;; raw character list of input file
(define ba1a-r
  (port->list/read-char in))

;;close input port
(close-input-port in)

;; function extracts pattern from txt file input
(define pattern-finder
  (lambda (ls els)
    (cond
      ((eq? #\newline (car ls))
       (cons (reverse els) (cdr ls)))
      (else
       (pattern-finder (cdr ls)
		       (cons (car ls) els))))))

;; variable holds pattern to be matched in the sequence list 
;; removes #\newline character from input list
(define ba1a-pattern
  (remove-all #\newline (cdr (pattern-finder ba1a-r '()))))

;; variable holds the sequence list
(define ba1a-sequence
  (car (pattern-finder ba1a-r '())))


#|  

  Helper Function: prefix?   
  _______________________________________________________________________

  The goal is to have pattern-count take a single argument string, 
  so we must use a helper function prefix? to hold the value of the pattern 
  we wish to match against, as well as compare that pattern 
  character-by-character recursively against the argument list. 
  
  prefix? input:    '(<pattern-character-list>) '(<sequence-character-list)
  bound-variables:  '(ba1a-pattern)             '(ba1a-sequence)  
  variable names:   ls1                         ls2

  prefix? output:   #t or #f

  Case Analysis Tests: prefix?
  _______________________________________________________________________
 
  Case #1: both ls1 ls2 are null 
  
  ((and (null? ls1)
	    (null? ls2))
       #t)

  Case #2: ls1 is not null, ls2 is null
  
  ((and (not (null? ls1))
	    (null? ls2))
       #f)

  Case #3: ls1 is null, ls2 is not null

  ((and (null? ls1)
        (not (null? ls2)))
    #t)

  Case #4: test first char of pattern against sequence, 
           recur until Case #1 or Case #3 are #t 
           else, return #f.
  
  ((eq? (car ls1) (car ls2))
       (prefix? (cdr ls1) (cdr ls2))) 
  (else 
   #f))))


  Using the macro check-equal? 
  to test Cases #1-4
  _______________________________________________________________________

|# 


#| ---- Helper Function: prefix? ---- |# 

(define prefix?
  (lambda (ls1 ls2)
    (cond
      ((and (null? ls1)
	    (null? ls2))
       #t)
      ((and (not (null? ls1))
	    (null? ls2))
       #f)
      ((and (null? ls1)
	    (not (null? ls2)))
       #t)
      ((eq? (car ls1) (car ls2))
       (prefix? (cdr ls1) (cdr ls2)))
      (else
       #f))))

;; Case #1 Test
(check-equal? "Case #1 prefix?" (prefix? '() '()) #t)

;; Case #2 Test
(check-equal? "Case #2 prefix?" (prefix? (string->list "ACGAGCTAC") '()) #f)

;; Case #3 Test
(check-equal? "Case #3 prefix?" (prefix? '() (string->list "ATTTAAGCGC")) #t)

;; Case #4 Test: eq/recursive clause
(check-equal? "Case #4 prefix?" (prefix? (string->list "ACGAGCTAC") (string->list "ACGAGCTAC")) #t)

;; Case #5 Test else clause
(check-equal? "Case #5 prefix?" (prefix? (string->list "ACGAGCTAC") (string->list "ACGGGGGCTA")) #f)


#|
 
  Function: pattern-count   
  _______________________________________________________________________

  pattern-count will take the argument list and 
  _______________________________________________________________________
  

  Notes on Scheme patterns 
  _______________________________________________________________________
  
  letrec pattern: 
  (letrec ((var (expr))) body) 
  
  Using the letrec pattern, we can bind the local variable (var) prefix? 
  to the lambda expression (expr) of the helper function prefix? 
  defined above. 
  
  letrec Pros:
  1) We can use this to prevent prefix? from being re-evaluated during 
  every recursive call to pattern-count. 

  2) 

  _______________________________________________________________________


Final Answer: 21 repeats of pattern: '(#\A #\C #\G #\A #\G #\C #\T #\A #\C)  
|#


#| ---- Function: pattern-count ---- |# 

(define pattern-count
  (letrec ((prefix?
	    (lambda (ls1 ls2)
	      (cond
		((and (null? ls1)
		      (null? ls2))
		 #t)
		((and (not (null? ls1))
		      (null? ls2))
		 #f)
		((and (null? ls1)
		      (not (null? ls2)))
		 #t)
		((eq? (car ls1) (car ls2))
		 (prefix? (cdr ls1) (cdr ls2)))
		(else
		 #f)))))
    (lambda (ls)
      (cond
	((null? ls) 0)
	((prefix? '(#\A #\C #\G #\A #\G #\C #\T #\A #\C) ls)
	 (add1 (pattern-count (cdr ls))))
	(else
	 (pattern-count (cdr ls)))))))

#| ---- Function: pattern-count/pmatch ----

  _______________________________________________________________________

  Using the Scheme pattern-match program pmatch, create a function that 
  takes a list argument <ba1a-sequence> (ls). Enter the pattern 
  argument <ba1a-pattern> in the syntactic form pair (<pattern> . ,rest). 
  If a match occurs add1 and recur on the rest of ba1a-sequence. 
 
  The first clause of pmatch contains the pattern for the empty list ()
  and returns 0 if it matches against the empty list 
  
  The third clause of pmatch contains a pattern for all variable numbers
  which will be matched when the product of the (add1 recursions are 
  added to the final value of the empty list which is 0. 

  Using check-equal? once more we can confirm that our pattern-count 
  accurately captures the number of repeats of a pattern in the given 
  sequence 
  _______________________________________________________________________

  Other Functions: (expand '(define <function> ...)
                   (print-gensym #f) 

  expand will unravel pmatch into primitive forms, such as let expressions
  (print-gensym #f) will make this more readable as it removes the 
  full has-id from the print-out.

  (expand '(define pattern-count/pmatch
  (lambda (ls)
    (pmatch ls
      [()
       0]
      [(#\A #\C #\G #\A #\G #\C #\T #\A #\C . ,rest)
       (add1 (pattern-count/pmatch (cdr ls)))]
      [(,x . ,rest)
       (pattern-count/pmatch (cdr ls))]))))


  Final Answer: 21 repeats of pattern: '(#\A #\C #\G #\A #\G #\C #\T #\A #\C)  

|#

(define pattern-count/pmatch
  (lambda (ls)
    (pmatch ls
      [()
       0]
      [(#\A #\C #\G #\A #\G #\C #\T #\A #\C . ,rest)
       (add1 (pattern-count/pmatch (cdr ls)))]
      [(,x . ,rest)
       (pattern-count/pmatch (cdr ls))])))

;; Case #1 pattern-count vs pmatch
(check-equal? "Case #1 pattern-count vs pmatch" (pattern-count ba1a-sequence) (pattern-count/pmatch ba1a-sequence))

(pattern-count/pmatch ba1a-sequence)

(newline)
(printf "The pattern ~s has ~s repeats. \n"
	(list->string ba1a-pattern) (pattern-count ba1a-sequence))
#!eof

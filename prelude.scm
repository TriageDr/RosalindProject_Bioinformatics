#|
  -- Bioinformatic Algorithms Problem BA1a Prelude --

  The prelude file contains all helper functions used in 
  001_BA1a_Program.scm for import and cleaning of the 
  input data rosalind_ba1a.txt

  The original Rosalind problem can be found at 
  http://rosalind.info/problems/ba1a/  

|#


(define member?
  (lambda (a ls)
    (cond
      ((null? ls) #f)
      (else
       (or (member? a (cdr ls))
	   (eq? (car ls) a))))))

(define remove-all
  (lambda (a ls)
    (cond
      ((null? ls) '())
      ((eq? (car ls) a)
       (remove-all a (cdr ls)))
      (else
       (cons (car ls)
	     (remove-all a (cdr ls)))))))

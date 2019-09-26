(load "../common/pmatch.scm")
(load "../common/check-equal.scm")
(load "../common/prelude.scm")

#|
  Problem BA1b: Most Frequent Words 
  _______________________________________________________________________
 
  Define a function most-frequent-kmer that determines that most frequent
  kmers for a given length of a kmer. 

  most-frequent-kmer should take 1 argument (ls) which is the string 
  of nucleotides and return a list of the most frequent kmers that 
  correspond to the defined kmer length located at the bottom of 
  the rosalind_ba1b.txt file.  
   
  Sample input: 
  ACGTTGCATGTCGCATGATGCATGAGAGCT  sequence
  4                               kmer length

  Sample output: 
  CATG GCAT 

  Scheme/Racket output: 
  ("CATG "GCAT")

  source: 
  http://rosalind.info/problems/ba1b/
  _______________________________________________________________________

|#

;; define input file 
(define in (open-input-file "../001_BA1b_FrequentWordsProblem/rosalind_ba1b.txt"))

;; function reads in character list from input file 
(define port->list/read-char
  (lambda (port)
    (let ((next-char (read-char port)))
      (if (eof-object? next-char)
	  '()
	  (cons next-char
		(port->list/read-char port))))))

;; raw character list of input file
(define ba1b-r
  (port->list/read-char in))

;;close input port
(close-input-port in)

;; function extracts kmer-length from txt file input
(define kmer-length-finder
  (lambda (ls els)
    (cond
      ((eq? #\newline (car ls))
       (cons (reverse els) (cdr ls)))
      (else
       (kmer-length-finder (cdr ls)
		       (cons (car ls) els))))))

;; variable hold the kmer length 
;; remove-all function, from prelude folder,
;; removes #\newline character from input list

(define ba1b-kmer-length
  (list->string (remove-all #\newline (cdr (kmer-length-finder ba1b-r '())))))

;; variable holds the sequence list
(define ba1b-sequence
  (car (kmer-length-finder ba1b-r '())))

(define dna-nucleotide-char?
  (lambda (ls)
    (if (null? ls)
	(begin 
	  (display "Test Passed!")
	  (newline)
	  (display "Only ATGC DNA nucleotides were found!")
	  (newline))
	(case (car ls)
	  ((#\A #\T #\G #\C)
	   (dna-nucleotide-char? (cdr ls)))
	  (else 
	   (error 'lookup (format "~s is not a DNA nucleotide" ls)))))))

#|  

  Helper Functions: sequence-for-kmer  
  _______________________________________________________________________

  Contract: 
  sequence-for-kmer is a function that takes two arguements, 
  the list of nucleotides (ls) and the kmer-length (n) and returns 
  a list that is the (kmer-length - 1) nucleotides shorter. 
  
  Logic/Strategy: 
  In order to evaluate all of the possible kmers of length (n) 
  that exist within list of nucleotides, we must first remove (n - 1)
  nucleotides from the end of the list. 

  If the kmer-length were 14, then we must remove (14 - 1) = 13 
  nucleotides from the end of the list. In doing so, we ensure that 
  the list can be iterated through by kmers of length 14 
  (aka 14-mers). 
 
  Said differently, when a recursive function whose job is to grab 
  every 14-mer is exactly 14 nucleotides away from the end of the 
  list, we know that it will grab the final possible 14mer and hit 
  the end of the list -- which hopefully the recursive function's 
  termination case (aka base case). 

  But what if we didn't shave the list down? Well, with the function 
  we're proposing, we would likely hit an error. After the last 14mer
  is found, there will only 13 nucleotides (a 13mer) left in the list. 
  This by definition cannot me a 14mer, and by the contract of the 
  problem we're focused on the most common kmer where the kmer length 
  is given to us in the original txt data file. Indeed, a different 
  function with the logic for handling the case where the remaining 
  list is less than the desired kmer length can be (and 
  will be written) later. For now lets start with the kmerized sequence. 

  sequence-for-kmer input:     list of nucleotide-chars      kmer length         
  bound-variables:             (ls)                          (n)
  variable names:              ba1b-sequence

  output:                      

  Case Analysis Tests: 
  _______________________________________________________________________
 
  Using the macro check-equal? 
  to test Cases #1-4

  _______________________________________________________________________

|# 

#| 
   ---- Helper Function: sequence-for-kmer ---- 

   returns a list 14mer-sequence that has 13 less
   nucleotides than the raw list 
|# 

;; return a list that will be used by next helpers
(define sequence-for-kmer/h
  (lambda (ls kmer els)
    (cond
      ((< (length ls) kmer)
       (reverse els))
      (else
       (sequence-for-kmer/h (cdr ls) kmer
			    (cons (car ls) els))))))

(define sequence-for-kmer
  (lambda (ls) 
    (sequence-for-kmer/h ls 14 '())))

(define 14mer-sequence
  (sequence-for-kmer ba1b-sequence))

#|   
   ---- Helper Function: get-all-kmers ---- 
  
  returns a nested list of all kmers with 
  the value of their occurences within 
  the kmer list at the head of the list 
  
 |#

(define length-is-n?/h
  (lambda (ls acc n)
    (cond
      ((null? ls) #f)
      ((eq? acc n) #t)
      (else
       (length-is-n?/h (cdr ls) (add1 acc) n)))))

;; determines if length of a ls is a certain value
(define length-is-n?
  (lambda (ls n)
    (length-is-n?/h ls 1 n)))

;; determines if ls length is 14
(define length-is-14mer?
  (lambda (ls)
    (length-is-n?/h ls 1 14)))

;; returns els with 14 nucleotides on it 
(define get-kmer
  (lambda (ls els)
    (if (length-is-14mer? els)
	(reverse els)
	(get-kmer (cdr ls)
		  (cons (car ls) els)))))

(define get-all-kmers
  (lambda (ls)
    (cond
      ((<= (length ls) 13) '())
      (else 
       (cons (get-kmer ls '())
	     (get-all-kmers (cdr ls)))))))

(define 14mer-ls
  (get-all-kmers ba1b-sequence)) 

(define occurs
  (lambda (s ls)
    (cond
      ((null? ls) 0)
      ((equal? (car ls) s)
       (add1 (occurs s (cdr ls))))
      (else
       (occurs s (cdr ls))))))

(define s-member?
  (lambda (s ls)
    (cond
      ((null? ls) #f)
      (else
       (or (s-member? s (cdr ls))
	   (equal? s (car ls)))))))

(define most-common-kmer
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((s-member? (car ls) (cdr ls))
       (cons 
	(cons (occurs (car ls) ls)
	      (car ls))
	(most-common-kmer (cdr ls))))
      (else
       (most-common-kmer (cdr ls))))))

;; shows values of copies 
(define most-common-kmer-ls
  (most-common-kmer 14mer-ls))




#!eof
#|
 
  Function: 
  _______________________________________________________________________


  Final Answer: 
|#


#| ---- Function:  ---- |# 

#| ---- Function: /pmatch ----

  _______________________________________________________________________

  Final Answer:   

|#



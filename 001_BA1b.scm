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

(define in (open-input-file "../001_BA1b_FrequentWordsProblem/rosalind_ba1b.txt"))

(define port->list/read-char
  (lambda (port)
    (let ((next-char (read-char port)))
      (if (eof-object? next-char)
	  '()
	  (cons next-char
		(port->list/read-char port))))))
(define ba1b-r
  (port->list/read-char in))

(define kmer-length-finder
  (lambda (ls els)
    (cond
      ((eq? #\newline (car ls))
       (cons (reverse els) (cdr ls)))
      (else
       (kmer-length-finder (cdr ls)
		       (cons (car ls) els))))))

(define ba1b-kmer-length
  (string->number
   (list->string
    (remove-all #\newline
		(cdr (kmer-length-finder ba1b-r '()))))))

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

(define occurs
  (lambda (s ls)
    (cond
      ((null? ls) 0)
      ((equal? (car ls) s)
       (add1 (occurs s (cdr ls))))
      (else
       (occurs s (cdr ls))))))


(define length-is-k?/h
  (lambda (ls acc k)
    (cond
      ((null? ls) #f)
      ((= acc k) #t)
      (else
       (length-is-k?/h (cdr ls) (add1 acc) k)))))

(define length-is-kmer?
  (lambda (ls)
    (length-is-k?/h ls 1 ba1b-kmer-length)))

;; returns els with kmer length of nucleotides on it 
(define get-kmer
  (lambda (ls els)
    (if (length-is-kmer? els)
	(reverse els)
	(get-kmer (cdr ls)
		  (cons (car ls) els)))))

(define get-all-kmers
  (lambda (ls k)
    (cond
      ((< (length ls) k) '())
      (else 
       (cons (get-kmer ls '())
	     (get-all-kmers (cdr ls) k))))))

(define kmer-ls
  (get-all-kmers ba1b-sequence ba1b-kmer-length)) 

(define make-kmer-assoc-ls
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else
       (cons
	(cons (car ls) (occurs (car ls) ls))
	(make-kmer-assoc-ls (cdr ls)))))))

(define kmer-assoc-ls
  (make-kmer-assoc-ls kmer-ls))

;; use Chez Scheme's built in sort
;; problem here -- the output gives you all 8 copies of each kmer
;; you need to count the copy number but not cons the copy each time

(define kmer-assoc-ls/sorted
  (sort (lambda (ls1 ls2)
	  (> (cdr ls1) (cdr ls2))) kmer-assoc-ls))

;; note that sort assumes the car of the list to start 

;;takes an association list with structure as follows 
;; (((#\G #\C #\G #\A #\G #\G #\G #\A #\C #\A #\T #\G #\T #\A)
;;   .
;;   8) ....)

(define kmer-repeat-value
  (cdar kmer-assoc-ls/sorted))

;;returns list of pairs (kmer . repeat)
(define get-most-common-kmers/pair
  (lambda (x ls)
    (cond
      ((null? ls) '())
      ((eqv? x (cdar ls))
       (cons (car ls)
	     (get-most-common-kmers/pair x (cdr ls))))
      (else
       (get-most-common-kmers/pair x (cdr ls))))))

;; returns a list with most common kmers 
(define get-most-common-kmers
  (lambda (x ls)
    (cond
      ((null? ls) '())
      ((eqv? x (cdar ls))
       (cons (list->string (caar ls))
	     (get-most-common-kmers x (cdr ls))))
      (else
       (get-most-common-kmers x (cdr ls))))))

;; specific for 8 repeat of the 14mer-assoc-ls
;; kmer-repeat-value holds the highest number of repeats
(define most-common-kmers
  (lambda (ls)
    (get-most-common-kmers kmer-repeat-value ls)))

;;outputs the strings of each most common kmer
(define most-common-kmers-ls
  (most-common-kmers kmer-assoc-ls))

(display (format "The most common kmer(s) listed below had ~s repeats. \n" kmer-repeat-value))
(newline)
(display (format "Most Common Kmers: ~s. \n" most-common-kmers-ls))

#!eof 

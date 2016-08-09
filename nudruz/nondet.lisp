(in-package :cm)
;;(use-package :screamer)

;; load nondeterminism stuff ;;;;  not (use-package 'screamer)
; MKG (in-package :screamer-user)

(defun list-eql (list1 list2)
  (and (subsetp list1 list2)
       (subsetp list2 list1)
       (= (length list1) (length list2))))

;; N-OF-LIST -- utility for wiggle-to2
(defun n-of-list (n l)
  (if (= n 0) '()
      (cons (a-member-of l) (n-of-list (- n 1) l))))

;; N-OF-LISTV -- similar utility
(defun n-of-listv (n l)
  (if (= n 0) '()
      (cons (a-member-ofv l) (n-of-list (- n 1) l))))

;; WIGTO -- using only specified intervals in path from 'startnum' to 'endnum'
;; with any length for 'allowed-ints' list
;; J. Siskind's solution
(defun wigto (startnum endnum maxlen allowed-ints)
  (remove-duplicates
   (all-values
    (let ((multvars (n-of-list (screamer::an-integer-between 0 maxlen) allowed-ints)))
      (unless (= (reduce #'+ multvars) (- endnum startnum)) (fail))
      multvars))
   :test #'list-eql))

;; EQLSUM -- finds all combos of 'componentnums' that sum to 'targetnum'
(defun eqlsum (targetnum componentnums)
  (remove-duplicates
   (all-values
    (let* ((maxlen (ceiling (/ targetnum (reduce #'min componentnums))))
           (multvars (n-of-list (screamer::an-integer-between 0 maxlen) componentnums)))
      (unless (= (reduce #'+ multvars) targetnum) (fail))
      multvars))
   :test #'list-eql))

;; EBT -- list of random 'neighbor' triads to triad 
;; with largest leap of 'span' and same sum
;; example: (ebt '(3 5 6) 1) = ((2 5 7) (3 4 7))
;; used by 'embell-triad' wrapper in nudruz.lisp 
(defun ebt (triad &optional (span 3))
   (remove-duplicates 
            (all-values
             (let* ((triadsum (apply #'+ triad))
                    (sortd (sort triad #'<))
                    (a screamer::(an-integer-between (max 0 (- (first sortd) span))
                                           (+ (first sortd) span)))
                    (b (screamer::an-integer-between (- (second sortd) span)
                                           (+ (second sortd) span)))
                    (c (screamer::an-integer-between (- (third sortd) span)
                                           (+ (third sortd) span))))
               (assert! (= (+ a b c) triadsum))
               (assert! (not (or (= a b) (= a c) (= b c))))
               (assert! (not (list-eql (list a b c) sortd)))
               (list a b c))) :test #'list-eql))


;; NEAR-EBT -- list of random 'neighbor' triads to triad 
;; with largest leap of 'span' and sum within 'sumspan' range of orig. sum
;; example: (ebt '(3 5 6) 1) = ((2 5 7) (3 4 7))
;; used by 'embell-triad' wrapper in nudruz.lisp 
(defun near-ebt (triad &optional (sumspan 5) (span 3))
   (remove-duplicates 
            (all-values
             (let* ((triadsum (apply #'+ triad))
                    (sortd (sort triad #'<))
                    (a (screamer::an-integer-between (max 0 (- (first sortd) span))
                                           (+ (first sortd) span)))
                    (b (screamer::an-integer-between (- (second sortd) span)
                                           (+ (second sortd) span)))
                    (c (screamer::an-integer-between (- (third sortd) span)
                                           (+ (third sortd) span))))
               (assert! (< (abs (- (+ a b c) triadsum)) sumspan))
               (assert! (not (or (= a b) (= a c) (= b c))))
               (assert! (not (list-eql (list a b c) sortd)))
               (list a b c))) :test #'list-eql))

;; ALL-BTWN -- all numbers between & incl. two nums,
;; allows for ascending/descending 
(defun all-btwn (num1 num2)
  (if (> num2 num1)
    (all-values
     (screamer::an-integer-between num1 num2))
    (reverse
     (all-values
      (screamer::an-integer-between num2 num1)))))


;; CONTOUR-CONSTRAINTS-ND
;; utility to establish constraints for "non-to-clists"
(defun contour-constraints-nd (antn cvec)
  (eval
   (cons 'andv
         (loop for n to (- (length antn) 1) collect
               (funcall (case (nth n antn) 
                          (1 '<v)
                          (0 '=v)
                          (-1 '>v))
                        (nth n cvec)  
                        (nth (+ n 1) cvec))))))

;; NTN-TO-CLISTS
;; returns all contour vectors within binsize 
;; that comply with ntn (note-to-note) list
;; (ntn-to-clists '(-1 1) 3) = ((1 0 1) (2 0 1) (1 0 2) (2 0 2) (2 1 2))
(defun ntn-to-clists (antn binsize)
  (all-values
   (let* ((alen (+ (length antn) 1))
          (cvec (n-of-listv alen (all-btwn 0 (- binsize 1)))))
     (assert! (contour-constraints-nd antn cvec))
     (solution cvec 
               (reorder #'domain-size
                        #'(lambda (x) (declare (ignore x)) nil)
                        #'<
                        #'divide-and-conquer-force)))))

;; stacked canon stuff

;; first species stacked canon

(defun intv (x y)
	(- y x))

(defun abs-intv (x y &optional (modlen 12))
	(mod (abs (- y x)) modlen))

(defun consn-p (x y &optional (consnlist '(3 4 7 8 9 12)))
  (or (eql x 'r) 
      (eql y 'r) 
      (member (abs-intv x y) consnlist)))


;; CANTEST -- useful deterministic test for nondet canons
;; tlevel = comes transposition level
;; skip = # places skipped
(defun cantest (inlist tlevel &optional (skip 1) (consvec '(3 4 7 8 9 12)))
  (not (set-difference
        (mapcar #'intv (nthcdr skip inlist) 
                (mapcar (lambda (x) (+ x (* skip tlevel))) 
                        inlist))
        consvec)))

;; SCAN-CONSTRAINT-ND -- checks for melodies that pass stacked canon test
(defun scan-constraint-nd (invec tlevel vcs &optional (consvec '(3 4 7 8 9 12)))
  (eval
   (cons 'andv
         (loop for n from 1 to (- vcs 1) collect
               (funcallv #'cantest invec tlevel n consvec)))))

;; SCANONS-ND -- finds all permissible melodies for stacked canons
;; mlen = length of melody
;; tlevel = canon interval
;; vcs = total number of voices in the canon
;; consvec = mod12 consonances
(defun scanons-nd (mlen tlevel vcs &optional (consvec '(3 4 7 8 9 12)))
  (all-values
   (let ((cvec (n-of-list mlen '(0 1 2 3 4 5 6 7 8 9 10 11))))
     (assert! (scan-constraint-nd cvec tlevel vcs consvec))
     (solution cvec 
               (reorder #'domain-size
                        #'(lambda (x) (declare (ignore x)) nil)
                        #'<
                        #'linear-force)))))

;; TZPATHFACTORS -- computes paths to any transposition
;; returns all vectors (lr# ud# diag#)
(defun tzpathfactors (x y tlevel &optional (modlen 12))
  (all-values
   (let ((n (screamer::an-integer-between 
             (* -1 (floor (/ modlen (* 2 x))))
             (floor (/ modlen (* 2 x)))))
         (m (screamer::an-integer-between 
             (* -1 (floor (/ modlen (* 2 y))))
             (floor (/ modlen (* 2 y)))))
         (p (screamer::an-integer-between -6 6)))
     (unless (= tlevel (mod
                   (+ (* n x) (* m y) (* p (+ x y)))
                   modlen))
       (fail))
     (list n m p))))


;; 
; BESTMATCH

;; NONMATCH -- 'pairlist' in from 'nudruz/non-matches'
;; 'vars1' & 'vars2' from 'bestmatch'
(defun nonmatch (pairlist vars1 vars2)
  (eval
   (cons 'andv
	 (mapcar 
	  (lambda (x) (notv (equalv (nth (first x) vars1)
				    (nth (second x) vars2))))
	 pairlist))))

;; BESTMATCH 
(defun bestmatch (len1 len2 pairlist)
  (all-values
    (let* ((maxlen (+ len1 len2))
	   (mel1places
	    (n-of-list len1 (loop for n to (- maxlen 1) collect n)))
	   (mel2places
	    (n-of-list len2 (loop for n to (- maxlen 1) collect n))))
      (assert!
	(eval 
	 (cons '<v mel1places)))
      (assert! 
	(eval 
	 (cons '<v mel2places)))
      (assert! 
	(eql 0 (apply #'min (append mel1places mel2places))))
      (assert! 
	(eql (length (remove-duplicates (append mel1places mel2places)))
	     (+ 1 (apply #'max (append mel1places mel2places)))))
      (assert! 
	(nonmatch pairlist mel1places mel2places)))
    (list mel1places mel2places)))

;; call with "(screamer-user::wiggle-to2 45 48 8 '(2 -1))" etc.


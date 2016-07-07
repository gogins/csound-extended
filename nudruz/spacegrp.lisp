;; SPACEGRP.LISP
;; some interesting transformations using matrix groups
;; to extend traditional (dihedral) transformations

;; Drew Krause, 2004
;; drkrause@mindspring.com
;; www.wordecho.org

;; (load "nudruz.lisp")

;; LISTS->PTP -- makes array of keynums & durations
;; for use with 'mx' transformations
;; (lists->ptp '(50 60 55 43) '(3 2 4 1)) =
;; #2A((50 0) (60 3) (55 5) (43 9))
(defun lists->ptp (pitlist durlist)
  (let* ((tplist (butlast (melint->line 0 durlist)))
         (adjlen (min (length pitlist) (length tplist)))
         (nupits (loop repeat adjlen for x in pitlist collect x))
         (nutps (loop repeat adjlen for y in tplist collect y))
	 (dims (list 2 (length nupits))))
    (cllib:matrix-transpose
     (make-array dims :initial-contents (list nupits nutps)))))

;;; different mappings in/out of spacegroups

;; PITENC7 -- makes (mod7 octave) pairs
(defun pitenc7 (apitch)
  (if (listp apitch)
    (loop for x in apitch collect (pitenc7 x))
  (let ((fpit (mod apitch 96)))
    (list (mod (* 7 fpit) 12)
          (floor (/ fpit 12))))))

;; PITENC5 -- makes (mod5 octave) pairs
(defun pitenc5 (apitch)
  (if (listp apitch)
    (loop for x in apitch collect (pitenc5 x))
  (let ((fpit (mod apitch 96)))
    (list (mod (* 5 fpit) 12)
          (floor (/ fpit 12))))))

;; PAIRS->PTP -- utility to bring in pairs
(defun pairs->ptp (pairs)
    (make-array (list (length pairs) 2) :initial-contents pairs))

;; PTP->PIT7 -- decodes array to pits mod7
(defun ptp->pit7 (a-ptp)
  (loop for n to (- (array-dimension a-ptp 0) 1) collect
        (+ (mod (* (aref a-ptp n 0) 7) 12)
           (* 12 (mod (aref a-ptp n 1) 7)))))

;; PTP->PIT5 -- decodes array to pits mod5
(defun ptp->pit5 (a-ptp)
  (loop for n to (- (array-dimension a-ptp 0) 1) collect
        (+ (mod (* (aref a-ptp n 0) 5) 12)
           (* 12 (mod (aref a-ptp n 1) 7)))))

;; PITENCTZ -- makes (mod4 mod3) sum pairs
(defun pitenctz (apitch)
  (if (listp apitch)
    (loop for x in apitch collect (pitenctz x))
  (let ((fpit (mod apitch 12)))
    (case fpit 
      (0 '(0 0)) (1 '(3 1)) (2 '(2 2)) (3 '(1 0))
      (4 '(0 1)) (5 '(3 2)) (6 '(2 0)) (7 '(1 1))
      (8 '(0 2)) (9 '(3 0)) (10 '(2 1)) (11 '(1 2))))))
          
;; PTP->PITTZ -- decodes array to pits by mod4/3 tonnetz
(defun ptp->pittz (a-ptp)
  (loop for n to (- (array-dimension a-ptp 0) 1) collect
        (let* ((x (mod (aref a-ptp n 0) 4))
               (y (mod (aref a-ptp n 1) 3))
               (modpair (list x y)))
          (cond
            ((seq-eql modpair '(0 0)) 0)
            ((seq-eql modpair '(3 1)) 1)
            ((seq-eql modpair '(2 2)) 2) 
            ((seq-eql modpair '(1 0)) 3)
            ((seq-eql modpair '(0 1)) 4) 
            ((seq-eql modpair '(3 2)) 5) 
            ((seq-eql modpair '(2 0)) 6) 
            ((seq-eql modpair '(1 1)) 7)
            ((seq-eql modpair '(0 2)) 8) 
            ((seq-eql modpair '(3 0)) 9) 
            ((seq-eql modpair '(2 1)) 10) 
            ((seq-eql modpair '(1 2)) 11)))))


;; create 2x2 scaling matrix
;; (scale-array 3) = 
(defun scale-array (scalesize)
  (cllib:matrix-multiply scalesize
			 (cllib:matrix-id 2)))

;; SCALE-PTP scale pitch & rhythm
;; (scale-ptp 3 (lists->ptp '(1 2 3) '(2 2 1)))
;; =  #2A((3 0) (6 6) (9 12))
(defun scale-ptp (scalesize ptparray1)
  (cllib:matrix-multiply ptparray1 (scale-array scalesize)))

;; MX-APPLY -- basic operation
;; myarray = #2A((3 1) (2 2) (6 4) (7 5))
;; (mx-apply mx12 myarray) = #2A((4 -1) (4 -2) (10 -4) (12 -5))
(defun mx-apply (a-mx ptp-array)
  (cllib:matrix-multiply ptp-array a-mx))

;; HEAP-MX -- make a random list of 'spacegroup' transforms
;; (heap-mx matz12 2) = (#2A((0 -1) (1 -1)) #2A((1 0) (0 1)))
(defun heap-mx (spacegroup len)
  (loop for x in (heapvec len (length spacegroup)) 
	collect (nth x spacegroup)))

;; MXLIST -- makes list of transformed pit/tp arrays
;; -->> applies all transforms to initial array
;; run as (mxlist a-mxlist ptparray)
(defun mxlist (a-mxlist ptparray1)
  (loop for x in a-mxlist collect
        (mx-apply x ptparray1)))

;;; MXCHAIN -- makes list of tranformed pit/tp arrays
;;; -->> applies transform to the most recent entry!
;;; (mxchain matz6 (lists->ptp '(3 2 6 7) '(1 2 4 5)))
(defun mxchain (a-mxlist ptparray1)
  (cons ptparray1
	(when a-mxlist
	  (mxchain (cdr a-mxlist)
                   (mx-apply (first a-mxlist) ptparray1)))))


;; 
;; pit/dur interface

;; PTP->LISTS -- makes pit & dur lists from ptparray
(defun ptp->lists (a-ptp &optional (basepit 0))
  (let* ((pitlist (transp (loop for n to (- (array-dimension a-ptp 0) 1)
                                collect (aref a-ptp n 0)) basepit))
         (init-tplist (loop for n to (- (array-dimension a-ptp 0) 1)
                            collect (aref a-ptp n 1)))
         (tplist (transp init-tplist (* -1 (apply #'min init-tplist))))
         (combined-list (combine-pits pitlist tplist)))
    (list (first combined-list) 
          (append (melint (second combined-list)) 
                  (list 1)))))

;; MINSTARTPIT -- utility 
;; lowest starting pitch in a list of lists
(defun minstartpit (pitlist)
  (first (bottomline pitlist)))

;; SPACEGRP->MOTIVE -- from lists back to motive pit/dur paired lists
;; uses "mxtype" as 'list (default) or 'chain
;; returns each motive in separate pit/dur paired lists
;; run as: (spacegrp->motive mxlist pitlist durlist {'list or 'chain})
(defun spacegrp->motive (a-mxlist pitlist durlist &optional (mxtype 'list))
  (let* ((newptp (lists->ptp pitlist durlist))
         (arraylist (if (equal mxtype 'list)
                      (mxlist a-mxlist newptp)
                      (mxchain a-mxlist newptp)))
         (basepit (minstartpit pitlist))
         (outlists (loop for x in arraylist collect
          (ptp->lists x))))
    (loop for y in outlists collect
         (list (loop for pit in (first y) collect
                     (transp pit (- basepit (minstartpit (first y)))))
               (second y)))))

;; SPACEGRP->VECS -- from lists back to big pit/dur lists
(defun spacegrp->vecs (a-mxlist pitlist durlist &optional (mxtype 'list))
  (let* ((spgrp (spacegrp->motive a-mxlist pitlist durlist mxtype))
         (the-pits (loop for x in spgrp append (first x)))
         (the-durs (loop for y in spgrp append (second y))))
         (list the-pits the-durs)))


;; playback example

;(define mxplay
;  (process for mx in (spacegrp matz12 '(70 67 65 61 65) '(1 2 1 2 1))
;           for tactus = .25
;           for bigdur = 7
;           sprout (process for x from 0 to (- (length (first mx)) 1)
;                           for pit = (nth x (first mx))
;                           for dur = (* tactus (nth x (second mx)))
;                           output (multievent 'midi :keynum 
;                                              :keynum pit
;                                              :time (now)
;                                              :duration dur)
;                           wait dur)
;           wait bigdur))

;; store them in a midi file
; (events mxplay "mxplay.midi")

;; register interface (added Sept. 2008)

;; REGS->PTP -- converts register list to matrix
;; (regs->ptp '(2 1 r (5 3) 4)) = #2A((0 2) (1 1) (3 5) (3 3) (4 4))
(defun regs->ptp (reglist &optional (pitflag nil))
  (let* ((myregs (if pitflag (takereg reglist) reglist))
	 (rgs
	  (no-nils
	   (flatten
	    (mapcar 
	     (lambda (p) (loop for x in p collect (if (numberp x) x)))
	     (not-flat myregs)))))
	 (places
	  (no-nils
	   (flatten
	    (map 'list
		 (lambda (a b) (copylist (list a) b))
		 (indices (length myregs))
		 (take-poly myregs))))))
    (cllib:matrix-transpose
     (make-array (list 2 (length places)) 
		 :initial-contents (list places rgs)))))

;; SPACEREG -- applies a group element (2x2 matrix) to reglist
(defun spacereg (mxval inregs)
  (let* ((myarray
	  (mx-apply mxval
		    (regs->ptp inregs)))
	 (outvec
	  (transpose-matx (array->list myarray)))
	 (rawslots (first outvec))
	 (rawregs (second outvec))
	 (slots
	  (transp rawslots 
		  (- 0 (apply #'min rawslots))))
	 (regs
	  (transp rawregs 
		  (- 
		   (apply #'min (norests (flatten inregs)))
		   (apply #'min rawregs)))))
    (like-flat
     (loop for n to (apply #'max slots) collect
	   (if (member n slots)
	       (mapcar (lambda (x) (nth x regs)) (positions n slots))
	       'r)))))

;; SPACEREGLIST -- applies list of spacereg transforms 
;; -->> applies all transforms to initial reglist
;; outputs flattened reglist
(defun spacereglist (mxlist reglist &optional (treeflag nil))
  (let ((raw
	 (loop for x in mxlist collect
	       (spacereg x reglist))))
    (if treeflag raw (flatter raw))))

;; SPACEREGCHAINRAW
;; [does the work, wrapped by 'spaceregchain']
(defun spaceregchainraw (mxlist reglist)
   (cons reglist
	 (when mxlist
	   (spaceregchainraw (cdr mxlist)
			  (spacereg (first mxlist) reglist)))))

;; SPACEREGCHAIN
;; applies chain of spacereg transforms
;;; -->> applies transform to the most recent entry!
;; outputs flattened reglist
(defun spaceregchain (mxlist reglist &optional (treeflag nil))
  (let ((raw (spaceregchainraw mxlist reglist)))
    (if treeflag raw (flatter raw))))

;; SPACEGROUP DATA BELOW (mx's used in for all transforms

;; from the Dade group (here called 'matz12')
;; "gap> MatGroupZClass(2,4,4,1)"
(define mx1 (make-array 
	     '(2 2) :initial-contents '((-1 0) (-1 1))))
(define mx2 (make-array 
	     '(2 2) :initial-contents '((-1 0) (0 -1))))
(define mx3 (make-array 
	     '(2 2) :initial-contents '((-1 1) (-1 0))))
(define mx4 (make-array 
	     '(2 2) :initial-contents '((-1 1) (0 1))))
(define mx5 (make-array 
	     '(2 2) :initial-contents '((0 -1) (-1 0))))
(define mx6 (make-array 
	     '(2 2) :initial-contents '((0 -1) (1 -1))))
(define mx7 (make-array 
	     '(2 2) :initial-contents '((0 1) (-1 1))))
(define mx8 (make-array 
	     '(2 2) :initial-contents '((0 1) (1 0))))
(define mx9 (make-array 
	     '(2 2) :initial-contents '((1 -1) (0 -1))))
(define mx10 (make-array 
	      '(2 2) :initial-contents '((1 -1) (1 0))))
(define mx11 (make-array 
	      '(2 2) :initial-contents '((1 0) (0 1))))
(define mx12 (make-array 
	      '(2 2) :initial-contents '((1 0) (1 -1))))

;; MATZ12 -- list of all mx** [12 total]
(define matz12
  (list mx1 mx2 mx3 mx4 mx5 mx6 mx7 mx8 mx9 mx10 mx11 mx12))
 
;; "gap> MatGroupZClass(2,4,2,1)"
;; here called "matz6"
(define mb1 (make-array 
	     '(2 2) :initial-contents '((-1 1) (-1 0))))
(define mb2 (make-array 
	     '(2 2) :initial-contents '((-1 1) (0 1))))
(define mb3 (make-array 
	     '(2 2) :initial-contents '((0 -1) (-1 0))))
(define mb4 (make-array 
	     '(2 2) :initial-contents '((0 -1) (1 -1))))
(define mb5 (make-array 
	     '(2 2) :initial-contents '((1 0) (0 1))))
(define mb6 (make-array 
	     '(2 2) :initial-contents '((1 0) (1 -1))))

;; MATZ6 -- six-element space group "mb*" 
;; "gap> MatGroupZClass(2,4,2,1)"
(define matz6
  (list mb1 mb2 mb3 mb4 mb5 mb6))

;; "gap> MatGroupZClass(2,2,2,1)"
;; here called "matz4"
(define mw1 (make-array 
	     '(2 2) :initial-contents '((-1 0) (0 -1))))
(define mw2 (make-array 
	     '(2 2) :initial-contents '((-1 0) (0 1))))
(define mw3 (make-array 
	     '(2 2) :initial-contents '((1 0) (0 -1))))
(define mw4 (make-array 
	     '(2 2) :initial-contents '((1 0) (0 1))))

;; MATZ4 -- six-element space group "mb*" 
;; "gap> MatGroupZClass(2,2,2,1)"
(define matz4
  (list mw1 mw2 mw3 mw4))

;; "gap> MatGroupZClass(2,2,2,2)"
;; here called "
(define mr1 (make-array 
	     '(2 2) :initial-contents '((-1 0) (0 -1))))
(define mr2 (make-array 
	     '(2 2) :initial-contents '((0 -1) (-1 0))))
(define mr3 (make-array 
	     '(2 2) :initial-contents '((1 0) (0 1))))
(define mr4 (make-array 
	     '(2 2) :initial-contents '((0 1) (1 0))))

(define mrmat (list mr1 mr2 mr3 mr4))

; 2,3,2,1

(define mz1 (make-array 
	     '(2 2) :initial-contents
	     '(( -1 0 ) ( 0 -1 ))))
(define mz2 (make-array 
	     '(2 2) :initial-contents 
	     '( ( -1 0 ) ( 0 1 ) )))
(define mz3 (make-array 
	     '(2 2) :initial-contents
	     '( ( 0 -1 ) ( -1 0 ) )))
(define mz4 (make-array 
	     '(2 2) :initial-contents
	     '( ( 0 -1 ) ( 1 0 ) )))
(define mz5 (make-array 
	     '(2 2) :initial-contents
	     '( ( 0 1 ) ( -1 0 ) ))) 
(define mz6 (make-array 
	     '(2 2) :initial-contents
	     '( ( 0 1 ) ( 1 0 ) )))
(define mz7 (make-array 
	     '(2 2) :initial-contents
	     '( ( 1 0 ) ( 0 -1 ) ))) 
(define mz8 (make-array 
	     '(2 2) :initial-contents
	     '( ( 1 0 ) ( 0 1 ) ) ))

(define mzmat (list mz1 mz2 mz3 mz4 mz5 mz6 mz7 mz8))

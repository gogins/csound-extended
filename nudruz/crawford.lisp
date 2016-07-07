; inspired by J"The Music of Ruth Crawford Seeger"

; (load "nudruz.lisp")
; (load "rewrite.lisp")
; (load "data/besthex.lisp")

;; APPLYING UP/DOWN CONTOURS

;; UDWN -- note-to-note utility for 'make-updown'
(defun udwn (startpit endpit udval &optional (modlen 12))
  (let* ((m-startpit (mod startpit modlen))
	 (m-endpit (mod endpit modlen))
	 (modfloor (floor (/ startpit modlen)))
	 (grtrflag
	  (> m-startpit m-endpit)))
    (case udval
      (1 
       (+ m-endpit (* modlen 
		      (+ (if grtrflag 1 0) modfloor))))
      (-1
       (+ m-endpit (* modlen 
		      (+ (if grtrflag 0 -1) modfloor))))
      (t startpit))))

;; MAKE-UPDOWN -- applying +1/-1 contour to 'alist' from first pit 
(defun make-updown (alist udvals &optional (modlen 12))
  (if (eql 2 (length alist))
      (list (car alist) (udwn (car alist) (cadr alist) (car udvals) modlen))
      (cons (car alist)
	    (make-updown
	     (cons (udwn (car alist) (cadr alist) (car udvals) modlen)
		   (cddr alist))
	     (cdr udvals)
	     modlen))))

;; TAKE-UPDOWN - same as 'directions'
(defun take-updown (melody)
  (directions melody))

;; UPDOWN-OPPOSITE -- opposite signs
(defun updown-opposite (udvec)
  (mapcar (lambda (x) (* -1 x)) udvec))

;; UPDOWN-RANDOM -- random contour
(defun updown-random (len)
  (listsub '(-1 1) '(0 1) 
	   (random-codeword len)))

;; UPDOWN-CONVERSIONS -- list of updown p,i,r,ri
(defun updown-conversions (udvec)
  (list udvec
	(updown-opposite udvec)
	(reverse udvec)
	(reverse (updown-opposite udvec))))

;; MAKE-UPDOWN-RANDOM -- apply random updown
(defun make-updown-random (alist &optional (modlen 12))
  (make-updown alist 
	       (updown-random (- (length alist) 1))
	       modlen))

;; FLIP-CONTOUR -- change up/down directions
(defun flip-contour (melody)
  (make-updown melody 
	       (updown-opposite (take-updown melody))))

; UPDOWN-FROMTO -- stepwise up/down contour change (incl first & last)
(defun updown-fromto (ud-start ud-end)
  (let* ((start01
	  (listsub '(0 1) '(-1 1) ud-start))
	 (end01
	  (listsub '(0 1) '(-1 1) ud-end)))
    (mapcar
     (lambda (x) (chooser x '(-1 1)))
     (fromto-stepper start01 end01))))


;; (all-updowns len) ?

;; neumes = three or four pitches
;; "neumatic transformations" (p. 34)

;; CRAW-INTS
;; each interval is replaced by its complement or compound
(defun craw-ints (neume &optional (modlen 12))
  (let* ((neumelint (melint neume))
	 (neumintlen (length neumelint)))
    (mapcar (lambda (x) (melint->line (car neume) x))
	    (loop for victim to (- neumintlen 1) append
		  (loop for trindx in '(1 -1) collect
			(loop for n to (- neumintlen 1) collect
			      (if (eql n victim)
				  (+ (nth n neumelint)
				     (* trindx modlen))
				  (nth n neumelint))))))))

;; CRAW-MELCONVERSIONS
(defun craw-melconversions (mymel)
  (let* ((mymint (melint mymel))
	 (mymintit (transp mymint -1 #'*)))
    (mapcar 
     (lambda (x) 
       (melint->line (car mymel) x))
     (list mymint
	   mymintit
	   (reverse mymint)
	   (reverse mymintit)))))

;; CRAW-CONV -- random melconversion
(defun craw-conv (amel)
  (pickl (craw-melconversions amel)))

;; CRAW-MULT -- all intervals multiplied by a constant
(defun craw-mult (amel mfactor)  
  (let* ((amint (melint amel)))
    (melint->line (car amel) (transp amint mfactor #'*))))

;; CRAW-ADD -- all intervals expanded/contracted by a constant
(defun craw-add (amel mfactor)  
  (let* ((amint (melint amel)))
    (melint->line (car amel) 
		  (map 'list #'*
		       (take-updown amel)
		       (transp (mapcar #'abs amint) mfactor)))))

;; CRAW-PE -- 'partial expansion' (all collected)
;; one interval expands by a semitone
(defun craw-pe (amel)
  (let* ((mint (melint amel))
	 (pevec
	  (mapcar (lambda (x)
		    (cond ((plusp x) (+ x 1))
			  ((minusp x) (- x 1))
			  (t x)))
		  mint))
	 (mlen (- (length mint) 1)))
    (mapcar
     (lambda (x) (melint->line (car amel) x))
     (reverse
      (set-difference 
       (loop for m to mlen collect
	     (loop for n to mlen collect
		   (if (= m n) (nth n pevec) (nth n mint))))
       (list mint) 
       :test #'seq-eql)))))

;; CRAW-PC -- 'partial expansion' (all collected)
;; one interval contracts by a semitone
(defun craw-pc (amel)
  (let* ((mint (melint amel))
	 (pevec
	  (mapcar (lambda (x)
		    (cond ((plusp x) (- x 1))
			  ((minusp x) (+ x 1))
			  (t x)))
		  mint))
	 (mlen (- (length mint) 1)))
    (mapcar
     (lambda (x) (melint->line (car amel) x))
     (reverse
      (set-difference 
       (loop for m to mlen collect
	     (loop for n to mlen collect
		   (if (= m n) (nth n pevec) (nth n mint))))
       (list mint) 
       :test #'seq-eql)))))

;; ALL-CRAW -- all transformations
;; useful for 'generic-path' & 'generic-branch'
(defun all-craw (amel &optional (modlen 12))
  (remove-duplicates  
   (append (craw-ints amel)
	   (craw-melconversions amel)
	   (loop for n from 1 to 7 collect
		 (craw-mult amel n))
	   (loop for n from -11 to 11 collect
		 (craw-add amel n))
	   (craw-pe amel))
   :test #'seq-eql))

;; ALL-CRAW-PE-PC -- all contractions + expansions 
(defun all-craw-pe-pc (amel)
  (remove-duplicates
   (append (craw-pe amel) (craw-pc amel))
   :test #'seq-eql))

;;

;; verse rhythms -- inspired by Crawford

(defun make-verse (len)
  (mapcar
   (lambda (x) (ferney '(1) x (pickl (cons x (ones&twos x)))))
   (randvec len 3 3)))

; verse variations

;; VERSE-SWAPS -- adjacent bars swapped (all possible)
(defun verse-swaps (averse)
  (chooser
   (allswaps (indices (length averse)))
   averse))

;; VERSE-DROPS -- single verse dropped (all possible)
(defun verse-drops (averse)
  (let ((verseidx (indices (length averse))))
    (mapcar 
     (lambda (x) (chooser x averse))
     (mapcar 
      (lambda (x) 
	(safesort
	 (set-difference verseidx (list x))))
      verseidx))))

; SWAPDROP-CHAIN -- recursive random swaps & drops
(defun swapdrop-chain (a-verse len)
  (no-nils
   (recurz 
    (lambda (x)
      (pickl (append (verse-swaps x)
		     (verse-drops x))))
    a-verse
    len)))

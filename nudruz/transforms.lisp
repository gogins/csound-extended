;; transforms -- chord transformations

;; generalized paths & branches

;; GENERIC-NFORM-PATH -- determines a* path between normal forms
(defun generic-nform-path (func nform1 nform2)
  (let ((answer
         (a* nform1 nform2
             :Goal-Function #'seq-eql
             :Child-Generator-Function func
             :H-Function #'listdist-mod)))
    (if (eql (length answer) (length (flatten answer)))
        (list answer)
        (butlast answer))))

;; GENERIC-PATH -- path from any chd to another (same length)
;; preserves registers
;; omits target chd
;; 'tendreg' is slow! default is 'matchreg' [tendreg optional..]
(defun generic-path (funcname chd1 chd2 &optional (tendflag nil))
  (let* ((n1 (normal-form chd1))
	 (n2 (normal-form chd2))
	 (npath (generic-nform-path funcname n1 n2))
	 (rawpath     (append
		       (cons chd1 (cdr npath))
		       (list chd2))))
    (if tendflag 
	(butlast (tendreg rawpath))
	(let ((mrpath (matchreg-chds rawpath)))
	  (if (and (> (length mrpath) 1) 
		   (list-eql (car (last mrpath)) chd2))
	      (butlast mrpath)
	      mrpath)))))

;; GENERIC-BRANCH -- applies rsis-path to each pair in a list of chds
;; (includes final chord)
;; can be list of chords or list of paths
(defun generic-branch (funcname chds &optional (flatflag nil) (tendflag nil))
  (let ((rawlist (append
		  (mapcar
		   (lambda (x y) (generic-path funcname x y tendflag))
		   chds (cdr chds)) 
		  (list (last chds)))))
    (if flatflag (apply #'append rawlist) rawlist)))


;; some transformations

;;; CTONE -- shuffles sis w/ at least one c.t.
;; generally 1 or 2 distinct generated prime-forms
;; branches more often for chdlen > 3
(defun ctone (chd)
  (let* ((chdsis (sis (normal-form chd)))
	 (perms (permutations chdsis)))
    (set-difference
     (remove-duplicates
      (loop for pit in chd append
	    (mapcar (lambda (perm) (normal-form 
				    (melint->line pit perm)))
		    perms))
      :test #'list-eql)
     (list (normal-form chd))
     :test #'list-eql)))

;; TRITONE-FUNC -- one member moves a tritone
(defun tritone-func (chd)
  (filter (lambda (x) (eql (length x) (length chd)))
	  (loop for x in chd collect 
		(normal-form
		 (cons (+ x 6) (set-difference chd (list x)))))))

;; all permutations of 12-3 partitions
(defparameter threeparts
    '((4 4 4) (5 4 3) (5 3 4) (4 5 3) (4 3 5) (3 5 4) (3 4 5) (5 5 2) (5 2 5)
      (2 5 5) (6 3 3) (3 6 3) (3 3 6) (6 4 2) (6 2 4) (4 6 2) (4 2 6) (2 6 4)
      (2 4 6) (6 5 1) (6 1 5) (5 6 1) (5 1 6) (1 6 5) (1 5 6) (7 3 2) (7 2 3)
      (3 7 2) (3 2 7) (2 7 3) (2 3 7) (7 4 1) (7 1 4) (4 7 1) (4 1 7) (1 7 4)
      (1 4 7) (8 2 2) (2 8 2) (2 2 8) (8 3 1) (8 1 3) (3 8 1) (3 1 8) (1 8 3)
      (1 3 8) (9 2 1) (9 1 2) (2 9 1) (2 1 9) (1 9 2) (1 2 9) (10 1 1) (1 10 1)
      (1 1 10)))


;;; other transformations

(defun altriad-func1 (input)
  (no-nils
   (mapcar (lambda (s) (if (eql (length s) (length input)) s))
	   (loop for x in (allrots input) collect
		 (let ((a (first x))
		       (b (second x))
		       (c (third x)))
		   (normal-form
		    (flatten (list (placereg (- 12 (mod12 a)) (takereg a))
				   (placereg (- c b) (takereg b))
				   (placereg (mod12 (+ c 5)) (takereg c))))))))))

;; EMBELL-FUNC -- using 'embell-triad'
(defun embell-func (x &optional (step 1) (sumspan 2))
  (remove-duplicates
   (filter (lambda (m) (eql 3 (length m)))
	   (mapcar #'normal-form
		   (embell-triad (normal-form x) step sumspan)))
   :test #'seq-eql))

;; EMBELLPC-FUNC -- filters 'embell-func' by prime-form
(defun embellpc-func (x &optional (step 3) (sumspan 5))
  (let ((pform (prime-form x)))
    (filter (lambda (g) (seq-eql (prime-form g) pform))
	    (embell-func x step sumspan))))

;; STRAVROT-FUNC -- 2nd generation stravrots
;; preserves ivec
(defun stravrot-func (chd)
  (set-difference
   (remove-duplicates
    (filter (lambda (x) (eql (length x) (length chd)))
	    (loop for sr in
		  (mapcar #'normal-form (stravrot chd 'nf))
		  append (mapcar #'normal-form (stravrot sr 'nf))))
    :test #'seq-eql)
   (list (normal-form chd))
   :test #'seq-eql))

;; TONNETZ-FUNC -- tonnetz (trichords only)
(defun tonnetz-func (chd)
  (if (tzworthy chd)
      (mapcar (lambda (x) (gen-tz x chd))
	      (if (eql (inverse-idx chd) 1)
		  (list 'lm 'pm 'rm)
		  (list 'ilm 'ipm 'irm)))
      (list chd)))

; T5-FUNC -- add 5 to each member
(defun t5-func (alist &optional (modlen 12))
  (loop for x in alist collect
	(substitute (mod (+ x 7) modlen) x alist)))

; PLACEDIST - 'swap distance' between two lists
; used in 'swap-path'
(defun placedist (list1 list2)
  (sum
   (loop for x in list1 collect
	 (abs (- (position x list1) (position x list2))))))

;; SWAP-PATH -- swapping places to change from one order to another
;; includes starting list, but not goal list
(defun swap-path (seq1 seq2)
  (let ((answer
         (a* seq1 seq2
             :Goal-Function #'seq-eql
             :Child-Generator-Function #'allswaps
             :H-Function #'placedist)))
    (if (eql (length answer) (length (flatten answer)))
        (list answer)
        (butlast answer))))

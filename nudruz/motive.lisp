
;; MOTIVE.LISP -- using codes to create "motive" structure

;; motive = (list codeword durs)
;; e.g. ((1 0 0 1) (.25 .25 .25 .25)) = (note rest rest note) 

;; utility to play motives
;; MOTIVEPLAY -- takes pits and motive (list code durs)
(defun motiveplay (pits mtiv)
      (splay (code->slots (first mtiv) pits) (second mtiv)))

;; QUANTIZE-MOTIVE -- quantized motive
(defun quantize-motive (mtiv &optional (divs '(2 3 4 5 6 8)))
  (let* ((qlevs (multquant divs))
	 (qdurs (quantdurs (second mtiv) qlevs 'give-poly))
	 (nudurs (first qdurs))
	 (pvec (second qdurs)))
    (list
     (make-poly (first mtiv) pvec)
     nudurs)))

;; COMBINE-MOTIVES -- [quantized] set of appended motives
(defun combine-motives (mtivs &optional (noquant 'nil) (divs '(2 3 4 5 6 8)))
  (let ((cmtives
	 (list
	  (loop for m in mtivs append (first m))
	  (loop for m in mtivs append (second m)))))
    (if noquant
	cmtives
	(quantize-motive cmtives divs))))

;; INVERSE-MOTIVE -- atx become rests, etc.
(defun inverse-motive (mpair)
  (list (inverse-codeword (first mpair)) (second mpair)))

;; MULTAUG-MOTIVE -- multiplicative augmentation
(defun multaug-motive (mpair mfact)
  (list (first mpair) (transp (second mpair) mfact #'*)))

;; ADDAUG-MOTIVE -- additive augmentation [adding (afact)*(fast-tact) to every dur]
;; (addaug-motive '((1 1) (.25 .75)) 2)
;; = ((1 1) (0.75 1.25)) ;; added 2 of the fast-tact [.25] to each member
(defun addaug-motive (mpair afact)
  (list (first mpair)
	(transp 
	 (transp (hits->ints (second mpair)) afact) 
	 (fast-tact (second mpair)) #'*)))

;; ADDAUGS -- lots of augmented motives
(defun addaugs (mtiv &optional (numfacts 6))
  (loop for a in (indices (+ 1 numfacts)) collect
	(addaug-motive mtiv a)))

;; ADDDUR-MOTIVE -- adding same 'literal' duration to each dur 
(defun adddur-motive (mpair adur)
  (list (first mpair) (transp (second mpair) adur)))

;; MOTIVE-DENSITY -- avg # atks per beat in a motive
(defun motive-density (mpair)
  (/ (count 1 (first mpair))
     (apply #'+ (second mpair))))

;; SUBMOTIVES -- drops atx from motive; returns list of all applicable mpairs
(defun submotives (mpair &optional (single-len nil))
  (let* ((atx (positions 1 (first mpair)))
	 (nuplaces 
	  (if single-len (subsets-len atx single-len)
	      (loop for n from 1 to (- (count 1 (first mpair)) 1) 
		    append (subsets-len atx n)))))
    (loop for p in nuplaces collect
	  (list (loop for n to (- (length (first mpair)) 1) 
		      collect (if (member n p) 1 0))
		(second mpair)))))

;; SUBCODES -- all derived codes less than original weight (or of specified weight)
(defun subcodes (a-code &optional (single-len nil))
  (first (transpose-matx
	  (submotives (list a-code (copylist (list .5) 12)) single-len))))

;; BASECHANGE-MOTIVE -- change fundamental 'fast-tact'; returns list of motives
;; sim. to Boulez "simple transformation"
(defun basechange-motive (mpair)
  (let* ((durs (second mpair))
	 (bdur (floats->rats (fast-tact durs)))
	 (hints (hits->ints durs))
	 (nubases (set-difference '(1/2 1/3 1/4 1/5 2/3 3/4 2/5 3/5 4/5)
				  (list bdur))))
    (mapcar 
     (lambda (x)
       (list (first mpair)
	     (transp hints x #'*)))
     nubases)))

; SIMPLIFIED-MOTIVE -- returns motive
;; removes initial rest & ties thru subsequent rests
(defun simplified-motive (mpair)
  (let* ((code (first mpair))
	 (durs (second mpair))
	 (truncode (member 1 code))
	 (truncdurs (subseq durs (position 1 code)))
	 (ties (second (slots->durs (code->slots truncode (makecyc (list 1)))))))
    (list (copylist '(1) (length ties))
	  (sum-across truncdurs ties))))

;; DURS->MOTIVE -- from durlist to motive (no rests)
(defun durs->motive (durs)
  (list (copylist '(1) (length durs))
	durs))

;; MOTIVE->DURS -- returns duration [from simplified version]
(defun motive->durs (mtiv)
  (second (simplified-motive mtiv)))

;; DEMULTIPLIED-MOTIVE
;; from Boulez -- recursively applied rhythm [returns single motive]
(defun demultiplied-motive (mpair)
  (let* ((sm (simplified-motive mpair))
	 (hits (first sm))
	 (durs (second sm))
	 (precombos
	  (loop for n to (- (length hits) 1) collect
		(list (first mpair) 
		      (transp (transp (second mpair) 
				      (/ 1 (apply #'+ (second mpair))) #'*)
			      (nth n durs) #'*))))
	 (outcodes (loop for p in precombos append (first p)))
	 (outdurs (loop for p in precombos append (second p))))
    (list outcodes outdurs))) 

;; MULTIPLIED-MOTIVE -- 'expanded' motive
(defun multiplied-motive (mpair)
  (let* ((sm (simplified-motive mpair))
	 (hits (first sm))
	 (durs (second sm))
	 (ft (fast-tact durs))
	 (hints (hits->ints durs))
	 (minhint (apply #'min hints))
	 (base0hints (transp hints (* -1 minhint))))
    (if (eql 2 (length (first sm))) 
	(list hits durs)
	(list (copylist hits (length hits))
	      (loop for n to (- (length hits) 1) append 
		    (transp
		     (transp base0hints (nth n hints))
		     ft #'*))))))

;; NONRET-MOTIVE? -- checks for nonretrogradability
(defun nonret-motive? (mpair)
  (and (seq-eql (first mpair) (reverse (first mpair)))
       (seq-eql (second mpair) (reverse (second mpair)))))

;; SPAWNED-MOTIVE
;; always returns 2 motives -- existing pairs are used to find proportions
(defun spawned-motive (mpair)
  (let* ((code (first mpair))
	 (durs (second mpair))
	 (firstdur (first durs))
	 (seconddur (second durs))
	 (lastdur (car (last durs)))
	 (secondlastdur (car (last (butlast durs))))
	 (spfact1 (floats->rats (/ firstdur lastdur)))
	 (spfact2 (/ 1 spfact1)))
    (list
     (list code (cons (* spfact1 secondlastdur) (butlast durs)))
     (list code (append (cdr durs) (list (* spfact2 seconddur)))))))
;; SELFSIM.LISP
;; making self-similar canons with cyclops.lisp and scf (nudruz.lisp)

;; Drew Krause, 2004
;; drkrause@mindspring.com
;; www.wordecho.org

(load "cyclops.lisp")

;; S-SIM -- defines a (motoric) midi selfsim stream
;; January 2006: ties used by default
;; [setting non-nil 'no-ties' will return repeated notes instead]
(defun s-sim (melody pits rate tlevel rpts 
                      &optional (basedur 1) (no-ties nil))
  (if no-ties
      (splay (copylist (scf (transpose pits tlevel) melody) rpts) 
             (* basedur rate))
      (let ((blist (scf (transpose pits tlevel) melody)))
        (splay (copylist (norpt blist) rpts)
               (copylist (transp (tievec blist) (* basedur rate) #'*)
                         rpts)))))

;; SELFSIM -- constructs a set of midi selfsim streams
(defun selfsim (melody pits rates transpvec 
                       &optional (basedur 1) (no-ties nil))
  (let ((rptlen (reduce #'lcm rates)))
    (loop for n to (- (length rates) 1) collect
          (s-sim melody pits (nth n rates) (nth n transpvec)
                 (/ rptlen (nth n rates)) basedur no-ties))))

;; example
;(events (selfsim cyclops3p5x14 '(60 62 63 65) '(1 3 5) '(0 -7 -14) .25)
;        "newselfsim.midi")

;; S-SIM->SLOTS -- defines a (slotted) selfsim stream
;; ... like s-sim, but makes slot vector
(defun s-sim->slots (melody pits rate transp rpts)
  (let ((durs (loop repeat (length melody) collect rate))
        (scfmel (scf pits melody)))
    (flatten (loop repeat rpts collect
          (durs->slots
           (flatten
            (mapcar (lambda (x) (+ x transp)) scfmel))
           (flatten durs))))))

;; SELFSIMVEC -- constructs slotted selfsim pitch list
(defun selfsimvec (melody pits rates transpvec)
  (let ((rptlen (reduce #'lcm rates)))
    (merge-slots (loop for n to (- (length rates) 1) collect
          (s-sim->slots melody pits (nth n rates) (nth n transpvec)
                 (/ rptlen (nth n rates)))))))

;; SSIM-ATKLEN -- computes length of ssim vector, from mel & rates
(defun ssim-atklen (mel rates)
  (* (length mel)  
     (apply #'lcm rates)))

;; SSIM-TOTALEN -- computes total length of ssim in minutes
(defun ssim-totalen (mel rates &optional (basedur .25))
  (round
   (/ (ssim-atklen mel rates)
      (* (/ 1 basedur) 60))))

;; SSIM? -- testing self-similarity
;; rests allowed through [June 2010]
(defun ssim? (mel rate)
  (let ((lenmel (length mel)))
    (seq-eql-with-rests mel
	     (butlast (loop for n to (* rate lenmel) by rate collect 
			    (nth (mod n lenmel) mel))))))

;; SSIM-SPEEDS -- returns all rates of self-replication (up to length)
(defun ssim-speeds (mel)
  (no-nils
   (loop for n from 2 to (- (length mel) 1) collect
	 (if (ssim? mel n) n))))

;; TIES? -- does a melody contain ties?
(defun ties? (mel)
  (member 't 
	  (map 'list #'eql mel (cdr mel))))


; row-adjacencies

;; Jan 2012: autosimilar melodies (Amiot)

;; ratio = augmentation
;; period = length of melody
;; ratio & period must be coprime
(defun coprime? (num1 num2)
  (not (intersection (prime-factors num1) (prime-factors num2)))) 

(defun merge-one-element (elem partial-results)
           (unless (do* ((e partial-results (cdr e))
                         (a (car e) (car e)))
                        ((null e) nil)
                     (when (intersection elem a)
                       (setf (car e) (union a elem))
                       (return partial-results)))
             (push elem partial-results))
           partial-results)

(defun disjoint-sets (sets &key (test #'eql))
  (let ((disj-sets-hash (make-hash-table :test test)))
    (labels ((get-set (elem)
               ;; get the set/partition that the element belongs to
               ;; creating a new one if necessary
               (let ((partition (gethash elem disj-sets-hash)))
                 (or partition (setf (gethash elem disj-sets-hash)
                                     (cons (cons elem nil) nil)))))
             (merge-set (from-set to-set)
               ;; migrate all members of from-set to to-set
               ;; and update their (get-set ...) pointer to the new set also.
               (unless (eq from-set to-set)
                 (dolist (each-elem (car from-set))
                   (setf (gethash each-elem disj-sets-hash) to-set)) 
                 (setf (car to-set) (union (car from-set) (car to-set))))))
      (dolist (set sets)
        (let ((fs (get-set (first set))))
          (dolist (elem (rest set))
            (merge-set (get-set elem) fs))))
      (loop for x being the hash-values of disj-sets-hash
            collecting x into partitions
            finally (return (mapcar #'car (remove-duplicates partitions)))))))

;; AUTOSIM-PRIMITIVE 
;; gives indx structure from ratio & period
(defun autosim-primitive (ratio period)
  (if (coprime? ratio period)
      (let* ((placelists
	      (mapcar #'safesort
		      (disjoint-sets
		       (sort 
			(mapcar #'safesort
				(remove-duplicates 
				 (mapcar
				  (lambda (x) 
				    (remove-duplicates 
				     (list x (mod (* x ratio) period))))
				  (indices period))
				 :test #'list-eql))
			(lambda (a b) (< (car a) (car b))))))))
	(flatten
	 (merge-slots
	  (map 'list
	       (lambda (n plc)
		 (tpoints (copylist (list n) (length plc)) plc period))
	       (indices (length placelists))
	       placelists))))))

;; RPTS->DURS -- gives pits & repeat vector
(defun rpts->durs (inputvec)
  (list (norpt inputvec)
	(take-poly (gather-pits #'eql inputvec))))

;; MELDURS->RPTS
(defun meldurs->rpts (meldurs)
  (repeater (first meldurs) (second meldurs)))

;; CANNED-FRAGS
;; all selfsim canons (slotted list) based on frags of len
(defun canned-frags (pits durs fraglen &optional (transpdown 7) (removedupes? 't) (flat? t))
  (let* ((infrags
	  (make-poly (durs->slots pits (hits->ints durs)) fraglen))
	 (hyfrags (if removedupes? (remove-duplicates infrags :test #'seq-eql) infrags)) 
	 (hyspeeds (mapcar #'ssim-speeds hyfrags))
	 (rawoutput
	  (map 'list
	       (lambda (frag speed)
		 (if speed
		     (let ((thislcm (reduce #'lcm speed)))
		       (merge-slots
			(cons
			 (copylist frag thislcm)
			 (loop for spidx from 0 to (- (length speed) 1) collect
			       (copylist 
				(menses (transp frag (* (* -1 transpdown) (+ 1 spidx))) (nth spidx speed))
				(/ thislcm (nth spidx speed)))))))))
	       hyfrags
	       hyspeeds)))
    (if flat? (flatter rawoutput) (no-nils rawoutput))))

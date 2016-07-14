;; NUDRUZ.LISP

;; Drew Krause, 2004
;; drkrause@mindspring.com
;; www.wordecho.org
;; Michael Gogins, 2016

(load (merge-pathnames "cminit.lisp" *nudruz-home*))

(in-package :cm)

;(load "data/partition-table.lisp")

;; first, some needed basic stuff

;; FLATTEN -- removes all nesting in list
;; "thank you Paul Graham!"
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;; FLATTER -- removes one level of tree (preserves sublists)
(defun flatter (alist)
  (loop for a in alist append a))

;; FLATP -- tests if a list is flat
(defun flatp (inlist)
  (eql (length inlist)
       (length (flatten inlist))))

;; NESTP -- tests if list is more than 1d
(defun nestp (inlist)
  (not (eql (length inlist)
	    (length (flatten inlist)))))

;; LIKE-FLAT -- converts all 1-lists to atoms; leaves the rest alone
;; August 2006
(defun like-flat (alist)
  (mapcar (lambda (x) (if (and (listp x) (eql 1 (length x))) 
			  (car x)
			  x))
	  alist))

;; NOT-FLAT -- makes everything into a list; leaves lists alone
;; (not-flat '(2 4 (3 4) 2 (2 1 4))) = ((2) (4) (3 4) (2) (2 1 4))
;; removing rests in chords (May 2007) 
;; makes atoms into lists (May 2009)
(defun not-flat (alist)
  (if (listp alist)
      (mapcar (lambda (x) (if (listp x) (norests x) (list x)))
	      alist)
      (list alist)))

;; PAIRLISTFUNC -- apply  binary function to (a b) list 
;; added June 2008
;; (mapcar (pairlistfunc abs-intv) (make-poly (heapvec 12) 2)) = (3 5 3 2 2 3)
(defmacro pairlistfunc (binaryfunc)
`(lambda (x) (,binaryfunc (first x) (second x))))

;; DEEPFUNC -- applies function across all members of a tree
;; added June 2007
(defun deepfunc (func alist)
  (if (> (apply #'max (take-poly alist)) 1)
      (mapcar (lambda (x) (deepfunc func x)) (not-flat alist))
      (mapcar func alist)))

;; ARRAY->LIST -- converts array to list (of lists)
(defun array->list (ary)
  (loop for x to (- (nth 0 (array-dimensions ary)) 1)
        collect
        (loop for y to (- (nth 1 (array-dimensions ary)) 1)
              collect (aref ary x y))))

;; VEC->LIST -- converts simple vector to a list
;; added July 2005
(defun vec->list (avec)
    (loop for x to (- (array-total-size avec) 1) collect
          (svref avec x)))

;; ROUND-ALL -- round all in list
(defun round-all (reals)
  (if (numberp reals) 
      (round reals)
      (mapcar #'round-all reals)))

(defun listmax (alist)
  (apply #'max alist))

(defun listmin (alist)
  (apply #'min alist))

;; read/write external files

;; MAKE-LIST-FROM-FILE -- reads in file & converts each line into sublist
;; thank you comp.lang.lisp!
(defun make-list-from-file (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (read-from-string (concatenate 'string "(" line ")")))))

;; WRITE-OUPUT-TO-FILE -- wrtes lisp output to a (new) ascii file
(defun write-output-to-file (lispoutput filename)
  (with-open-file (stream filename :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
		  (loop for i in lispoutput
			do 
			(format stream "~a " i))))

;; patterns

;; CONVERT-INTO-PATT -- make everything into a pattern (for velocity)
(defun convert-into-patt (avalue)
  (if (pattern? avalue) avalue
      (makecyc 
      (if (listp avalue) avalue (list avalue)))))

;; MAKEPATT -- nicer name for 'convert-into-patt'
(defun makepatt (x) (convert-into-patt x))

;; CONVERT-INTO-RANDPATT -- make everything into a nonrepeating 'weighting' 
(defun convert-into-randpatt (avalue)
  (if (pattern? avalue) avalue
      (new weighting :of 
	   (norpt-rand (if (listp avalue) avalue (list avalue))))))

;;; LIST-EQL: whether two lists have equal contents, irrespective of order
;; (list-eql '(1 2 3) '(1 3 2)) = T
(defun list-eql (list1 list2)
  (and (subsetp list1 list2) 
       (subsetp list2 list1)
       (= (length list1) (length list2))))

;; SEQ-EQL: whether two lists are identical
;; (seq-eql '(1 2 3) '(1 3 2)) = nil
;; (seq-eql '(1 2) '(1 2)) = T
(defun seq-eql (list1 list2)
  (not (mismatch list1 list2)))

;; SEQ-EQL-WITH-RESTS: whether two lists are identical -- rests ignored!
;; useful for 'ssim'
(defun seq-eql-with-rests (list1 list2)
  (not
   (member nil
	   (map 'list
		(lambda (a b) (or
			       (symbolp a)
			       (symbolp b)
			       (eql a b)))
		list1 list2))))

;; NOT-MEL -- slotwise melodic complement
;; returns slots of 'bigmel' that don't include 'smallmel'
;; (not-mel '(0 1 (2 5) 3 (4 6)) (indices 5)) = (R R (5) R (6))
(defun not-mel (bigmel smallmel)
  (map 'list
       (lambda (a b)
	 (let ((ndiff (set-difference a b)))
	   (if ndiff ndiff 'r)))
       (not-flat bigmel)
       (not-flat smallmel)))

;; TRANSPOSE-MATX -- simple matrix transposition
; (transpose-matx '((1 2 3) (4 5 6))) = ((1 4) (2 5) (3 6))
(defun transpose-matx (matx)
  (loop for n to (- (length (car matx)) 1) collect
	(mapcar (lambda (x) (nth n x)) matx)))

;; DISTRIBUTE -- take list and distribute it (in order) among # lists
(defun distribute (vec listnum)
  (mapcar #'no-nils
	  (transpose-matx
	   (make-poly vec listnum))))

;; ORDERED-MERGE: combines lists into large list randomly, but keeping each internal seq
;; somewhat opposite to 'distribute'
(defun ordered-merge (inlists)
  (let* ((idxvec
	  (shuffle
	   (repeater (indices (length inlists)) (take-poly inlists)))))
    (mapcar 
     (lambda (x) (pop (nth x inlists)))
     idxvec)))

;; IS-PALINDROME: tests a list
;; (is-palindrome '(1 2 3 2 1)) = T
(defun is-palindrome (alist)
  (seq-eql alist (reverse alist)))

;; SAFESORT -- non-destructive sort
(defun safesort (a-list)
  (let ((templist (loop for a in a-list collect a)))
    (sort templist #'<)))

;; SORT-EVERY -- sort every entry in a list of lists
(defun sort-every (alist)
  (loop for x in alist collect
        (cond ((listp x) (safesort x))
              (t x))))

;; NO-NILS -- removes all instances of 'nil from a list
;; fixed 'reverse' bug April 2006
(defun no-nils (a-list)
  (reverse
   (set-difference a-list '(nil))))

;; NORESTS -- removes rests
(defun norests (alist)
   (no-nils (loop for x in alist collect
                  (if (not (eql x 'r)) x))))

; SUM -- add all in list
(defun sum (x)
  (apply #'+ x))

;; PRIME-FACTORS -- nonrepeating list of all prime factors
(defun prime-factors (n)
  (rsm.mod:factors n))

;; PRIMEP -- prime test
(defun primep (n)
  (eql 1 (length (prime-factors n))))

;; REL-PRIME? -- tests if two #s are relatively prime
(defun rel-prime? (a b)
    (eql 1 (gcd a b)))

; FACTORIAL 
(defun factorial (anum)
  (if (eql 1 anum) 1
      (* anum (factorial (- anum 1)))))

;; MULTIEVENT -- LETS YOU WRITE OUT POLYPHONY
(defun multievent (class arg &rest args)
  (let ((vals (getf args arg)))
    (if (listp vals)
      (loop for k in vals
            collect
            (apply #'make-instance class arg k args))
      (apply #'make-instance class arg vals args))))

;; multievent example
;(defun multithing
;  (process repeat 20
;        output (multievent 'midi :keynum
;                             :keynum (next pcycle)
;                             :time (now)
;                             :duration 1)
;        wait 1))

;; MIDI-IN -- creates (pits durs) two-element list of lists from midifile
;; note: gives same duration to simultaneous atx (legato to next atk)
(defun midi-in (midifile)
  (let ((mytracks (import-events midifile :meta-exclude t))
        (pits '())
        (starts '()))
    (map-objects #'(lambda (x) (push x pits)) mytracks 
                 :slot 'keynum :class 'midi)
    (map-objects #'(lambda (x) (push x starts)) mytracks 
                 :slot 'time :class 'midi)
    (list 
     (first (combine-pits (nreverse pits) (reverse starts)))
     (append    
      (melint (reverse (remove-duplicates starts))) (list 1.0)))))

;; MIDI-IN1 -- multichannel version of midi-in (midifile type 1)
;; note: gives same duration to simultaneous atx (legato to next atk)
(defun midi-in1 (midifile)
  (filter #'car
	  (loop for mytracks in 
		(import-events midifile :meta-exclude t)
		collect
		(let ((pits '())
		      (starts '()))
		  (map-objects #'(lambda (x) (push x pits)) mytracks 
			       :slot 'keynum :class 'midi)
		  (map-objects #'(lambda (x) (push x starts)) mytracks 
			       :slot 'time :class 'midi)
		  (list 
		   (first (combine-pits (nreverse pits) (reverse starts)))
		   (append    
		    (melint (reverse (remove-duplicates starts))) 
		    (list 1.0)))))))

;; NUMIDI-IN 
; -- creates (pits durs atx) three-element list of lists from midifile
;; USES RESTS! IE NON-LEGATO
(defun numidi-in (midifile)
  (let ((mytracks (import-events midifile :meta-exclude t))
        (pits '())
        (starts '())
	(durs '()))
    (map-objects #'(lambda (x) (push x pits)) mytracks 
                 :slot 'keynum :class 'midi)
    (map-objects #'(lambda (x) (push x starts)) mytracks 
                 :slot 'time :class 'midi)
    (map-objects #'(lambda (x) (push x durs)) mytracks 
                 :slot 'duration :class 'midi)
    (list 
     (first (combine-pits (nreverse pits) (reverse starts)))
     (mapcar (lambda (x) (apply #'min x))
	     (not-flat
	      (first (combine-pits (nreverse durs) (reverse starts)))))
     (reverse (remove-duplicates starts)))))

;; NUMIDI-IN1 -- multichannel version of 'numidi-in' (midifile type 1) 
; -- creates (pits durs atx) three-element list of lists from midifile
;; USES RESTS! IE NON-LEGATO
(defun numidi-in1 (midifile)
  (filter #'car
	  (loop for mytracks in
		(import-events midifile :meta-exclude t)
		collect
		(let ((pits '())
		      (starts '())
		      (durs '()))
		  (map-objects #'(lambda (x) (push x pits)) mytracks 
			       :slot 'keynum :class 'midi)
		  (map-objects #'(lambda (x) (push x starts)) mytracks 
			       :slot 'time :class 'midi)
		  (map-objects #'(lambda (x) (push x durs)) mytracks 
			       :slot 'duration :class 'midi)
		  (list 
		   (first (combine-pits (nreverse pits) (reverse starts)))
		   (mapcar (lambda (x) (apply #'min x))
			   (not-flat
			    (first (combine-pits (nreverse durs) (reverse starts)))))
		   (reverse (remove-duplicates starts)))))))

;; NUMIDI-VIN 
; -- creates (pits durs atx vels) four-element list of lists from midifile
;; USES RESTS! IE NON-LEGATO
(defun numidi-vin (midifile)
  (let ((mytracks (import-events midifile :meta-exclude t))
        (pits '())
        (starts '())
	(durs '())
	(vels '()))
    (map-objects #'(lambda (x) (push x pits)) mytracks 
                 :slot 'keynum :class 'midi)
    (map-objects #'(lambda (x) (push x starts)) mytracks 
                 :slot 'time :class 'midi)
    (map-objects #'(lambda (x) (push x durs)) mytracks 
                 :slot 'duration :class 'midi)
    (map-objects #'(lambda (x) (push x vels)) mytracks 
                 :slot 'amplitude :class 'midi)
    (list 
     (first (combine-pits (nreverse pits) (reverse starts)))
     (mapcar (lambda (x) (apply #'min x))
	     (not-flat
	      (first (combine-pits (nreverse durs) (reverse starts)))))
     (reverse (remove-duplicates starts))
     (reverse vels))))

;; NUMIDI-VIN1 -- multichannel version of 'numidi-vin1' (midifile type 1) 
; -- creates (pits durs atx vels) four-element list of lists from midifile
;; USES RESTS! IE NON-LEGATO
(defun numidi-vin1 (midifile)
  (filter #'car
	  (loop for mytracks in (import-events midifile :meta-exclude t)
		collect
		(let ((pits '())
		      (starts '())
		      (durs '())
		      (vels '()))
		  (map-objects #'(lambda (x) (push x pits)) mytracks 
			       :slot 'keynum :class 'midi)
		  (map-objects #'(lambda (x) (push x starts)) mytracks 
			       :slot 'time :class 'midi)
		  (map-objects #'(lambda (x) (push x durs)) mytracks 
			       :slot 'duration :class 'midi)
		  (map-objects #'(lambda (x) (push x vels)) mytracks 
			       :slot 'amplitude :class 'midi)
		  (list 
		   (first (combine-pits (nreverse pits) (reverse starts)))
		   (mapcar (lambda (x) (apply #'min x))
			   (not-flat
			    (first (combine-pits (nreverse durs) (reverse starts)))))
		   (reverse (remove-duplicates starts))
		   (reverse vels))))))

;; DRUMIDI-IN -- parses midi drum track
;; for passing to multichan csound
(defun drumidi-in (midifile)
  (let* ((min (midi-in midifile))
	 (minpits (first min))
	 (nfminpits (not-flat minpits))
	 (mindurs (second min))
	 (pitvals
	  (remove-duplicates (flatten minpits))))
    (loop for p in pitvals collect
	  (let* ((thesepits 
		  (mapcar
		   (lambda (x) (if (member p x) p 'r))
		   nfminpits))
		 (sdurs (slots->durs thesepits))
		 (outdurs (sum-across mindurs (second sdurs))))
	    (if (numberp (car (first sdurs)))
		(list (first sdurs)
		      outdurs
		      (butlast (melint->line 0 outdurs)))
		(list (cdr (first sdurs))
		      (cdr outdurs)
		      (cdr (butlast (melint->line 0 outdurs)))))))))

;; MIDI-TOTALEN -- midifile length in minutes 
(defun midi-totalen (mfile)
  (round
   (/ 
    (apply #'+ (second (midi-in mfile)))
    60)))

;; SPLAY -- the simplest: plays pits/durs
;; durs may be list, pattern, or simple value
;; added optional channel argument Nov. 2005
;; channel may be integer, list, or pattern (July 2008)
(defun splay (inpits indurs &optional (chans 0))
  (let* ((pl (plists inpits indurs))
         (pits (first pl))
         (durs (second pl))
	 (chanpatt (if (pattern? chans) chans (makecyc chans)))) 
    (process for x in pits
             for dur in durs
	     for chan in (next chanpatt (length pits))
             output (multievent 'midi :keynum
                                :channel chan
                                :keynum x
                                :time (now)
                                :duration dur)
             wait dur)))

;; PLAYMIDS -- combine midi files
(defun playmids (&rest names)
  (loop for nam in names collect
	(splay (first (midi-in nam)) (second (midi-in nam)))))

;; CSOUND -- defining basic event
(defobject i1 (i)
  (amp
   freq)
  (:parameters time dur amp freq))

;; SIMPL4SCO -- generic playback routine
(defun simpl-for-sco (pits durs atkpts)
  (process for x in pits
           for dur in durs
           for atk in atkpts
           when (or (numberp x) (listp x))
           output (multievent 'i1 :freq
                              :freq (hertz x)
                              :time (float atk)
                              :amp (between .15 .2)
                              :dur (float dur))))

;; MULTIPLE-ASSOC -- utility for 'combine-pits'
;; Bob Coyne contributes:
(defun multiple-assoc (thing alist)
  (loop for i in alist
        when (eql (first i) thing)
        append (cdr i)))

;; COMBINE-PITS -- coalesce polyphony into sublists & sort timepoints
;; (combine-pits '(10 20 30 40 50 60) '(6 2 3 2 1 2))
;; = ((50 (20 40 60) 30 10) (1 2 3 6))
(defun combine-pits (pitlist tplist)
  (let* ((combo-list (loop for tp in tplist
                           for val in pitlist
                           collect (list tp val)))
         (processed-list 
          (sort (loop for i in (remove-duplicates tplist)
                      collect (cons i (multiple-assoc i combo-list)))
                #'< :key #'first)))
    (list (mapcar #'(lambda (list) 
                      (if (= (length list) 2)
                        (second list) (rest list)))
                  processed-list)
          (mapcar #'first processed-list))))

;;; general combinatorics

;; PROPER-SUBSETS
;; power set of a list, minus trivial (empty & complete) sets
(defun proper-subsets (a-list)
    (butlast 
     (set-difference (cllib:subsets a-list) (list a-list 'nil))))

;; SUBSETS-LEN
;; all subsets of specified size
;; (subsets-len '(1 2 3 4) 2) = ((3 4) (2 4) (2 3) (1 4) (1 3) (1 2))
;; now takes lists of lengths [Nov. 2005]
(defun subsets-len (a-list len)
  (if (numberp len)
      (set-difference
       (mapcar (lambda (x) (if (= (length x) len) x)) 
               (cllib:subsets (remove-duplicates a-list)))
       (list 'nil))
      (loop while len append (subsets-len a-list (pop len)))))

;; SUBSEQUENCES -- returns list of 
;; all subsequences with length 'subseqlen' 
;; (subsequences '(1 2 3 4) 2) = ((1 2) (2 3) (3 4))
;; now takes lists of lengths [Nov. 2005]
(defun subsequences (a-list subseqlen)
  (if (numberp subseqlen) 
      (if (< subseqlen (length a-list))
          (loop for n to (- (length a-list) subseqlen)
                collect 
                (loop for y from n to (+ n (- subseqlen 1))
                      collect (nth y a-list)))
          a-list)
      (loop while subseqlen append (subsequences a-list (pop subseqlen)))))

;; ALL-SUBSEQUENCES
;; returns list of all but trivial (complete & atomic) subsequences 
(defun all-subsequences (a-list)
  (loop for n from 2 to (- (length a-list) 1)
        append (subsequences a-list n)))

;; SEG-WITH-PIT -- returns (random) sublist containing value
(defun seg-with-pit (pit alist seglen)
  (if (member pit alist)
      (pickl
       (filter (lambda (x) (member pit x))
	       (subsequences alist seglen)))))

;; ALL-PARTITIONS -- all ordered partitions for length 1-20
;; 'nontrivial' flag omits atoms & complete list
;; added Jan. 2012
(defun all-partitions (a-length &optional (nontrivial nil))
  (let ((rawlist 	  (nth a-length partition-table)))
    (if nontrivial
	(filter (lambda (x) (and (not (member 1 x)) 
				 (not (member a-length x))))
		rawlist)
	rawlist)))

;; LIST-PARTITIONS -- partitioning a list
;; 'nontrivial' flag omits atoms & complete list
;; added Jan. 2012
(defun list-partitions (alist &optional (nontrivial nil))
  (mapcar (lambda (x) (make-poly alist x))
	  (all-partitions (length alist) nontrivial)))

;; PERMUTATIONS --  returns all permutations of a list
(defun permutations (a-list)
  (loop for x in 
        (cllib:permutations-list (make-array (length a-list) 
                                             :initial-contents a-list))
        collect (coerce x 'list)))

;; PERMUTELIST -- permutes a-list by 'aperm' list of indices
;; ;; (permutelist '(3 1 2 0) '(7 8 9 10)) = (10 8 9 7)
(defun permutelist (aperm amel)
  (map 'list (lambda (x) (nth x amel)) aperm))

;; ROTATE-LIST -- rotates list by "factor"
(defun rotate-list  (alist &optional (factor 1))
  (append (subseq alist factor (length alist))
	  (subseq alist 0 factor)))

;; ROTATIONS -- set of rotations (not flat)
(defun rotations (alist rots)
  (loop for r in rots collect (rotate-list alist r)))

;; ALLROTS -- returns list of all rotations of list
;; (allrots '(58 60 54)) = ((58 60 54) (60 54 58) (54 58 60))
;; fixed May 2009 
(defun allrots (alist)
  (mapcar 
   (lambda (x) (rotate-list alist x))
   (indices (length alist))))

;; ALLSWAPS -- all pairwise 'place swaps' (permutations) in a list
(defun allswaps (alist)
  (let ((polys (cons 2 
		     (copylist (list 1) (- (length alist) 2)))))
    (loop for p in (allrots polys) collect
	  (flatten
	   (mapcar 
	    (lambda (x) (if (numberp x) x (reverse x)))
	    (make-poly alist p))))))

;; HOLES -- gaps in list of integers
(defun holes (alist)
  (no-nils
  (loop for x from (reduce #'min alist) to (reduce #'max alist) collect
        (if (not (member x alist)) x))))

;; NTHS -- returns values corresp. to list of positions
;; (nths '(2 3 5) (indices 12 20)) = (22 23 25)
;; added Sept. 2008
;; allows recursion - Oct. 2009
(defun nths (poslist biglist)
  (mapcar (lambda (x) 
	    (if (listp x) (nths x biglist) (nth x biglist))) 
	  poslist))

;; TAKE-PLACES -- returns positions of each value in a list
;; (take-places '(17 13 14) (indices 12 10)) = (7 3 4)
;; added Sept. 2008
(defun take-places (valist biglist)
  (mapcar (lambda (x) (position x biglist)) 
	  valist))

;; LIST-COMPLEMENT -- elements of 'smallist' not found in 'biglist'
(defun list-complement (biglist smallist)
  (reverse (set-difference biglist smallist)))

;; PIX -- selecting subset of list
(defun pix (len alist)
  (chooser (heapvec len (length alist)) alist))

;; SORT-TO-OTHER 
;; returns elements of list1 in same order as list2 
;; (sort-to-other '(2 3 4) '(0 1 3 2 7 10 8 4 11 5 9 6)) = (3 2 4)
(defun sort-to-other (sublist superlist)
  (mapcar (lambda (x) (nth x superlist))
	  (safesort (take-places sublist superlist))))

;; TRANSPAIR
;; stepwise expansion/contraction between superset & subset (May 2009)
;; always sorts to the order in larger list
;; (transpair '(2 1) (indices 5))
;; = ((2 1) (0 1 2) (0 1 2 4) (0 1 2 3 4))
(defun transpair (list1 list2)
  (let* ((growflag (< (length list1) (length list2)))
	 (sub (if growflag list1 list2))
	 (super (if growflag list2 list1))
	 (sdiff
	  (shuffle (set-difference super sub)))
	 (growlists			; intermediate lists
	  (mapcar
	   (lambda (x) (sort-to-other (append sub x) super))
	   (butlast (reverse (thinout sdiff 'nest))))))
    (if growflag
	(append (list sub) growlists (list super))
	(append (list super) (reverse growlists) (list sub)))))

;; TRANSLISTS -- does 'transpair' on a list of lists (May 2009)
;; returns first, last, & all transition lists
(defun translists (tvec)
  (append
   (flatter
    (mapcar #'butlast
	    (map 'list #'transpair tvec (cdr tvec))))
   (last tvec)))

;; serial list utilities

;; INVERSION -- simple (mod 12 or other) inversion function
;; added May 2009
(defun inversion (input &optional (modlen 12))
  (let ((nums (if (listp input) input (list input))))
    (mapcar (lambda (x) (mod (- modlen x) modlen)) nums)))

;; LISTINV -- inverse of a list, by sort order
(defun listinv (a-list)
  (let ((slist (safesort a-list)))
    (loop for x in a-list collect
          (nth (- (- (length slist) 1) (position x slist)) slist))))

;; PFORMS -- all canonical transformations of a set (w/ duplicates removed)
(defun pforms (aset)
  (let ((aset '(3 5 6)))
    (mapcar #'normal-form
	    (remove-duplicates
	     (append (alltransp aset) (alltransp (inversion aset)))
	     :test #'list-eql))))

; DSYM -- degree of symmetry of pcset
(defun dsym (aset)
  (length (no-nils
	   (mapcar (lambda (x) (list-eql x aset))
		   (append (alltransp aset) (alltransp (inversion aset)))))))

;; ALLROWS-BYTYPE -- returns list of inverted transformed lists
;; trtype can be 'p, 'i, 'r, or 'ri
;; 'transposes' elements by sorted level (with wraparound)
;; returns list of lists
;; note: inversion is a peculiar (rotational) application
;; use 'invert' for classical inversion function
(defun allrows-bytype (a-list trtype)
  (let ((slist (safesort (remove-duplicates a-list))))
    (loop for y to (- (length slist) 1) collect
          (loop for x in 
                (case trtype 
                  (p a-list)
                  (i (listinv a-list))
                  (r (reverse a-list))
                  (ri (reverse (listinv a-list))))
                collect 
                (nth (mod (+ y (position x slist)) (length slist)) slist)))))

;; ROWPERMS-BYTYPE -- list 'boulez-style' permutations of a list 
;; (list of lists)
;; returning indices of original list
(defun rowperms-bytype (a-list trtype)
  (loop for tlist in (allrow-bytype a-list trtype) collect
        (loop for y in tlist collect (position y a-list)))) 

;; BASICROWS -- P, I, R, RI
(defun basicrows (arow &optional (modnum 12))
  (let ((invs (inversion arow modnum)))
    (list
     arow
     invs
     (reverse arow)
     (reverse invs))))

;; ALLROWS -- shuffled no-dupe list of all (p,i,r,ri) transforms
;; rebuilt from scratch June 2008
(defun allrows (alist &optional (modnum 12))
  (let ((invs (mapcar (lambda (x) (- modnum x)) alist)))
    (shuffle
     (remove-duplicates
      (loop for rowform in 
	    (list alist 
		  (reverse alist)
		  invs
		  (reverse invs))
	    append (alltransp rowform modnum))
      :test #'seq-eql))))

;; M5 -- 'm' transformation = (row * 5 mod 12)
(defun m5 (alist)
  (mod12 (transp alist 5 #'*)))

;; M7 -- 'm' transformation = (row * 7 mod 12)
(defun m7 (alist)
  (mod12 (transp alist 7 #'*)))

;; GRANDROWS -- 'grand' row matrix
;; classical row forms + rotations, reverse rotations
(defun grandrows (myrow &optional (modlen 12))
  (remove-duplicates 
   (flatter
    (mapcar (lambda (x) (allrows x modlen))
	    (append (allrots myrow) (allrots (reverse myrow)))))
   :test #'seq-eql))

;; TIGHTROWS -- row forms with common segments of specified length (default 4)
;; from either classical 'allrows' or grand 'grandrows'
(defun tightrows (row &optional (modnum 12) (grandflag nil) (seglen 4))
  (let ((sseq (subsequences row seglen)))
    (remove-duplicates 
     (cons row
	   (filter (lambda (x) (intersection
				sseq
				(subsequences x seglen)
				:test #'list-eql))
		   (if grandflag (grandrows row modnum) (allrows row modnum))))
     :test #'seq-eql)))

;; ALLROWPERMS -- returns shuffled no-dupe list of all (p,i,r,ri) permutations
;; returning indices of original list
(defun allrowperms (a-list)
  (shuffle
   (remove-duplicates
    (loop for x in '(p i ri r) append 
          (rowperms-bytype a-list x))
    :test (lambda (x y) (not (mismatch x y))))))

;; APPLY-PERM -- apply position-ordered permutation
;; (apply-perm '(1 3 0 2 0) '(35 51 2 .5)) =  (51 0.5 35 2 35)
(defun apply-perm (permlist a-list)
  (loop for x in permlist collect (nth x a-list)))

;; texture stuff

;; CHOP-LIST -- divide list into # equal sublists w/o remainder
;; added Feb 2012
(defun chop-list (alist chops)
  (make-poly alist (floor (/ (length alist) chops)) 'fit))

;; MAKE-POLY -- distributes line according to texture vector
;; (make-poly '(1 2 3 4 5 6) '(1 0 2)) = ((1) (R) (2 3) (4) (R) (5 6))
;; changed June 2005
;; Nov. 2005: added 0->R functionality
;; Aug. 2006: using 'likeflat' for better output
;; May 2009: 'fitflag'=t ends with complete texture, ie no partial textures
(defun make-poly (mel texture &optional (fitflag nil))
  (like-flat
   (if fitflag 
       (let* ((mel-len (length mel))
	      (txtlist
	       (copylist texture
			 (floor (/ mel-len 
				   (apply #'+ 
					  (flatten (not-flat texture))))))))
	 (loop for txt in txtlist
	       collect (no-nils
			(if (eql txt 0) (list 'r)
			    (loop repeat txt collect (pop mel))))))
       (let ((txtcyc (new cycle of texture)))
	 (loop while mel 
	   collect (no-nils
		    (let ((nxt-txt (next txtcyc)))
		      (if (eql nxt-txt 0) (list 'r)
			  (loop repeat nxt-txt collect (pop mel))))))))))

;; TAKE-POLY -- gives length of each event in list
;; (take-poly '((55 4 6) 2 (R) 43 (2 1) 23)) =  (3 1 0 1 2 1)
;; Nov. 2005: added R->0 functionality
(defun take-poly (a-list)
  (loop for x in a-list collect
        (cond ((or (eql x 'r) (and (listp x) (member 'r x))) 0)
              ((numberp x) 1)
              (t (length x)))))

; MERGE-POLY -- use poly vector to merge melodies [Aug. 2006]
;; 'mels' is a list of lists [melodies]
(defun merge-poly (mels texture)
  (like-flat
   (let ((melcycs (mapcar (lambda (x) (makecyc x)) mels))
	 (txtcyc (new cycle of texture)))
     (loop until (some #'eop? melcycs) 
       collect (no-nils 
		(let ((nxt-txt (next txtcyc)))
		  (if (eql nxt-txt 0) (list 'r)
		      (loop for n in (indices nxt-txt) collect 
			    (next (nth n melcycs))))))))))

;; MOD12 -- returns number or list, mod 12 [old function]
;; (mod12 '(23 36)) = (11 0)
(defun mod12 (input)
  (cond ((eql input 'r) 'r)
        ((numberp input) (mod input 12))
	(t (mapcar #'mod12 input))))

;; MODLIST -- returns number or list, mod N [preferred]
(defun modlist (input &optional (modlen 12))
  (cond ((eql input 'r) 'r)
        ((numberp input) (mod input modlen))
	(t (mapcar #'(lambda (x) (modlist x modlen)) input))))

;;INTV -- interval between two notes (simple difference)
;; can be positive or negative
;; (intv 11 17) = 6
(defun intv (x y)
	(- y x))

;;ABS-INTV -- absolute mod-x difference between 2 notes
;; (abs-intv 20 4) = 4
(defun abs-intv (x y &optional (modlen 12))
	(mod (abs (- y x)) modlen))

;;MOD-INTV -- *closest* mod-x difference between 2 notes
;; (mod-intv 11 0) = 1
(defun mod-intv (x y &optional (ordflag nil) (modlen 12))
  (let ((x-near-y (car (matchreg (list x) (list y) ordflag modlen))))
    (min (mod (- y x-near-y) modlen)
         (mod (- x-near-y y) modlen))))

;; moved from tonnetz.lisp
;; INVERSE-IDX = 1 if a transposition, -1 if an inversion
;; (inverse-idx '(2 9 6)) = -1
(defun inverse-idx (a-chd &optional (modlen 12))
  (let* ((mulprim (multiple-value-list (prime-form a-chd)))
	 (pform (car mulprim))
	 (tlevel (cadr mulprim))
         (transp-p (transp pform tlevel))
         (mmsorted (safesort (modmult transp-p 1 modlen))))
    (if (find-if 
         (lambda (x) (equal x mmsorted))
         (permutations (modmult a-chd 1 modlen)))
      1 -1)))

;; TRANSPLEVEL -- transposition level (unique) of a chord relative to its prime-form 
(defun transplevel (a-chord)
  (second
   (multiple-value-list
       (prime-form a-chord))))

;; INVERT-CHD -- pc-set inversion of a chord
;; (invert-chd '(0 2 5)) = (0 3 5)
(defun invert-chd (chd)
  (let* ((mi (melint chd))
         (imi (reverse mi)))
    (melint->line (first chd) imi)))

;; NORMAL-FORM of a chord
(defun normal-form (chd)
  (let* ((pf (multiple-value-list (prime-form chd)))
         (primeform (first pf))
         (tlevel (second pf))
         (inv-idx (inverse-idx chd)))
    (if (eql inv-idx 1) 
        (mod12 (transp-to tlevel primeform))
        (mod12 (transp-to tlevel (invert-chd primeform))))))

;; TN-TYPE of a chord
(defun tn-type (chd)
  (let* ((pf (multiple-value-list (prime-form chd)))
         (primeform (first pf))
         (inv-idx (inverse-idx chd)))
    (if (eql inv-idx 1) 
        (mod12 primeform)
        (mod12 (invert-chd primeform)))))

;; SIS -- step-inverval series of a chord
;; (sis '(3 7 10)) = (3 4 5)
;; generalized to any size chord, Apr. 2007 
(defun sis (a-chd &optional (modlen 12))
  (let* ((pform (prime-form a-chd))
	 (apform (append pform
			 (list (+ (car pform) modlen)))))
    (mapcar (lambda (x) (mod x modlen))
	    (melint apform))))

;;; OIS -- ordered pitch-class intervallic structure
;; from Heinemann diss. (1993)
(defun ois (chd)
  (cond ((listp chd) (loop for x in chd collect (- x (first chd))))
         ((numberp chd) (list 0))))

;; BZMULT-SIMPLE -- non-commutative simple multiplication
;; returns rest if either input is a rest
(defun bzmult-simple (chd1 chd2)
  (let* ((ois1 (if (listp chd1) (ois (safesort chd1)) (ois chd1))))
    (if (or (eql 'r chd1) (eql 'r chd2))
        'r
        (if (numberp chd2)
            (transp-to chd2 ois1)
    (safesort 
     (reduce #'union
             (loop for c2 in chd2 collect 
                   (transp-to c2 ois1))))))))

;; note: "(bz-matx (chds))" found in "graphs.lisp"
;; note: "(bz-taxipath (chds len))" found in "graphs.lisp"

;; BZMS-SHADOW
;; new set-difference from bzmult-simple
;; returns rest if empty, or if either input is a rest
(defun bzms-shadow (chd1 chd2)
  (let* ((bzms (bzmult-simple chd1 chd2))
         (shad (if (eql bzms 'r) 'r
                   (set-difference bzms (union chd1 chd2)))))
    (if shad 
        (if (eql 'r shad) 'r (safesort shad))
        'r)))

;; BZMS-CHAIN -- chain of "bzms-shadow"s
(defun bzms-chain (chd1 chd2 &optional (chainlen 2))
  (cond ((eql chainlen 1) nil)
        ((eql chainlen 2) (list chd1 chd2))
        (t (let* ((prevchain (bzms-chain chd1 chd2 (- chainlen 1)))
                  (lastpair (last prevchain 2)))
             (append prevchain 
                     (list
                      (bzms-shadow (first lastpair) (second lastpair))))))))

;;; NEAREST-MULT -- nearest multiple of modnum to input
(defun nearest-mult (input modnum)
  (cond ((eql input 'r) 'r)
        ((listp input) (map 'list (lambda (x) (nearest-mult x modnum)) input))
        (t (- input (mod input modnum)))))

;; SHUFFLEBYMOD -- shuffles list by modmult indices
(defun shufflebymod (alist factor &optional (modlen 12))
  (loop for x to (- (length alist) 1) collect
        (nth 
         (+ 
          (nearest-mult x modlen)
          (mod (* factor x) modlen))
        alist)))

;; HEAPVEC -- list of 'heaped' mod-x integers
;; as (heapvec len &optional (modlen 12))
;; (heapvec 5 3) = (1 2 0 2 0)
(defun heapvec (len &optional (modlen 12) (transplevel 0))
  (let ((randpitheap (new heap of 
                          (loop for x from 0 to (- modlen 1) collect x))))
    (transp (loop repeat len collect (next randpitheap)) transplevel)))

;; RANDVEC -- list of random mod-x integers (could include repeats)
;; as (random len &optional (modlen 12))
;; (randvec 5 3) = (2 1 2 0 0)
;; added Nov. 2005 
(defun randvec (len &optional (modlen 12) (transplevel 0))
  (let ((randpitrand (new weighting :of 
                          (loop for x from 0 to (- modlen 1) collect x))))
    (transp (loop repeat len collect (next randpitrand)) transplevel)))

;; COPYLIST -- make copies of list
; (copylist '(3 4 5) 3) = (3 4 5 3 4 5 3 4 5)
;; outputs list or tree [July 2008] 
;; takes atom as input [May 2009]
(defun copylist (input mult &optional (notflat nil))
  (let* ((a-list (if (listp input) input (list input)))
	 (rawlist
	  (loop repeat mult collect a-list)))
    (if notflat rawlist (flatten rawlist))))

; COPYTHING - copy an atom into a list
(defun copything (athing length)
  (flatten (copylist (list athing) length)))

;; CONSN-P -- interval consonance test 
(defun consn-p (x y &optional (consnlist '(3 4 8 9)))
  (or (eql x 'r) 
      (eql y 'r) 
      (member (abs-intv x y) consnlist)))

;; TRICP -- interval + pit trichord test (duos only)
;; (tricp '(0 3) 8 '(0 3 7)) = T
(defun tricp (duo pit trich)
  (not
   (set-difference
    (car (multiple-value-list (prime-form (append duo (list pit)))))
    (car (multiple-value-list (prime-form trich))))))

;; TRICHORD-P -- generalized tricp
;; (trichord-p '(0 7 10) 4 '(0 4 7)) = T
(defun trichord-p (a-list pit trich)
  (cond ((or (not (listp a-list)) (< (length a-list) 2)) 'nil)
        ((= (length a-list) 2) (tricp a-list pit trich))
        ((> (length a-list) 2) 
         (car (member 't (mapcar (lambda (x) (tricp x pit trich))
                            (subsets-len a-list 2)))))))

;; MELINT -- list of melodic intervals within a list
;; "thanks Kenny Tilton!"
;; enhanced Feb. 2006 to also measure non-adjacent skips
;; example: (melint '(8 5 10 2)) = (-3 5 -8)
(defun melint (list &optional (skip 1))
  (mapcar #'intv list (nthcdr skip list)))

;; MELINT-COUNT -- returns # of melodic interval 
;; (up or down) in a melody
(defun melint-count (a-melody intv)
  (loop for a in (melint a-melody) count (= (abs a) intv)))

;; LINE-FROM-MELS -- builds a line from starting pitch & melint
;; --> adds all intervals to the starting point
;; "thanks Kenny Tilton!"
;; example: (line-from-mels 7 '(2 8 0 1)) =  (7 9 15 7 8)
(defun line-from-mels (offset ints)
  (cons offset (mapcar (lambda (int) (+ int offset)) ints)))

;; MELINT->LINE -- builds a line from starting pitch & melint
;; --> adds intervals from the previous pitch
;; (melint->line 50 '(1 2 4)) = (50 51 53 57)
(defun melint->line (startnum int-vector)
  (cons startnum
        (when int-vector
          (melint->line (+ (first int-vector) startnum)
                        (cdr int-vector)))))

;; REPLACE-INTV: replace melodic intervals (singly or in lists)
;; examples:
;; (replace-intv '(4 3 4 7 6) '(1 3) '(2 5)) = (4 2 4 9 7)
;; (replace-intv '(4 3 4 7 6) '(1 3) 2) =  (4 2 4 6 4)
;; (replace-intv '(4 3 4 7 6) 1 2) = (4 2 4 7 5)
(defun replace-intv (a-list a-int rplc-int)
  (let* ((mels (melint a-list))
         (rplcd-mels 
          (loop for x in mels collect
                (if (cond ((numberp a-int) (= (abs x) a-int))
                          (t (member (abs x) a-int)))
                  (* (cond ((numberp rplc-int) rplc-int)
                           (t (nth (position (abs x) a-int) rplc-int)))
                     (if (plusp x) 1 -1))
                  x))))
    (melint->line (first a-list) rplcd-mels)))

;; REVOICE -- shift smaller intervals up an octave
(defun revoice (chd toosmalls &optional (octsize 12))
  (if (numberp (car chd))
      (let ((sschd (safesort chd)))
	(replace-intv  sschd toosmalls (transp toosmalls octsize)))
      (loop for c in chd collect (revoice c toosmalls octsize))))

;; REORDER-BY-MELINT -- returns a melody permuted with the 
;; most frequent instance of a melint (as list of instances)
;; example: (reorder-by-melint '(1 2 3 4) 2) = 
;;  ((1 3 2 4) (1 3 4 2) (2 4 1 3) (2 4 3 1) (3 1 2 4) 
;;   (3 1 4 2) (4 2 1 3) (4 2 3 1))
(defun reorder-by-melint (a-list mel-intv)
  (let* ((aperms (permutations a-list))
         (maxmel (loop for x in aperms
                       maximize (melint-count x mel-intv))))
    (no-nils (loop for p in aperms collect 
                   (if (= (melint-count p mel-intv) maxmel) p)))))

;; STACK-UP 'voices' a melody from bottom to top 
;; example: (stack-up '(5 6 1 3 2)) = (5 6 13 15 26)
(defun stack-up (a-list &optional (oct-size 12))
  (let ((newmels (loop for x in (melint a-list) 
                       collect (mod x oct-size))))
    (melint->line (first a-list) newmels)))

;; STACK-DOWN 'voices' a melody from top to bottom
;; example: (stack-down '(72 5 1 3 2)) = (72 65 61 51 50)
(defun stack-down (a-list &optional (oct-size 12))
  (let ((newmels (loop for x in (melint a-list) 
                       collect (* -1 (- oct-size (mod x oct-size))))))
    (melint->line (first a-list) newmels)))

;; MINMAX-FILT -- returns only those ints between range
(defun minmax-filt (a-list &optional (filtmin 21) (filtmax 108))
  (set-difference (loop for x in a-list collect 
        (if (and (>= x filtmin) (<= x filtmax)) x)) '(nil)))

;; RANGEFIT -- move into correct range by mods
;; prevents extreme high & low values
;; may cause weird leaps  
(defun rangefit (alist newmin newmax)
  (let* ((modarea (- newmax newmin))
	 (minoffset (mod newmin modarea)))
    (if (nestp alist)
	(mapcar (lambda (x) (rangefit x newmin newmax)) (not-flat alist))
	(mapcar 
	 (lambda (x)
	   (cond ((symbolp x) x)
		 ((listp x) (rangefit x newmin newmax))
		 (t
		  (+ newmin (mod (- x minoffset) modarea)))))
	 alist))))

;; TOPLINE -- takes top line from a poly or mono list
;; (topline '(5 2 (7 1) 4)) =  (5 2 7 4)
(defun topline (a-list)
  (loop for x in a-list collect
        (cond ((eql x 'r) 'r)
        ((numberp x) x)
           (t (apply #'max x)))))

;; BOTTOMLINE -- takes lowest line from a poly or mono list
;; (bottomline '(5 2 (7 1) 4)) =  (5 2 1 4)
(defun bottomline (a-list)
  (loop for x in a-list collect
        (cond ((eql x 'r) 'r)
        ((numberp x) x)
           (t (apply #'min x)))))

;; MIDLINE -- takes all pits btwn. topline & bottomline
(defun midline (a-list)
  (let ((tops (topline a-list))
        (bottoms (bottomline a-list)))
    (loop for x from 0 to (- (length a-list) 1) collect
          (car (if (numberp (nth x a-list)) (nth x a-list)
              (set-difference (nth x a-list)
                              (list (nth x tops) (nth x bottoms))))))))

;; MAKECYC -- a quick helpful macro
(defmacro makecyc (a-list)
  `(new cycle of ,a-list))

; MAKEPAT - a generalized 'makecyc'
; added August 2005
; (next (makepat (indices 4) random) 10) = (0 0 2 0 0 3 0 1 2 3)
(defmacro makepat (a-list &optional (pat-type 'cycle))
  `(new ,pat-type of ,a-list))

;; MINTPROBS -- uses ivec to generate related melint weights
; use with (new weighting :of (mintprobs chd))
(defun mintprobs (chd)
  (let ((chdivec (ivec chd)))
	 (loop for n to 5 append
	       (list
		(list (+ n 1) :weight (nth n chdivec))
		(list (* -1 (+ n 1)) :weight (nth n chdivec))))))

;; MINTWGT -- melint weighting based on chd's ivec
(defun mintwgt (chd)
  (let ((mp (mintprobs chd)))
    (new weighting of mp)))

;; WIGGLE -- builds a random line with specified intervals
;; (wiggle 50 6 '(1 -2)) = (50 48 46 44 42 43)
(defun wiggle (startnum len allowed-ints)
  (let ((wiggle-ints
         (loop repeat (- len 1) collect (pickl allowed-ints))))
    (melint->line startnum wiggle-ints)))

;; TRANSP -- applying 'op' of 'level' to number or list
;; level may be a rest [July 2008] 
(defun transp (input level &optional (op #'+))
  (if (eql level 'r) 'r
      (cond
	((eql input 'r) 'r)
	((numberp input) (funcall op input level))
	(t (mapcar (lambda (x) (transp x level op)) input))))) 

;; INVERT-DK -- subtracting number or list from number, mod12
;; added Sept. 2008
(defun invert-dk (input level)
  (if (eql level 'r) 'r
      (cond
	((eql input 'r) 'r)
	((numberp input) (mod12 (- level input)))
	(t (mapcar (lambda (x) (invert-dk x level)) input))))) 

;; TRANSP-ORDER -- transposing row's order (like rotation)
(defun transp-order (alist idx)
  (rotate-list alist idx))

;; INVERT-ORDER -- inverting & shifting row's order (like retrograde rotation)
(defun invert-order (alist idx)
  (let* ((alen (length alist))
	 (new-ords (mapcar (lambda (x) (mod (- idx x) alen))
			   (indices alen))))
    (nths new-ords alist)))

;; DOUBLER -- doubles the whole list using 'transp'
(defun doubler (alist tlevel &optional (op #'+))
  (loop for x in alist collect 
        (list x (transp x tlevel op))))

;; CONTOUR-INVERSION -- inversion of a contour 
;; modlen defaults to max in list
(defun contour-inversion (nums &optional (modlen (apply #'max nums)))
    (mapcar (lambda (x) (- modlen x)) nums))

;; BASIC-CONTOURS p,i,r,ri of contours
(defun basic-contours (aset &optional (modlen (apply #'max aset)))
  (let ((cinv (contour-inversion aset modlen))) 
    (list aset cinv (reverse aset) (reverse cinv))))

;; SCONTOURS -- 'allrots' of contour
(defun scontours (aset)
  (remove-duplicates
   (allrots aset)
   :test #'seq-eql))

;; BASIC-SCONTOURS -- 'allrots' of contour (incl. p,i,r,ri)
(defun basic-scontours (aset)
  (remove-duplicates
   (flatter
    (mapcar #'allrots
	    (basic-contours aset)))
   :test #'seq-eql))

;; TAKE-CONTOUR
;; extracts contour vector from a list
;; (take-contour '(55 16 25 90 55)) = (2 0 1 3 2)
(defun take-contour (a-list)
  (let ((sortedlist 
         (safesort (remove-duplicates a-list))))
    (loop for n in a-list 
          collect (position n sortedlist))))

;; GIVE-CONTOUR-TO-SET
;; (give-contour-to-set contourlist setlist)
;; applies contour vector to a list regardless of initial positions
;; starts at the bottom of the sorted input list
;; 1. if (max contourlist) > (length sorted inputlist),
;;      all higher contour indices are stripped out
;; 2. if (max contourlist) < (length sorted inputlist),
;;      contour is applied from the bottom of the inputlist
;; (give-contour-to-set '(2 0 1 3 2) '(3 4 5 6 7)) = (5 3 4 6 5)
(defun give-contour-to-set (contourlist a-list)
  (let* ((sortedinput 
          (safesort (remove-duplicates a-list)))
         (ctrlist (if (> (apply #'max contourlist)
                         (- (length sortedinput) 1))
                    (intersection contourlist
                                   (loop for x to
                                         (- (length sortedinput) 1)
                                         collect x))
                    contourlist)))
    (loop for n in ctrlist
          collect (nth n sortedinput))))

;; ALL-CONTOURS-IN-SET -- returns each mel from 'a-list' that obeys contour
(defun all-contours-in-set (contourlist a-list)
  (let* ((sortedinput 
          (safesort (remove-duplicates a-list)))
         (ctrlist (if (> (apply #'max contourlist)
                         (- (length sortedinput) 1))
		      (intersection contourlist
				    (loop for x to
					  (- (length sortedinput) 1)
					  collect x))
		      contourlist))
	 (ctrspan (+ 1 (- (apply #'max ctrlist) 
			  (apply #'min ctrlist))))
	 (zero-ctour (transp ctrlist (* -1 (apply #'min ctrlist)))))
    (loop for trp to (- (length sortedinput) ctrspan) collect
	  (loop for n in (transp zero-ctour trp) 
		collect (nth n sortedinput)))))

;; PREBUMP
;; utility for give-contour-to-mel
;; (prebump '(2 3 1 0) '(9 6 8 10)) = (10 8 9 6) 
(defun prebump (contour melody)
  (let* ((nodupecnt (remove-duplicates contour :from-end t))
         (bumperpair (pairlis melody nodupecnt)))
    (no-nils (loop for x to (- (length contour) 1) collect
          (car (rassoc x bumperpair))))))

;; BUMPUP -- utility for give-contour-to-mel
;; (bumpup '(10 8 9 7) 12) =  (10 20 21 31)
(defun bumpup (alist &optional (octsize 12))
  (if (cadr alist)
    (cons (car alist)
          (if (< (cadr alist) (car alist))
            (bumpup (transp (cdr alist) octsize) octsize)
            (bumpup (cdr alist) octsize)))
    alist))

;; GIVE-CONTOUR-TO-MEL -- shapes mel to contour
;; keeps pc order of mel, uses octave displacement
;; (give-contour-to-mel '(3 2 1 0 1) '(5 6 7 8)) = (41 30 19 8 19)
(defun give-contour-to-mel (contour mel)
  (permutelist contour (bumpup (prebump contour mel))))

;; TRANSP-TO: moves a list to start at a designated level
;; (transp-to 50 '(3 4 7)) = (50 51 54)
;; Jan. 2010: handles slots
;; June 2010: handles initial rests
(defun transp-to (level input)
  (let ((firstinpit (car (norests input))))
    (if (eql 'r input) 'r
	(if (numberp input) level
	    (mapcar (lambda (y) 
		      (if (eql 'r y) 'r
			  (- y (- (car (norests input)) level)))) input)))))

;; TRANSP-MIN -- transpose list to new min
(defun transp-min (alist &optional (newmin 0))
  (transp alist 
	  (+ newmin (* -1 (apply #'min (flatten alist))))))

;; TR-BY-GRP -- transposing members of list by groups
;; example: (tr-by-grp '(1 2 3 4) '(1 2) '(40 50)) =
;; (41 52 53 44 51 52 43 54 51 42 53 54)
(defun tr-by-grp (a-list nums levels &optional (op #'+))
  (let ((listcyc (new cycle of a-list))
	(numcyc (new cycle of nums))
	(levelcyc (new cycle of levels)))
	(flatten (loop
	  until (and (eop? listcyc) (eop? levelcyc))
	  collect (transp (next listcyc (next numcyc))
		(next levelcyc) op)))))
	
;; INTERLOCK -- interlocking 2 lists
;; example: (interlock '(1 2 3) '(33 44 55) '(1 2) '(2 3)) = 
;; (1 33 44 2 3 55 33 44 1 55 33 2 3 44 55 33 1 44 55 2 3 33 44 55)
;; Sept. 2006: 'notfullcyc' flag stops evaluation when first list is exhausted
;; adjusted June 2008: non-flat output
(defun interlock (list1 list2 num1 num2 &optional (notfullcyc nil))
  (let ((list1cyc (new cycle of list1))
	(list2cyc (new cycle of list2))
	(num1cyc (new cycle of num1))
	(num2cyc (new cycle of num2)))
    (loop
	until (if notfullcyc (eop? list1cyc)
		  (and (eop? list1cyc) (eop? list2cyc)
		       (eop? num1cyc) (eop? num2cyc)))
      append (next list1cyc (next num1cyc))
      append (next list2cyc (next num2cyc)))))

;; TR-INSERT  -- after each item, insert the same item transposed 
;;	according to list of factors
;; example: (tr-insert '(1 2 3 4) '(0 20 50) #'+) =
;; (1 1 2 22 3 53 4 4 1 21 2 52 3 3 4 24 1 51 2 2 3 23 4 54)
(defun tr-insert (alist levels op)
  (let ((alistcyc (new cycle of alist))
	(levelcyc (new cycle of levels)))
    (flatten (loop 
		 until (and (eop? alistcyc) (eop? levelcyc))
	       for x = (next alistcyc)
	       collect x
	       collect (transp x (next levelcyc) op)))))

;; INTERSECTION (standard lib) -- including only selected list items 
;; example: (intersection '(1 2 3 4 5 6 5 4 3 2 1) '(2 3 4)) = 
;; (2 3 4 4 3 2)

;; SCF -- to substitute members of a list
;; Thanks Bob Coyne!
;; used with 'cyclops.lisp'
;; example: (scf '(10 14 15) '(a b b c b c c)) = (10 14 14 15 14 15 15)
(defun scf (newbies target-list)
  (let ((oldies nil))
    ;; first extract the unique ordered elements from orig. list
    (loop for i in target-list
	  do (pushnew i oldies))
    (setq oldies (reverse oldies))
    ;; if wrong number of new elements, signal an error
;    (when (not (= (length oldies) (length newbies)))
;      (error "Length mismatch ~a with ~a" newbies oldies))
    (loop for old in oldies
	  for new in newbies
	  do (setq target-list (substitute new old target-list)))
    target-list))

;; STRAVROT -- cycles thru list with Stravinsky-style rotation
;; example: (stravrot '(4 7 2)) = (4 7 2 4 11 1 4 6 9)
;; April 2006: nested list option
;; fixed May 2009 [problems with 'rotation' pattern]
(defun stravrot (alist &optional (nestp 'nil))
  (let ((rawout
	 (mapcar 
	  (lambda (x) (mod12 (transp-to (car alist) x)))
	  (allrots alist))))
    (if nestp rawout (flatten rawout))))

;; ALL-STRAV -- p,i,r,ri stravrot forms as list-of-lists
(defun all-strav (alist)
  (let* ((strav-p (stravrot alist 'np))
	 (strav-i (stravrot (inversion alist) 'np))
	 (strav-r (stravrot (reverse alist) 'np))
	 (strav-ri (stravrot (reverse (inversion alist)) 'np)))
    (remove-duplicates
     (append strav-p strav-i strav-r strav-ri)
     :test #'seq-eql)))

;; CHDHEIGHT -- a way of computing height of chords
(defun chdheight (achord)
  (let ((schd (safesort achord)))
    (loop for x to (- (length schd) 1) sum
          (* (+ x 1) (nth x schd)))))

;; CHORDINTS -- all intervals between members of a chord
(defun chordints (alist)
  (remove-duplicates 
   (safesort
    (loop for x in (subsets-len alist 2) collect 
          (mod-intv (car x) (cadr x))))))

;; LOWERCHORD -- binary (<) comparator by height
(defun lowerchord (chd1 chd2)
  (< (chdheight chd1) (chdheight chd2)))

;; HIGHERCHORD -- binary (>) comparator by height
(defun higherchord (chd1 chd2)
  (> (chdheight chd1) (chdheight chd2)))

;; CHDINV-UP -- next inversion of a chord (bottom member + oct-size)
;; oct-size will traditionally be 12
;; (chdinv-up '(2 5 9)) = (5 9 14)
(defun chdinv-up (alist &optional (oct-size 12))
  (flatten (append (cdr alist) (+ (car alist) oct-size))))

;; CHDINV-DOWN -- 'previous' inversion of a chord (top member - oct-size)
;; oct-size will traditionally be 12
;; (chdinv-down '(9 12 13) 5) = (8 9 12)
(defun chdinv-down (alist &optional (oct-size 12))
  (cons (- (car (last alist)) oct-size) (butlast alist)))

;; CHD-INVERSION -- returns numbered inversion of a chord (up or down)
;; rewritten March 2008
(defun chd-inversion (achd in-idx &optional (octsize 12))
  (if (eql 0 in-idx) achd
      (let* ((inidx-minus (if (minusp in-idx) -1 1))
	     (idx (abs in-idx))
	     (orig-ctr (take-contour achd))
	     (sorted-chd (safesort achd))
	     (chdlen (length achd))
	     (octfact (floor (/ idx chdlen)))
	     (remfact (rem idx chdlen))
	     (octvect (copylist (list octfact) (length achd)))
	     (remvect (append (copylist (list 1) remfact)
			      (copylist (list 0) (- chdlen remfact))))
	     (transpvect 
	      (transp (map 'list #'+ octvect remvect) inidx-minus #'*)))
	(give-contour-to-set 
	 orig-ctr
	 (map 'list 
	      (lambda (a b) (+ a (* octsize b)))
	      (if (eql -1 inidx-minus) (reverse sorted-chd) sorted-chd)
	      transpvect)))))

;; NEIGHBOR -- finds the closest member to a number
;; example: (neighbor 7 '(2 4 6 8 9)) = 6
(defun neighbor (num alist)
  (let ((mindist
	 (loop for i in alist minimize (abs (- num i)))))
    (car (member-if
	  (lambda (x) (= (abs (- num x)) mindist)) alist))))

;; NEIGHBOR-COMPL -- finds closest member, returns rest of list
;; example: (neighbor-compl 7 '(2 4 6 8 9)) = (2 4 8 9)
(defun neighbor-compl (num alist)
  (set-difference alist (list (neighbor num alist))))

;; STACK-BY -- 'voice' chords to favor an interval
;; returns list of chords
(defun stack-by (a-list intv &optional (oct-size 12))
  (let* ((stax (loop for x in 
		     (reorder-by-melint a-list intv)
		     collect (stack-up x oct-size)))
	 (maxstax (loop for y in stax
			maximize (melint-count y intv))))
    (no-nils (loop for s in stax collect
                   (if (= (melint-count s intv) maxstax) s)))))

;; SCRUNCH -- change measure length by adjusting downbeat duration
;; returns downbeat alone if (new meas length < upbeats) 
;; example: (scrunch '(3.5 .25 .25 .5) 2.75) = (1.75 0.25 0.25 0.5)
(defun scrunch (alist newlen)
  (if (< newlen (apply #'+ (cdr alist))) (list newlen)
  (cons (- newlen (apply #'+ (cdr alist))) (cdr alist))))

;; ARPEGG -- arpeggiate dyads and chords
;; pitnums = number of pitches in each arpeggiation
;; atknums = number of attacks before moving to next pit group
;; notflat = flag to include sublists, otherwise flat
;; June 2007: 'alist-only' flag will stop at end of 'alist'
;; April 2009: can take & return non-flat lists (ie chords)
;; example: (arpegg '(1 2 3 4) '(2 3) '(4 3)) = 
;; (1 2 1 2 3 4 1 2 3 2 3 4 1 2 3 4 3 4 1 2 3 4 1 4 1 2 3 4)
(defun arpegg (alist pitnums atknums 
		     &optional (alist-only 'nil) (notflat 'nil)) 
  (let* ((listcyc (new cycle of (indices (length alist))))
	 (pitcyc (new cycle of pitnums))
	 (atkcyc (new cycle of atknums))
	 (nlist (patt-to-sum pitcyc (length alist)))
	 (arpeglist 
	  (if alist-only
	      (loop for nl in nlist 
		    collect (next (new cycle of (next listcyc nl))
				  (next atkcyc)))
	      (loop until (and (eop? listcyc) (eop? pitcyc) (eop? atkcyc))
		collect (next (new cycle of (next listcyc (next pitcyc)))
			      (next atkcyc))))))
    (chooser
     (if notflat arpeglist (flatten arpeglist))
     alist)))

;; MODMULT -- multiplies & mods (number or list)
;; example: (modmult '(1 2 3) 3 4) = (3 2 1)
(defun modmult (input multnum modnum)
  (if (number? input) 
    (mod (* multnum input) modnum)
    (mapcar (lambda (x) (mod (* multnum x) modnum)) input)))

;; MODAREA -- move list into range
;; [not sure this works properly -- 'rangefit' seems better]
(defun modarea (alist min max)
  (mapcar (lambda (x) (+ min (mod x (- max min))))
	  alist))

;; CLIP-HI -- returns list <= a given value
;; example: (clip-hi 10 '(8 9 10 11 12)) = (8 9 10 10 10)
(defun clip-hi (num a-list)
  (mapcar (lambda (x) (min x num)) a-list))

;; CLIP-LO -- returns list >= a given value
;; example: (clip-lo 5 '(7 6 5 4 3)) = (7 6 5 5 5)
(defun clip-lo (num a-list)
  (mapcar (lambda (x) (max x num)) a-list))

;; CONSMATCH -- put a line against another, match where it can
;; example: (consmatch '(9 8 7 6) '(1 (2 6) r 3 (1 5) r 2 3 (1 4) 2 3) 'nil)
;; ((1 9) (2 6) R 3 (8 1 5) R 2 (3 7) (1 4) (2 6) 3)
(defun consmatch (blist alist &optional (fill-rests 't) (consvec '(3 4 8 9)))
  (let ((alistcyc (new cycle of alist))
	(blister blist))
    (loop 
        until (and (eop? alistcyc) 
                   (or (null? blister)
                       (not (member-if
                             (lambda (x) (consn-p x (car blister) consvec)) 
                             (flatten alist)))))
      for next-a = (next alistcyc)
      collect 
      (cond ((numberp next-a)
             (cond ((null? blister) next-a)
                   ((consn-p next-a (car blister) consvec) 
                    (list next-a (pop blister)))
                   (t next-a)))
            ((listp next-a)
             (cond ((null? blister) next-a)
                   ((member-if (lambda (x)
                                 (consn-p x (car blister) consvec))
                               next-a)
                    (push (pop blister) next-a))
                   (t next-a)))
            ((eql next-a 'r)
             (if fill-rests
               (if (null? blister) 'r (pop blister)) 'r))))))

;; SELF-STRETTO -- recursive self-consmatch
;; added February 2006
(defun self-stretto (mel vcs intvl &optional (waitlen 1))
  (if (eql vcs 1) mel
      (consmatch 
       (append (copylist '(r) (* waitlen (- vcs 1)))
               (transp mel (* (- vcs 1) intvl)))
       (self-stretto mel (- vcs 1) intvl waitlen))))

;; TRIMATCH -- match line to another by assembled trichords
; (trimatch '(1 2 3 4) '((4 8) 10 (2 3) r (0 7)) '(0 4 7))
; = ((1 4 8) 10 (2 3) 2 (3 0 7))
(defun trimatch (blist alist trich &optional (fill-rests 't))
  (let ((alistcyc (new cycle of alist))
	(blister blist))
    (loop 
        until (eop? alistcyc)
      for next-a = (next alistcyc)
      collect 
      (cond 
        ((eql next-a 'r)
         (if fill-rests
           (if (null? blister) 'r (pop blister)) 'r))
        ((trichord-p next-a (car blister) trich)
         (push (pop blister) next-a))
        (t next-a)))))

;; MYHAND -- doubles melody whenever an interval occurs
;; a-list = melody, an-intvl = melodic interval to find
;; dblint = doubling interval when an-intvl occurs
;; dbltype = where to double: can be 'start or 'end (of the melodic interval)
;; (myhand '((10 2) 5 6 (9 20) 4 3) -5 11 'end) = ((10 2) (16 5) 6 (9 20) 4 3)
(defun myhand (a-list an-intvl dblint &optional (dbltype 'start))
  (let* ((monoline (topline a-list))
         (a-melint (melint monoline))
         (typid (if (equal dbltype 'start) 0 1))
         (dbld-line 
          (loop for n to (- (length a-melint) 1) collect
                (if (= (nth n a-melint) an-intvl)
                  (if (numberp (nth (+ n typid) a-list)) 
                    (list (+ (nth (+ n typid) a-list) dblint)
                          (nth (+ n typid) a-list))
                    (cons (+ (nth (+ n typid) monoline) dblint) 
                          (nth (+ n typid) a-list)))
                  (nth (+ n typid) a-list))))
         (cleanup (loop for x in dbld-line collect
                        (if (numberp x) x
                            (remove-duplicates x)))))
    (if (equal dbltype 'start)
      (append cleanup (if (listp (last a-list)) 
                        (last a-list) (list (last a-list))))
      (append (list (first a-list)) cleanup))))

;; THINOUT -- removes each instance of members in list, from the beginning
;; May 2009: fixed, & added 'nestp' flag
;; (thinout '(1 2 3 1 2 4 4 5) 'nest) = 
;; ((1 2 3 1 2 4 4 5) (2 3 2 4 4 5) (3 4 4 5) (4 4 5) (5))
(defun thinout (alist &optional (nestp nil))
  (let ((nestlist
	 (butlast  
	  (append (list alist) 
		  (loop with nodupes = (remove-duplicates alist :from-end t)
			for nd in nodupes
			for excluded = (append excluded (list nd))
			collect 
			(reverse (set-difference alist excluded)))))))
    (if nestp nestlist (flatten nestlist))))

;; FILLIN -- builds up to entire list from repeated starts
;; (fillin '(1 2 3)) = (1 1 2 1 2 3)
(defun fillin (alist)
  (let ((myaccum (new accumulation :of alist)))
    (loop until (eop? myaccum) collect (next myaccum))))

;; INDICES -- get a quick list of integers
;; (indices 4) = (0 1 2 3)
;; enhanced June 2005
(defun indices (len &optional (base 0))
  (transp (loop for n to (- len 1) collect n) base))

;; SCALEY -- quick & dirty macro
(defmacro scaley (min max len)
  `(next (new range from ,min to ,max) ,len))

;; STEPPER -- gets from integer a to integer b by step (omits b)
(defun stepper (a b)
  (if (eql a b) (list a)
      (let ((dir (if (> b a) 'to 'downto)))
	(butlast (eval `(loop for x from ,a ,dir ,b by 1 collect x))))))

;; LIST-STEPPER -- stepping within list
(defun list-stepper (inlist &optional (flatflag nil))
  (let ((rawout
	 (map 'list #'stepper inlist (cdr inlist))))
    (if flatflag (flatten rawout) rawout)))

;; RANDSCALEY -- generate a 'totalen' chain of random-length indices 
;; each sub-chain of length btwn/incl. 'minindex' & 'topindex' 
;; [fixed June 2005 to avoid repeated 'minindex']
(defun randscaley (minindex topindex totalen)
  (let ((rawlist (loop repeat totalen append
        (transp (indices (+ 2 (random (- topindex minindex)))) minindex))))
    (subseq rawlist 0 totalen)))

;; RANDOM-INDICES -- returns shuffled indices
;; (random-indices 7) = (3 0 4 1 5 2 6)
(defun random-indices (len)
  (shuffle (indices len)))

;; RANDMEL -- random indices using modlen; no repeated entries
;; (randmel 10 4) =  (3 2 0 2 0 1 3 2 1 3)
(defun randmel (list-len &optional (modlen 12))
  (let ((melgen (new weighting :of
                     (loop for x in (indices modlen)
                           collect (list x :max 1)))))
    (loop repeat list-len collect (next melgen))))

;; SNAKE -- oscillating among indices
;; July 2008: 'len' defaults to single (palindromic) instance
;; (snake 5 10 'down) =  (4 3 2 1 0 1 2 3 4 3)
(defun snake (height &optional 
		     (mindex 0)
		     (len (- (* 2 height) 2))
		     (direction 'up))
  (transp
   (let* ((baselist (if (eql direction 'up) (indices height)
			(reverse (indices height))))
	  (snakepal (new palindrome :of baselist :elide true)))
     (next snakepal len))
   mindex))

;; EXPAND -- revised Feb. 2006 (see below)

;; WRAPPERS FOR NONDETERMINISTIC STUFF

;; WIGGLE-TO -- all paths from startnum to endnum 
;; by combination of allowed-ints
;; if fails, returns startnum
(defun wiggle-to (startnum endnum steps allowed-ints)
  (if (or (eql startnum 'r) (eql endnum 'r))
      startnum
      (let ((wt (screamer-user::wigto startnum endnum steps allowed-ints)))
	(if wt (no-nils wt) startnum))))

;; WIGLINE -- builds a line of wiggle-to's by durlist 
;; returns ((pits-with-wiggles) (durs-with-wiggles))
;; (wigline '(45 55 51) '(10 12 8) '(2 -3)) = 
;; ((45 47 49 51 53 55 57 54 51) (6 1 1 1 1 10 1 1 8))
(defun wigline (a-line durlist intvs &optional (treeflag nil))
  (let* ((proper-durlist (if (listp durlist) durlist (list durlist)))
         (durs (next (new cycle of proper-durlist) (length a-line)))
         (wigler (loop for x to (- (length a-line) 2) collect
                       (wiggle-to (nth x a-line) (nth (+ x 1) a-line)
                                  (nth x durs) intvs)))
         (wigpits (loop for x to (- (length wigler) 1) collect
                        (if (atom (nth x wigler))
			    (nth x a-line)
			    (butlast (melint->line (nth x a-line)
						   (shuffle 
						    (pickl (nth x wigler))))))))
         (wigdurs (loop for x to (- (length wigler) 1) collect
                        (if (atom (nth x wigler))
			    (nth x durs)
			    (cons (+ (- (nth x durs) 
					(length (nth x wigpits))) 1)
				  (loop repeat 
				      (- (length (nth x wigpits)) 1) 
				    collect 1))))))
    (if treeflag
	(list (append wigpits (list (last a-line)))
	      (append wigdurs (list (last durs))))
	(list (append (flatten wigpits) (last a-line))
	      (append (flatten wigdurs) (last durs))))))

;; EQL-SUMMER -- all ways to sum componentnums to targetnum
;; if fails, returns targetnum
(defun eql-summer (targetnum componentnums)
  (let ((es (screamer-user::eqlsum targetnum componentnums)))
    (if es es (list targetnum))))

;; ONES&TWOS -- all seqs of 1&2 summing to a number
(defun ones&twos (anum)
  (remove-duplicates 
   (flatter
    (mapcar #'permutations (eql-summer anum '(1 2))))
   :test #'seq-eql))

;; EMBELL-TRIAD -- list of random 'neighbor' triads to triad 
;; with largest leap of 'span' and same sum
;; example: (embelltriad '(3 5 6) 1) = ((2 5 7) (3 4 7))
(defun embell-triad (triad &optional (steps 3) (sumspan 5))
  (screamer-user::near-ebt triad sumspan steps))

;;

;; NTN->CLISTS -- wrapper for "ntn-to-clists" nondet function
;; returns all contours in binsize that correspond to 
;; ntn (note-to-note) contour
;; (ntn->clists '(-1 1) 3) = ((1 0 1) (2 0 1) (1 0 2) (2 0 2) (2 1 2))
(defun ntn->clists (antn binsize)
  (screamer-user::ntn-to-clists antn binsize))

;; BESTMATCH-NONDET -- most compact 'consn-p' structuring
;; calls screamer
;; .... slowwww when list lengths > 3
(defun bestmatch-nondet (line1 line2)
  (no-nils
   (not-flat
    (norests
     (merge-slots
      (map 'list #'tpoints
	   (list line1 line2) 
	   (car
	    (sort
	     (screamer-user::bestmatch (length line1) 
				       (length line2) 
				       (non-matches line1 line2))
	     (lambda (a b) (< (apply #'max (flatten a))
			      (apply #'max (flatten b))))))))))))

;; REGISTER STUFF

;; AVG-CHDPIT -- 'average' of an integer list 
;; returns integer
(defun avg-chdpit (chd)
  (car (multiple-value-list (floor (/ (apply #'+ chd) (length chd))))))

;; AVG-CHD-SORT -- sorts list of chords by 'avg-chdpit'
;; cruder version of 'tsp-chords'
;; (works for different-sized chords)
(defun avg-chd-sort (chdlist)
  (sort chdlist
   (lambda (a b) (< (avg-chdpit a) (avg-chdpit b)))))

;; OCTAVESPREAD -- make list of any pitch in all octaves
(defun octavespread (pit &optional (spreadmin 21) (spreadmax 108) (modlen 12))
  (minmax-filt (loop for x in (indices (+ 1 (floor (/ spreadmax modlen)))) 
                     collect
                     (+ (mod pit modlen) (* x modlen)))
               spreadmin spreadmax))

;; MATCHREG -- change register to minimize leaps
;;  .. in list case, bring all members of list1 closest to
;; corresponding members of list2 by shifting octaves
;; July 2006: ordered or unordered match, lists of unequal size

(defun mr (a b &optional (modlen 12))
  (let* ((atransp (loop for z from 0 to (floor (/ 90 modlen))
		       collect (+ (* z modlen) (mod a modlen))))
	(smalldiff (loop for x in atransp minimize (abs (- x b)))))
    (car (member-if (lambda (q) (= (abs (- q b)) smalldiff)) atransp))))

(defun matchreg (thing1 thing2 &optional (ordflag nil) (mlen 12))
  (let ((adjthing1 (closest (mod12 thing2) (mod12 thing1))))
    (cond ((and (numberp thing1) (numberp thing2))
           (mr thing1 thing2 mlen))
          ((and (listp thing1) (listp thing2))
           (cond ((= (length thing1) (length thing2))
                  (map 'list 
                       (lambda (a b) (mr a b mlen))
                       (if ordflag thing1 adjthing1) thing2))
                 ((< (length thing1) (length thing2))
                  (matchreg thing1
                            (subseq thing2 0 (length thing1))))
                 ((> (length thing1) (length thing2))
                  (append (matchreg (subseq thing1 0 (length thing2))
                                    thing2 ordflag)
                          (matchreg (subseq thing1 (length thing2))
                                    thing2 ordflag)))))
          ((and (listp thing1) (numberp thing2))
           (mapcar (lambda (x) (mr x thing2 mlen)) thing1))
          ((eql 'r thing1) 'r)
          ((eql 'r thing2) thing1)
          ('t (error 
               "Type or length mismatch ~a with ~a" thing1 thing2)))))

;; MATCHREG-CHDS -- does consecutive chdmatch for list 
;; list of equal length chords
;; output length fixed Sept. 2006
(defun matchreg-chds (chdlist &optional (ordflag nil))
  (if (eql 2 (length chdlist))
      (list (first chdlist) (matchreg (second chdlist) (first chdlist)))
    (cons (car chdlist)
	  (matchreg-chds 
	   (cons (matchreg (cadr chdlist) (car chdlist) ordflag) 
		 (cddr chdlist))))))

;; TAKEREG -- returns reg of pits (atom or list)
(defun takereg (input &optional (basepit 0) (modlen 12))
  (cond ((numberp input) 
         (car (multiple-value-list (floor (/ (- input basepit) modlen)))))
        ((eql input 'r) 'r)
        (t (mapcar 
	    (lambda (x) (takereg x basepit modlen))
	    input))))

;; PARSEREG -- returns a slotlist for each register
;; added June 2010
(defun parsereg (inpits &optional (basepit 0) (modlen 12)) 
  (let* ((tpoly (take-poly inpits))
	 (regvec
	  (make-poly (norests (takereg (flatten inpits) basepit modlen)) tpoly))
	 (minreg
	  (apply #'min (flatten (norests regvec))))
	 (maxreg
	  (apply #'max (flatten (norests regvec)))))
    (loop for r from minreg to maxreg
	  collect
	  (mapcar
	   (lambda (x) (if (and (listp x) (= 1 (length x))) (car x) x))
	   (map 'list 
		(lambda (rg pt)
		  (cond ((and (numberp rg) (eql rg r)) pt)
			((and (listp rg) (member r rg))
			 (filter 
			  (lambda (x) (eql r (takereg x basepit modlen)))
			  pt))
			(t 'r)))
		regvec
		inpits)))))

;; PLACEREG -- translates list to registers
;; input = original pitch list
;; regbase = lowest note in output (24 often works for me)
;; modindex = size of 'octave' (usually 12)
;; reg = pattern, list, or scalar governing placement in different octaves
;; Sept. 2008: allows sublists & rests in 'reg'
;; (placereg '(1 2 3 4 5 6) (new cycle of '(5 3 4 2))) =  (61 38 51 28 65 42)
(defun placereg (inpits reg &optional (modindex 12) (regbase 0))
  (let* ((input (modlist inpits modindex))
         (incyc (new cycle of input))
         (regpatt (cond ((pattern? reg) reg)
                        ((listp reg) (makecyc reg))
                        (t (makecyc (list reg))))))
    (loop until (eop? incyc)
      collect 
      (let ((thisreg (next regpatt)))
	(if (eql 'r thisreg) 
	    'r
	    (if (numberp thisreg)
		(transpose (next incyc) 
			   (+ regbase (* modindex thisreg)))
		(map 'list 
		     (lambda (a b) (transpose a (+ regbase (* modindex b))))
		     (next incyc (length thisreg))
		     thisreg)))))))		     

;; FIXREG -- place pc into same register always [random by default] 
(defun fixreg (melody &optional (reglist (shuffle (heapvec 12 4 3))))
  (cond ((eql melody 'r) 'r)
        ((numberp melody)
	 (car (placereg melody (nth (mod12 melody) reglist))))
	(t (mapcar (lambda (x) (fixreg x reglist)) melody))))

;; TENDREG -- replace registers for smoothest transition from start to end
;; [adjusts register of all chords in the middle]
;; can be ordered or unordered
(defun tendreg (chdlist &optional (ordflag nil) (basepit 0) (modlen 12))
  (case (length chdlist)
    (1 chdlist)
    (2 chdlist)
    (t
     (let* ((startreg (takereg (car chdlist) basepit modlen))
	    (endreg (takereg (car (last chdlist)) basepit modlen))
	    (basereg (union startreg endreg))
	    (loreg (apply #'min basereg))
	    (hireg (apply #'max basereg))
	    (allreg (indices (+ 1 (- hireg loreg)) loreg))
	    (mid-baselist (mapcar #'mod12 (butlast (cdr chdlist))))
	    (midregs (loop for mb in mid-baselist collect
			   (loop for b in mb collect 
				 (loop for r in allreg append
				       (placereg b r modlen basepit)))))
	    (posschds (loop for m in midregs collect (all-combos m 'flat)))
	    (posspaths (if (eql 3 (length chdlist))
			   (loop for p in (first posschds) collect
				 (list p))
			   (all-combos posschds)))
	    (complete-posspaths 
	     (mapcar (lambda (x) 
		       (append (list (car chdlist)) x (last chdlist)))
		     posspaths))
	    (bestdist
	     (apply #'min
		    (mapcar 
		     (lambda (x) (total-distances x ordflag)) 
		     complete-posspaths)))
	    (shortpaths 
	     (no-nils
	      (loop for x in complete-posspaths collect
		    (if (eql bestdist (total-distances x)) x))))
	    (spleaps (mapcar #'each-distance shortpaths))
	    (bestleaps (car (list<sort spleaps))))
    (car
     (member-if 
      (lambda (x)
        (seq-eql bestleaps (each-distance x)))
      shortpaths))))))

;; OTONE-REG -- places pcs as overtones of fund
;; added May 2009
(defun otone-reg (fund input &optional (top-otone 27))
  (let* ((alist (if (listp input) input (list input)))
	 (harms (key-harmonics fund (indices top-otone 1)))
	 (choice-matx
	  (loop for n in (indices 12) collect
		(position n
			   (mod12 harms)))))
    (mapcar 
     (lambda (x) (nth (nth x choice-matx) harms))
     alist)))

;; some handy graphs for registers

;; MIDDLEWEIGHT-5 -- tends to bounce back to 3 of 5
;; May 2009: changed to range 3-7
;mkg (defun middleweight-5 
;mkg   (new graph of 
;mkg        `((3 :id A :to ,(new weighting of '(C D)))
;mkg 	 (4 :id B :to C)
;mkg 	 (5 :id C :to ,(new weighting of '(A B C D)))
;mkg 	 (6 :id D :to ,(new weighting of '(C E)))
;mkg 	 (7 :id E :to D))))

;; FOLLOWING-5 -- tends to move gradually from one area to another
;; used mostly for registers
;; May 2009: changed to range 3-7
;mkg (defun following-5 
;mkg       (new graph of 
;mkg 	   `((3 :id A :to B)
;mkg 	     (4 :id B :to ,(new weighting of '(A B C)))
;mkg 	     (5 :id C :to ,(new weighting of '(B C D)))
;mkg 	     (6 :id D :to ,(new weighting of '(C D E)))
;mkg 	     (7 :id E :to D))))

;; FOLLOWING-3 -- tends to move gradually from one area to another
;; used mostly for registers
;mkg (defun following-3 
;mkg       (new graph of 
;mkg 	   `((4 :id A :to B)
;mkg 	     (5 :id B :to ,(new weighting of '(A B C)))
;mkg 	     (6 :id C :to B))))

;;; RHYTHM STUFF

;; INBETWEEN -- utility for 'valbytime'
(defun inbetween (anumber alist)
  (let ((mins (mapcar (lambda (x) (> x anumber)) alist)))
    (cond ((eql 't (car mins)) 0)
          ((not (member 't mins)) (- (length alist) 1))
          (t (- (position t mins) 1)))))

;; VALBYTIME -- changes value according to 'changedurs'
;; returns list of vals matching 'durs' 
;; (valbytime '(3 3 3 3) '(s n p j q) '(2 2 2 2 2 2 2)) = (s s n p p j q q)
(defun valbytime (changedurs vals durs)
  (let* ((changeatx (melint->line 0 changedurs))
         (duratx (melint->line 0 durs))
         (changeidx (loop for x in duratx collect
                          (inbetween x changeatx))))
    (loop for y in changeidx collect (nth y vals))))


;; DURS->SLOTS -- makes placeholders between attacks
;; 'durs' should be integers
;; (durs->slots '(55 52 54) '(2 3 2)) = (55 R 52 R R 54 R)
;; note: must use (if (numberp a-pit)) clause in playback!
;; Dec. 2007: now treats chords properly
(defun durs->slots (pits durs)
   (loop for n to (- (length pits) 1) append
         (cons (nth n pits) 
               (loop repeat (- (nth n durs) 1) collect 'r))))

;; MERGE-SLOTS -- combines all pits slot-by-slot
;; [fixed Nov. 2005]
;; makes use of rest placeholder 'r
;; (merge-slots '((2 3 r r 5 6) (r (4 5) r 1 1 r) (44 10 r))) 
;; = ((2 44) (3 4 5 10) R 1 (5 1) 6)
(defun merge-slots (&rest lists)
  (let* ((plists (car lists))
         (maxlen (apply #'max 
                        (mapcar #'length plists)))
         (rufflist
          (loop for n to (- maxlen 1) collect
                (loop for x in plists collect
                      (if (< n (length x)) (nth n x) 'r))))
         (rawlist
          (loop for ruffslot in rufflist collect
                (if (or (some #'listp ruffslot) (some #'numberp ruffslot))
		    (remove-duplicates 
		     (flatten 
		      (set-difference ruffslot '(r))))
		    'r))))
    (not-flat
     (loop for x in rawlist collect
	   (cond ((and (listp x) (> (length x) 1)) x)
		 ((listp x) (first x))
		 (t x))))))

;; STARTINGRESTNUM -- utility for slots->durs (May 2005)
;; counts the number of starting rests
;; returns 'nil' if not applicable
(defun startingrestnum (alist)
  (let ((restlist (loop repeat (length alist) collect 'r)))
  (if (eql (first alist) 'r) (mismatch alist restlist))))

;; SLOTS->DURS -- takes slotlist; makes pitlist & durlist (integers)
;; completely rewritten Sept. 2006
;; fixed 'r as list - Feb. 2008
(defun slots->durs (inmel)
  (let* ((melody 
	  (mapcar (lambda (x) (if (and (listp x) (list-eql x (list 'r))) 'r x))
		  inmel))
	 (gathered-mel (loop with fragment
			 for (note next-note) on melody
			 do (push note fragment)
			 unless next-note
			 collect (nreverse fragment) into result
			 and do (loop-finish)
			 when 
			 (not (eql 'r next-note))
			 collect (nreverse fragment) into result
			 and do (setf fragment nil)
			 finally (return result))))
    (list (mapcar #'car gathered-mel)
	  (mapcar #'length gathered-mel))))

;; MERGE-SD -- chaining slot/dur vectors together
;; returns merged sd-pairs (1 per voice)
;; can be single or multiple sd-pairs
(defun merge-sd (sdsvec)
  (if (eql 2 (apply #'max (take-poly sdsvec)))  
      (mapcar #'flatter (transpose-matx sdsvec))
      (mapcar #'merge-sd (transpose-matx sdsvec))))

;; ATX->DURS -- takes atk/dur pair & returns pits/dur pair
;; accounts for initial rest
(defun atx->durs (atkdurpair)
  (let* ((pits (first atkdurpair))
	 (atx (second atkdurpair))
	 (mint (append (melint atx) (list 1))))
    (if (member 0 atx) 
	(list pits mint)
	(list (cons 'r pits)
	      (cons (first atx) mint)))))

;; PITSDURS->ATX -- constructs atkpts from pits/durs
(defun pitsdurs->atx (pits durs)
  (let ((rawout
	 (butlast
	  (melint->line 0
			(sum-across durs
				    (second
				     (slots->durs pits)))))))
    (if (eql (car pits) 'r) (cdr rawout) rawout)))

;; STRUMS -- creates downbeat followed by upbeats
;; integer list for use in 'ferney' etc.
;; (strums 3 3 6 2 7) = (3 1 1 6 1 1 1 1 4 1 1 1)
(defun strums (iters min-db max-db min-upatx max-upatx &optional (mval 1))
  (flatten (loop repeat iters
             collect
             (append (list (between min-db (+ max-db 1)))
                     (loop repeat (between min-upatx
                                           (+ max-upatx 1))
                       collect mval)))))

;; GATHER-STRUMS
;; gathers (2 1 1 1) etc. sublists from flat strum list
(defun gather-strums (polylist)
  (let ((rawpos
	 (append (reverse (filterpos (lambda (x) (> x 1)) polylist)) 
		 (list (length polylist)))))
    (mapcar #'reverse (make-poly polylist (melint rawpos)))))

;; DNBEATS -- takes pits & durs,
;; makes vectors of long downbeat + 1's (like strums)
;; (dnbeats '(50 43 56 78 43) 9) = (5 1 1 1 1)
(defun dnbeats (pits durs &optional (treeflag nil))
  (if (numberp (car pits))
      (if (> durs (length pits))
	  (cons (+ 1 (- durs (length pits))) (copylist '(1) (- (length pits) 1))))
      (if (eql (length pits) (length durs))
	  (let ((final
		 (map 'list (lambda (x y) (dnbeats x y)) pits durs)))
	    (if treeflag final (flatten final))))))

;; LAYOUT -- insert random rests in melody
;; (layout .5 '(50 40 30 20 10)) =  (R 40 30 R R)
(defun layout (factor pitlist)
        (loop for x in pitlist collect
              (odds factor x 'r)))

;; MENSES -- mensuration by inserting rests btwn members
;; (menses '(1 2 3 4) 3) = (1 R R 2 R R 3 R R 4 R R)
(defun menses (a-list rate)
  (flatten 
   (loop for x in a-list collect 
         (append (list x)
               (loop repeat (- rate 1) collect 'r)))))

;; POPULATE -- randomly add a list to a list (both in sequence)
;; (populate '(2 3 4 5) '(10 20 30 40 50 60 70 80))
;; (10 20 2 30 40 3 50 4 5 60 70 80)
(defun populate (nuthings oldthings)
  (let* ((totlen (+ (length nuthings) (length oldthings)))
         (pushidx (heapvec (length nuthings) totlen))
         (nucopy nuthings)
         (oldcopy oldthings))
    (loop for x from 0 to (- totlen 1) collect
          (if (member x pushidx) (pop nucopy) (pop oldcopy)))))

;; ADD-RESTS -- add rests to end, to sum to 'newlen'
(defun add-rests (alist newlen)
  (append alist 
	  (copylist (list 'r) (- newlen (length alist)))))

;; RANDRESTS -- randomly add rests
;; (randrests '(1 2 3 4 5) 10) = (1 R 2 R R R 3 4 5 R)
(defun randrests (inlist totalen)
  (let ((restvec (loop repeat (- totalen (length inlist)) collect 'r)))
    (populate restvec inlist)))

;; RANDURS -- quantized ransegs
;; density = avg # atx per beat 
(defun randurs (totalen &optional (density 1) (qlevels '(3 4)))
  (quantdurs
   (melint
    (rescale-all (ransegs (round (* totalen density))) 0 1 0 totalen))
   (ferney '(1) (next (new weighting :of qlevels) totalen))))

;; DIVVY-UP -- simple utility used in 'ferney'
(defun divvy-up (mlen subdiv &optional (numtype 'float))
  (loop repeat subdiv collect 
        (if (eql numtype 'float) 
          (float (/ mlen subdiv))
          (/ mlen subdiv))))

;; FERNEY -- build list from mlens, subdivs, durs
;; now using modified 'ferneyrat' [Jan. 2006]
;; may specify rats or floats
;; cycling through everything
;; omitting durations will yield 'basic' mlens/subdivs list
;; Jan 2011: added 'treeflag'
;; (ferney '(2 3) '(1 2 4) '(2 4) 'float) = 
;; (3.5 3.0 3.5 3.5 1.5 5.5 1.0 5.5 1.5 5.0 2.0 4.5 2.0 3.0)
(defun ferney (mlens subdivs &optional (durs '(1)) (treeflag nil) (numtype 'rat))
  (let* ((mlens-cyc (new cycle of mlens))
         (subdivs-cyc (new cycle of subdivs))
         (durs-cyc (new cycle of durs))		
	 (ferntree
	  (loop until (and (eop? mlens-cyc)
			   (eop? subdivs-cyc))
	    collect (divvy-up (next mlens-cyc) 
			      (next subdivs-cyc)
			      numtype)))
         (flatcyc 
          (new cycle of
               (flatten ferntree))))
    (if treeflag ferntree
	(loop until (and (eop? flatcyc) (eop? durs-cyc))
	  collect (apply #'+ (next flatcyc (next durs-cyc)))))))

;; FERNCYC -- making a cycle of ferney
(defun ferncyc (mlens subdivs &optional (durs '(1)))
  (makecyc (ferney mlens subdivs durs)))

;; FERNEYS -- list of (nonrepeating) ferneys -- flat (default) or not flat!
(defun ferneys (len &optional (divs '(3 4)) (width 1) (treeflag nil))
  (let* ((divpatt (new weighting :of (norpt-rand divs)))
	 (rawout
	  (loop repeat len collect 
		(ferney width (next divpatt)))))
    (if treeflag rawout (flatten rawout))))

;; FERNLIST -- building list of ferneys from div list
(defun fernlist (alist &optional (width 1))
  (mapcar (lambda (div) (ferney width div))
	  alist))

;; SUM-ACROSS -- adds up members of baselist according to durlist
;; note: throws away any remainder of baselist
;; (sum-across '(1 2 1 2 1 2) '(2 3)) = (3 4)
(defun sum-across (baselist durlist)
  (let ((base-cyc (if (pattern? baselist) baselist (new cycle of baselist))))
    (loop for x in durlist collect
          (apply #'+ (next base-cyc x)))))

;; SUM-ACROSS-ALL -- adds up members of baselist according to durlist
;; note: uses all of baselist
;; Added September 2005
;; (sum-across-all '(1 2 1 2 1 2) '(2 3)) = (3 4 2)
(defun sum-across-all (baselist durlist)
  (let* ((mp (not-flat (make-poly baselist durlist))))
    (mapcar (lambda (x) (apply #'+ x)) mp)))

;; STRUMFIT -- provides upbeats for existing durlist
;; --> need to deal correctly with shorter durs in durlist
(defun strumfit (durlist max-upatx)
  (flatten (loop for dur in durlist
                 for chop = (min (+ 1 (random max-upatx)) (- dur 1))
                 collect
                 (if (> dur (+ chop 1))
                   (append (list (- dur chop))
                           (loop repeat chop collect 1))
                   dur))))

;; LCMDURS -- produces list of lists: 
;; durs summing to (lowest multiple of) a given qlevel [May 2009]
;; example: (lcmdurs '(1/2 .25 .375 .125 1/5) .25)
;; = ((1/2) (1/4) (3/8 3/8) (1/8 1/8) (1/5 1/5 1/5 1/5 1/5))
(defun lcmdurs (indurlist &optional (inqlevel .5))
  (let* ((indurs (mapcar #'rational indurlist))
	 (qlevel (rational inqlevel))
	 (ftact (fast-tact indurs))
	 (nbrf-q (rational (/ qlevel ftact)))
	 (hints (hits->ints indurs))
	 (mults 
	  (mapcar 
	   (lambda (x) (lcm x nbrf-q))
	   hints))) 
    (map 'list
	 (lambda (h m)
	   (copylist (list (* h ftact))
		     (/ m h)))
	 hints
	 mults)))

;; LCMDURS-LIST -- returns flat norpt-rand list of lcmdurs
;; 'repeatervec' applies repeater; default = 1
(defun lcmdurs-list (len durs qval &optional (repeatervec 1))
  (subseq 
   (flatten
    (repeater 
     (next
      (convert-into-randpatt
       (lcmdurs durs qval))
      len)
     repeatervec))
   0 len))

;; examples of possible rhyt-pairs
(defun updur () (pairlis '(0 1 2 3 4 5 6 7 8 9 10 11)
			'(1 1 1 2 2 2 3 3 3 4 4 4)))

(defun downdur () (pairlis '(0 1 2 3 4 5 6 7 8 9 10 11)
			'(4 4 4 3 3 3 2 2 2 1 1 1)))

(defun shuffdur () (pairlis '(0 1 2 3 4 5 6 7 8 9 10 11)
			(shuffle '(1 2 3 4 5 6 7 8 9 10 11 12))))

(defun randdur (maxdur)
  (let ((randloop (loop repeat 12 collect (+ 1 (random maxdur)))))
    (pairlis '(0 1 2 3 4 5 6 7 8 9 10 11) randloop)))

;; FIXRHYTHM -- set durations according to size of melodic interval
;; example: (melrhythm '(0 9 7 2 3 5) pitdur) = 
;; (0.75 0.25 0.5 0.25 0.25 1.0)
;; see also "leapdur" for a more probabalistic approach
(defun fixrhythm (a-melody rhyt-pairs)
  (let ((thismel (melint a-melody)))
    (append (loop for i in thismel
		  collect (cdr (assoc (mod12 (abs i)) rhyt-pairs)))
	    (list 1))))


;; midi example: random durations fixed by melodic interval
;(defun fixed-mel
;  (let* ((a-mel (randmel 100))
;	(a-rhylist (fixrhythm a-mel downdur)))
;  (process for i below (length a-mel)
;        output (new midi :time (now)
;                     :keynum (+ 60 (nth i a-mel)) 
;                     :duration (* .125 (nth i a-rhylist)))
;	wait (* .125 (nth i a-rhylist)))))

; (events fixed-mel "fixedmel.midi")

;; midi example: random durations fixed by pitch class
;(defun fixed-pc
;  (loop for key from 60 to 72 
;        for beg from 0 by .1
;        collect (new midi :time beg
;                     :keynum (
;                     :duration 1)))
;
;; store them in a midi file
; (events fixed-pc "fixedpc.midi")

;; FAST-TACT -- find fastest tactus from list of durations
;; i.e. rational gcd of rationals (not always the minimum)
;;;; (fast-tact '(1.2 1.1 1/2 5 2.9)) = 0.1
;;;; ...but also: (fast-tact '(10 15 30)) = 5
;; fixed June 2005 & Sept 2005
(defun fast-tact (a-list)
  (let* ((rats (loop for x in a-list collect (rationalize x)))
	 (denoms (loop for x in rats collect (denominator x)))
	 (maxden (apply #'lcm denoms)))
    (if (member-if-not (lambda (x) (= (floor x) x)) a-list)
      (/ 1 maxden)
      (apply #'gcd a-list))))

;; HITS->INTS -- takes rhythms (reals), 
;; converts to integers according to fastest-tact
;;;; (hits->ints '(.25 1.125 .5)) = (2 9 4)

(defun hits->ints (a-list)
  (loop for x in a-list collect (floor (/ x (fast-tact a-list)))))

;; SIFTOUT -- returns only those members in 'place mod'
;; (siftout '(1 2 3 4 5 6) 3 '(1 2)) = (1 2 4 5)
;; fixed July 2011
(defun siftout (a-list modlen places)
  (let* ((placeslist (if (listp places) places (list places)))
	 (idxlist (indices (length a-list))))
    (nths
     (no-nils
      (mapcar (lambda (x) (if (member (mod x modlen) placeslist) x)) idxlist))
     a-list)))

;; utilities for 'closest'

;; PAD-PITCHES -- utility for 'listdist'
;; returns 'padded' smaller chord in relation to larger chord
;; [pads with pitch in smallchd that's closest to avg in largechd]
(defun pad-pitches (smallchd largechd)
    (append smallchd
            (loop repeat (- (length largechd) (length smallchd))
              collect (neighbor (avg-chdpit largechd) smallchd))))

;; LISTDIST -- sum of distance between 2 lists
;; "city blocks" measurement if ordered, otherwise closest match
;; (listdist '(4 5 6) '(5 10 1)) = 7
;; (listdist '(4 5 6) '(5 10 1) 'ordered) = 11
;; can be different lengths, but best when unordered in that case
(defun listdist (pits1 pits2 &optional (ordered-flag nil))
  (let* ((inchd1 (if (listp pits1) pits1 (list pits1)))
         (inchd2 (if (listp pits2) pits2 (list pits2)))
         (chds (cond ((< (length inchd1) (length inchd2))
                      (list (pad-pitches inchd1 inchd2) inchd2))
                     ((> (length inchd1) (length inchd2))
                      (list inchd1 (pad-pitches inchd2 inchd1)))
                     (t (list inchd1 inchd2))))
         (list1 (first chds))
         (list2 (second chds)))
    (if ordered-flag
        (loop for n to (- (length list1) 1) sum
              (abs (- (nth n list1) (nth n list2))))
        (let* ((clperms (permutations list2)))
          (loop for p in clperms minimize
                (loop for n to (- (length list1) 1) sum
                      (abs (- (nth n list1) (nth n p)))))))))

;; LISTDIST-MOD -- sum of mod-intv distance 
;; ordered or unordered (June 2007)
(defun listdist-mod (lst1 lst2 &optional (ordered-flag nil) (modlen 12))
  (if ordered-flag
      (apply #'+ (map 'list (lambda (x y) (mod-intv x y modlen)) lst1 lst2))
      (apply #'min 
	     (mapcar
	      (lambda (r) (listdist-mod r lst2 'ordered modlen))
	      (permutations lst1)))))

;; CLOSEST -- orders list2 as the minimum total distance from list1
;; (closest '(4 5 6) '(5 10 1)) = (1 5 10)
(defun closest (list1 list2)
  (let* ((clperms (permutations list2))
	 (cldist (loop for x in clperms collect (listdist list1 x 'ordered))))
    (nth (position (apply #'min cldist) cldist) clperms)))

;; CLOSEST-MOD -- orders list2 as the minimum modlen distance from list1
;; (closest-mod '(11 7 4) '(7 4 0)) = (0 7 4)
(defun closest-mod (list1 list2 &optional (modlen 12))
  (let* ((clperms (permutations list2))
	 (cldist (loop for x in clperms collect 
                       (listdist-mod list1 x 'ord modlen))))
    (nth (position (apply #'min cldist) cldist) clperms)))

;; CLOSEST-MOD-LIST -- 'closest-mod' applied to entire list
(defun closest-mod-list (chdlist &optional (modlen 12))
  (if (eql 2 (length chdlist))
      (list (first chdlist) (closest-mod (first chdlist) (second chdlist) modlen))
    (cons (car chdlist)
	  (closest-mod-list
	   (cons (closest-mod (car chdlist) (cadr chdlist) modlen) 
		 (cddr chdlist))
	   modlen))))

;; NEXTCONS -- finds next consonance with y from x
;; in direction 'movement' (1=up,-1=down)
;; [changed June 2005]
;; (nextcons 8 10 1) = 11
(defun nextcons (y x movement &optional (oblq nil) (consvec '(3 4 8 9)))
  (let ((startx (+ x movement)))
    (if (and oblq (consn-p x y consvec)) x
    (if (consn-p y startx consvec) startx
      (nextcons y (+ x movement) movement oblq consvec)))))

;; NORPT -- removes repeated entries in flat list
;; (norpt '(2 3 3 4 3 5 5)) = (2 3 4 3 5)
;; now includes rests [January 2006]
(defun norpt (melody &optional (test #'eql))
  (loop with fragment
    for (note next-note) on melody
    do (setf fragment note)

    unless next-note
    collect fragment into result
    and do (loop-finish)

    when (not (funcall test next-note note))
    collect fragment into result
    and do (setf fragment nil)

    finally (return result)))

;; TIEVEC -- number of immediate repeats
;; for use with 'norpt' and 'sum-across'
; (tievec '(3 3 2 2 1 0 3 2 3 1)) = (2 2 1 1 1 1 1 1)
(defun tievec (alist)
  (let* ((eqlsubs (not-flat (conjunct-fragments alist 0))))
    (loop for x in eqlsubs collect
          (length x))))

; MAKE-TIES -- converting all repeats to ties
;; makes mel+dur pair .. feed into 'play-sd' to play
(defun make-ties (mel)
  (list (norpt mel)
	(tievec mel)))

;; CHDS->TIES -- takes chords; returns tievecs for each line
;; chords must all be the same length
(defun chds->ties (chdlist)
  (mapcar #'make-ties
	  (make-poly (alternate (mapcar #'safesort chdlist))
		     (list (length chdlist)))))
      
;; PLAY-TIES -- plays list of ties w/duration vec.
;; e.g. (play-ties '(((60 70) (2 1)) ((63 54) (1 4))) .5)
(defun play-ties (tlist indurs)
  (let* ((tilens (mapcar (lambda (x) (apply #'+ (second x))) tlist))
	 (maxlen (apply #'max tilens))
	 (durs (cond ((pattern? indurs) (next indurs maxlen))
		     ((listp indurs) (next (makecyc indurs) maxlen))
		     (t (copylist (list indurs) maxlen)))))
    (loop for tl in tlist collect
	  (splay (first tl)
		 (sum-across durs (second tl))))))

;; PLAYCHDS->LINES -- chds->lines->ties
(defun playchds->lines (chds durs)
  (play-ties
   (mapcar #'make-ties 
	   (chds->lines chds))
   durs))

;; REMOVE-RPTS
;; convert repeated notes to rests
;; (remove-rpts '((50 30) 30 (21 30 70) 21 40 40))
;; ((50 30) R (21 70) R (40) R)
(defun remove-rpts (inlist)
  (let ((a-list (mapcar (lambda (x) (if (listp x) x (list x))) inlist)))
    (cons (nth 0 a-list)
          (loop for r from 1 to (- (length a-list) 1)
                collect 
                (if 
                    (set-difference (nth r a-list) (nth (- r 1) a-list))
                  (set-difference (nth r a-list) (nth (- r 1) a-list))
                  'r)))))

;; STUTTER -- convert a note to repeats of same length
;; uses "endlen" to determine if last note should be the remainder ("short")
;; or added to the previous note ("long")
;; (stutter 65 3.5 1.0 'short) = ((65 65 65 65) (1.0 1.0 1.0 .5))
;; (stutter 65 3.5 1.0 'long) = ((65 65 65) (1.0 1.0 1.5))
(defun stutter (pit len stutlen &optional (endlen 'long))
  (if (>= stutlen len) 
    (list (list pit) (list len))
    (let* ((divs (mapcar #'float (multiple-value-list (floor len stutlen))))
           (divlist (loop repeat (car divs) collect (float stutlen)))
           (lenlist (if (eql endlen 'long) 
                      (append (butlast divlist) (list (+ stutlen (cadr divs))))
                      (if (= 0 (cadr divs))
                        divlist
                        (append divlist (cdr divs)))
                      )))
      (list (loop repeat (length lenlist) collect pit)
            lenlist))))

;; STUTLIST -- stutters all notes by single value or cycled list
;; input 'pitsndurs' = list of (pitslist durslist)
(defun stutlist (pitsndurs stutlen &optional (endtype 'long))
  (let* ((pits (first pitsndurs))
         (durs (second pitsndurs))
         (stutcyc (new cycle of stutlen))
         (pitcyc (new cycle of pits))
         (durcyc (new cycle of durs))
         (bigstut (loop repeat (length pits) collect
                        (stutter (next pitcyc) (next durcyc) (next stutcyc) 
                                 endtype))))
    (list (loop for x in bigstut append (first x))
          (loop for y in bigstut append (second y)))))


;; ------------ added since May 2005 ------------------

; IVEC -- construct interval vector
; example: (ivec '(0 1 2 3 5 6 7 9 10)) =  (6 6 7 7 7 3)
(defun ivec (alist)
  (let* ((pform (prime-form alist))
         (pairs (subsets-len pform 2))
         (intlist (loop for x in pairs
                        collect (mod-intv (first x) (second x)))))
    (cdr (loop for y to 6 collect (count y intlist)))))

;; TRANSPVEC -- returns list of all local transpositions
;; ordered from most to least invariant w/chord
;; 'subl' optional arg breaks into sublists by invariance
(defun transpvec (chd &optional (wsubl nil))
  (let* ((cvec (ivec chd))
         (maxi (apply #'max cvec))
         (mini (apply #'min cvec))
         (tvec (no-nils 
                (loop 
                    for x from maxi downto mini collect
                    (if (member x cvec)
                      (no-nils (flatten
                                (loop for y to 5 collect 
                                      (if (eql (nth y cvec) x)
                                        (list (+ y 1) (* -1 (+ y 1))))))))))))
    (if wsubl
      (loop for x in tvec collect
            (loop for y in x collect (transp chd y)))
      (loop for x in tvec append
            (loop for y in x collect (transp chd y))))))

;; MODSUM -- computing a mod sum
(defun modsum (x y &optional (modlen 12))
  (mod (+ x y) modlen))

;; SUMVEC -- interval sum vector of a chord
(defun sumvec (alist &optional (modlen 12))
  (let* ((pairs (append (copylist (subsets-len alist 2) 2)
                        (loop for x in alist collect (list x x))))
         (intlist (loop for x in pairs collect 
                        (modsum (first x) (second x) modlen))))
     (loop for y to (- modlen 1) 
           collect (count y intlist))))

;; ITRANSPVEC -- returns list of all local inversion transp
;; ordered from most to least invariant w/chord
;; 'subl' optional arg breaks into sublists by invariance
(defun itranspvec (chd &optional (wsubl nil))
  (let* ((cvec (sumvec chd))
         (maxi (apply #'max cvec))
         (mini (apply #'min cvec))
         (tvec (no-nils 
                (loop 
                    for x from maxi downto mini collect
                    (if (member x cvec)
                      (no-nils (flatten
                                (loop for y to 11 collect 
                                      (if (eql (nth y cvec) x) y)))))))))
    (if wsubl
      (loop for x in tvec collect
            (loop for y in x collect (transp (invert-dk chd) y)))
      (loop for x in tvec append
            (loop for y in x collect (transp (invert-dk chd) y))))))

;; ENTROPY
;; returns shuffled transpvec or itranspvec
;; can work nicely with 'smoothlist', 'make-poly', etc.
(defun entropy (vec &optional (iflag nil))
  (cons (shuffle vec)
          (loop for x in 
                (mapcar #'shuffle 
                        (if iflag 
                          (itranspvec vec 'subl)
                          (transpvec vec 'subl)))
                append (loop for y in x collect (shuffle y)))))


; TPOINTS
; place (pitch) VEC into PLACES within tp of length TPLEN
; exits at end of PLACES (PLACES count from 0, not 1)
; (tpoints '(.3 .4 .5 .6) '(3 2 4 1) 4) 
;    = (R R R 0.3 R R 0.4 R 0.5 0.6 R R)
(defun tpoints (vec places &optional (tplen 12))
  (let ((indcyc (makecyc (indices tplen)))
        (modplaces (mapcar (lambda (y) (mod y tplen)) places)))
    (append 
     (loop for x in modplaces append
           (loop until (eql (next indcyc) x)
             collect 'r)
           collect (pop vec))
     (loop repeat (- tplen (+ 1 (car (last modplaces))))
       collect 'r))))

;; PLACE-SLOTS -- placing into indexed slots [rests otherwise] 
;; Sept. 2008: handles (equal length) lists for 'idx' & 'val'
(defun place-slots (idx val &optional (len 12))
  (if (numberp idx)
      (loop for n to (- len 1) collect (if (eql idx n) val 'r))
      (flatter
       (merge-slots
	(map 'list (lambda (a b) (place-slots a b len))
	     idx val)))))

;; DURS->TPVEC -- takes durations (integers), produces positions mod 'tplen'
;; 'startplace' = offset from 0 mod tplen
;; (durs->tpvec '(3 6 4 9)) = (0 3 9 1 10)
(defun durs->tpvec (int-list &optional (startplace 0) (tplen 12))
  (mapcar (lambda (y) (mod (+ y startplace) tplen)) (melint->line 0 int-list)))

;; SLOTS->TPOINTS -- returns tpoint positions from slotlist
; (slots->tpoints '(r r 3 r r 1 r 2 r 0 0 1 r) 3) = (2 2 1 0 1 2)
(defun slots->tpoints (slotvec &optional (modlen 12))
  (no-nils
   (loop for x to (- (length slotvec) 1) collect
         (if (not (eql 'r (nth x slotvec))) (mod x modlen)))))

;; CONJUNCT-FRAGMENTS -- makes sublists of conjunct elements, 
;; separated by leaps larger than 'jumpsize'
;; thanks Kenny Tilton!
;; can work with rests [January 2006]
(defun conjunct-fragments (melody &optional (jumpsize 12))
  (like-flat
   (loop with fragment
     for (note next-note) on melody
     do (push note fragment)
     unless next-note
     collect (nreverse fragment) into result
     and do (loop-finish)
     when 
     (or
      (flet ((stake (x)		; end frag when changing to/from rests
	       (if (listp (type-of x)) (car (type-of x)) (type-of x))))
	(not (eql (stake next-note) (stake note))))
      (and (numberp note) (numberp next-note)
	   (> (abs (- next-note note)) jumpsize)));end when exceeding jump size
     collect (nreverse fragment) into result
     and do (setf fragment nil)
     finally (return result))))

;; GATHER-PITS -- makes sublists 'gathered' by binary function
;; can work with rests
(defun gather-pits (binaryfunc melody)
  (like-flat
   (loop with fragment
     for (note next-note) on melody
     do (push note fragment)
     unless next-note
     collect (nreverse fragment) into result
     and do (loop-finish)
     when 
     (or
      (flet ((stake (x)		; end frag when changing to/from rests
	       (if (listp (type-of x)) (car (type-of x)) (type-of x))))
	(not (eql (stake next-note) (stake note))))
      (and (numberp note) (numberp next-note)
	   (not (funcall binaryfunc note next-note))))
     collect (nreverse fragment) into result
     and do (setf fragment nil)
     finally (return result))))

;; PARSE-BY-REG -- splitting melody into slotlist by register
;; arbitrary range divisions (octsize)
;; June 2007: melody can include rests
;; (parse-by-reg '(60 2 30 31 61 62)) 
; = ((R 2 R R R R) (R R 30 31 R R) (60 R R R 61 62))
(defun parse-by-reg (melody &optional (octsize 12) (base 0))
  (let* ((mel-octs 
          (mapcar 
	   (lambda (x) (if (eql x 'r) 'r (floor (/ (- x base) octsize)))) 
	   melody))
         (allocts (remove-duplicates (safesort (norests mel-octs)))))
    (loop for x in allocts collect
          (loop for y to (- (length mel-octs) 1) collect
                (if (and (numberp (nth y melody)) 
			 (eql (nth y mel-octs) x)) 
		    (nth y melody) 'r)))))

; RANDSTEPS -- random melody from "startpit" of "length", stepsizes 1 & 2
(defun randsteps (startpit length &optional (lobound 0) (highbound 128))
  (let ((steprange (new range :from startpit :to highbound :downto lobound
                        :stepping (new weighting :of '(-2 -1 1 2)))))
    (next steprange length)))

; RANDSTEPS-SINGLE -- random melody from "startpit" of "length", 
; stepsize 1 only
(defun randsteps-single (startpit length &optional (lobound 0) (highbound 128))
  (let ((steprange (new range :from startpit :to highbound :downto lobound
                        :stepping (new weighting :of '(-1 1)))))
    (next steprange length)))

;; SECTIONS -- breaks up vals into sublists by 'slower' duration cycle
;; inserts 'nil' for sections without attacks
(defun sections (vals durs changedurs)
  (let* ((changeatx (melint->line 0 changedurs))
         (duratx (melint->line 0 durs))
         (changeidx (loop for x in duratx collect
                          (inbetween x changeatx)))
         (rdcd (remove-duplicates changeidx))
         (sec-count (loop for idx to (- (length changedurs) 1) collect 
                          (if (member idx rdcd) (count idx changeidx) 0))))
    (make-poly vals sec-count)))

;; SMOOTH -- takes two lists, makes three
;; by set difference w/ intersection in middle 
;; if no intersection, just returns the two lists
;; July 2008: now accepts 'lists of lists' (tests for 'list-eql')
;; (smooth '(1 2 3 4) '(3 4 5 6)) = ((1 2) (3 4) (5 6))
(defun smooth (list1 list2)
  (let ((eql-func (if (numberp (car list1)) #'eql #'list-eql)))
    (no-nils  (list (set-difference list1 list2 :test eql-func)
		    (intersection list1 list2 :test eql-func)
		    (set-difference list2 list1 :test eql-func)))))

;; SMOOTHLIST -- smooth applied to all successive members
;; of a list
(defun smoothlist (alist)
  (case (length alist) 
    (1 alist)
    (2 (smooth (first (last alist 2)) (car (last alist))))
    (t
     (let ((smb (smoothlist (butlast alist))))
       (append (butlast smb) 
	       (smooth (car (last smb)) 
		       (car (last alist))))))))

;; REMOVE-REDUNDANCY -- removes all duplicated instances in list-of-lists
;; keeps first instance
(defun remove-redundancy (listoflists)
  (let* ((numlists (length listoflists)))
    (cons (car listoflists)
	  (loop for n from 1 to (- numlists 1) 
		collect
		(remove-if
		 (lambda (x) (member x
				     (loop for s in 
					    (indices n)
					   append
					   (intersection (nth n listoflists) 
							 (nth s listoflists)))))
		 (nth n listoflists))))))

;; ALLTRANSP -- all transpositions of a vector within mod
;; returns list of lists
(defun alltransp (alist &optional (modlen 12))
  (loop for x to (- modlen 1) collect
        (mapcar (lambda (s) (mod s modlen)) (transp alist x))))


;; ALTERNATE -- alternate among members of list of lists
;; arbitrary number of lists
;; terminates with shortest list
(defun alternate (lists)
  (let ((minlen 
         (reduce #'min
                 (mapcar #'length lists))))
    (loop for x to (- minlen 1) append
          (loop for st in lists collect (nth x st)))))

;; CHAIN-CONTOUR-UTIL -- enjambs & starts list2 where list1 ends
(defun chain-contour-util (list1 list2)
  (append (butlast list1) (transp-to (car (last list1)) list2)))

;; CONTOUR-CHAIN -- makes one big contour from list
;; resets to 0 as min
(defun contour-chain (contourlist)
  (let ((rawlist 
   (if (eql (length contourlist) 2)
     (chain-contour-util (first contourlist) (second contourlist))
     (chain-contour-util (contour-chain (butlast contourlist))
                         (car (last contourlist))))))
    (transp rawlist (* -1 (apply #'min rawlist)))))

;; DIRECTIONS -- returns 1/-1 indices for melodic up/down
;(directions '(2 3 9 6 1 3)) = (1 1 -1 -1 1)
(defun directions (melody)
  (let ((mymelint (melint melody)))
    (loop for x in mymelint collect
          (if (eq x (abs x)) 1 -1))))

;; AU-CONTRAIRE -- makes consonant contrary-motion counterline 
;; uses oblique & consvec options, passed to "consn-p" utility
(defun au-contraire (melody duxpoint &optional 
                            (oblq nil) (consvec '(3 4 8 9)))
  (let* ((mdirections
          (mapcar (lambda (x) (* x -1)) (directions melody)))
         (meltop (first melody))
         (starter (if (consn-p meltop duxpoint consvec)
                    duxpoint
                    (nextcons meltop duxpoint (pick 1 -1) oblq consvec))))
    (if (eql (length melody) 2)
      (cons starter 
            (list (nextcons (second melody) starter 
                            (car mdirections) oblq consvec)))
      (let ((last-ac (au-contraire (butlast melody)
                                   starter oblq consvec)))
        (append last-ac
                (list (nextcons (car (last melody))
                                (car (last last-ac))
                                (car (last mdirections))
                                oblq consvec)))))))

;;;;;;;;;; working with processes & sprouts

;; LEN-EQL -- simple utility
;; returns two lists, each of min length btwn the two
;; returns 'nil' if either is not a list
(defun len-eql (list1 list2)
  (if (and (listp list1) (listp list2))
    (let ((minlen (min (length list1) (length list2))))
      (list (subseq list1 0 minlen)
            (subseq list2 0 minlen)))))

;; PLISTS -- utility to get pits & durs to match
;; returns list: (pits durs sum-of-durs) 
(defun plists (inpits indurs)
  (let* ((pitlen (length inpits))
         (durs (cond ((pattern? indurs) (next indurs pitlen)) 
		     ((listp indurs) (next (makecyc indurs) pitlen))
		     (t  (copylist (list indurs) pitlen)))))
  (list inpits durs (apply #'+ durs))))

; deprecated
;; PLISTS -- utility to get pits & durs to match
;; returns list: (pits durs sum-of-durs) 
;(defun plists (inpits indurs)
;  (let* ((minlists (len-eql inpits indurs))
;         (pitlen (length inpits))
;         (pits (if minlists (first minlists) inpits))
;         (durs (cond (minlists (second minlists))
;                     ((pattern? indurs) (next indurs pitlen))
;                     (t (copylist (list indurs) pitlen)))))
;    (list pits durs (apply #'+ durs))))

;; SPLAY-CMN -- export to cmn 
;; pits & durs only
(defun splay-cmn (inpits indurs)
  (let* ((pl (plists inpits indurs))
         (pits (first pl))
         (durs (second pl)))
    (process for x in pits
             for dur in durs
             output (multievent 'cmn :note
                                :note x
                                :time (now)
                                :duration dur)
             wait dur)))

;; SPLAY-PAIR -- for convenience 
;; 'splay's pit/dur list
(defun splay-pair (spair &optional (chan 0))
  (splay (first spair) (second spair) chan))

;; SPLAY-FB - play forwards, then backwards
;; durs may be list, pattern, or simple value
;; "midrest" = length of rest in middle (default 0)
(defun splay-fb (inpits indurs &optional (midrest 0) (chan 0))
  (let* ((pl (plists inpits indurs))
         (pits (first pl))
         (durs (second pl)))
    (process for x in (append pits (list 'r) (reverse pits))
             for dur in (append durs (list midrest) (reverse durs))
             output (multievent 'midi :keynum
                                :channel chan
                                :keynum x
                                :time (now)
                                :duration dur)
             wait dur)))


;; TPLAY -- plays triple-list: pits, durs, atx
(defun tplay (pitsdursatx &optional (chan 0))
  (let* ((pits (first pitsdursatx))
         (durs (second pitsdursatx))
	 (atkpts (third pitsdursatx)))
    (process for x in pits
             for dur in durs
	     for atk in atkpts
             output (multievent 'midi :keynum
                                :channel chan
                                :keynum x
                                :time atk
                                :duration dur))))

;; TPLAY-FB -- plays triple-list, fwds & bkwds
(defun tplay-fb (pitsdursatx &optional (midlen 0) (chan 0))
  (let* ((pits (first pitsdursatx))
         (durs (second pitsdursatx))
	 (atkpts (third pitsdursatx))
	 (endpt (+ (car (last durs)) (car (last atkpts))))
	 (rvsatx
	  (transp
	   (reverse
	    (mapcar (lambda (x) (- endpt x))
		    (map 'list #'+ durs atkpts)))
	   (+ endpt midlen))))
    (process for x in (append pits (list 'r) (reverse pits))
             for dur in (append durs (list midlen) durs)
	     for atk in (append atkpts (list endpt) rvsatx)
             output (multievent 'midi :keynum
                                :channel chan
                                :keynum x
                                :time atk
                                :duration dur))))

;; VPLAY -- plays pits/durs + velocity
;; durs may be list, pattern, or simple value
;; velocity may be list, pattern, or simple value
(defun vplay (inpits indurs vel &optional (chans 0))
  (let* ((pl (plists inpits indurs))
         (pits (first pl))
         (durs (second pl))
	 (chanpatt (if (pattern? chans) chans (makecyc chans)))
	 (velpatt (makepatt vel)))
    (process for x in pits
             for dur in durs
	     for chan in (next chanpatt (length pits))
             output (multievent 'midi :keynum
                                :channel chan
                                :keynum x
				:amplitude (next velpatt) 
                                :time (now)
                                :duration dur)
             wait dur)))

;; VELSPLAY -- plays pits/durs + velocity
;; durs may be list, pattern, or simple value
;; 'velfunc' = function to get velocity from each pit & its dur: "(vp p d)"
(defun velsplay (inpits indurs velfunc &optional (chans 0))
  (let* ((pl (plists inpits indurs))
         (pits (first pl))
         (durs (second pl))
	 (chanpatt (if (pattern? chans) chans (makecyc chans)))) 
    (process for x in pits
             for dur in durs
	     for thisvel = (funcall velfunc x dur)
	     for chan in (next chanpatt (length pits))
             output (multievent 'midi :keynum
                                :channel chan
                                :keynum x
				:amplitude thisvel
                                :time (now)
                                :duration dur)
             wait dur)))

;; PLAY-SD -- quick way to play output from slots->durs
(defun play-sd (slotsdurs basedur &optional (chan 0))
  (splay (first slotsdurs)
	 (sum-across basedur (second slotsdurs))
	 chan))

;; CLONE - copies section from midifile-vec, returns pitsdursatx triple-list
(defun clone (midivec startime endtime)
  (let* ((minatx (third midivec))
	 (clonelength (- endtime startime))
	 (startpos
	  (position-if (lambda (x) (>= x startime)) minatx))
	 (endpos (position-if (lambda (x) (>= x endtime)) minatx))
	 (rawpits (subseq (first midivec) startpos endpos))
	 (rawdurs (squeezedurs (subseq (second midivec) startpos endpos) 
			       clonelength))
	 (rawatx (transp (subseq minatx startpos endpos) (* -1 startime)))
	 (startdiff (- (nth startpos minatx) startime)))
    (if (plusp startdiff)
	(list
	 (cons 'r rawpits)
	 (cons startdiff (squeezedurs rawdurs (- (sum rawdurs) startdiff)))
	 (cons 0 rawatx))
	(list rawpits rawdurs rawatx))))

;; CLONES -- makes 'shuffled' tplay vector from midifile
;; sections = (startime endtime)
(defun clones (midifile sections)
  (let* ((min (numidi-in midifile))
	 (rawclones
	  (transpose-matx 
	   (loop for s in sections collect 
		 (clone min (first s) (second s)))))
	 (starts
	  (butlast
	   (melint->line 0
			 (mapcar 
			  (lambda (x) (- (second x) (first x)))
			  sections)))))
    (mapcar #'flatter
	    (list  (first rawclones)
		   (second rawclones)
		   (map 'list #'transp (third rawclones) starts)))))

;; NOTE: pass "-E 'I'patchno/[chan+1]" to timidity to get polyphony

;; PITSEQ -- a separate utility for pits only
(defun pitseq (pits levels)
  (loop for x in levels append
        (transp pits x)))

;; SPSEQUENCE -- returns pits/durs at various transp levels (in order)
(defun spsequence (inpits indurs levels)
  (let* ((pl (plists inpits indurs))
         (pits (first pl))
         (durs (second pl))
         (seqlen (third pl)))
    (process for level in levels
             sprout (splay (transp pits level) durs)
             wait seqlen)))

;; SQUEEZEDURS -- truncates & otherwise adjusts "durs" to sum to "len"
(defun squeezedurs (durs len)
  (let* ((sublens (cdr (melint->line 0 durs)))
         (shortlist (loop for x to (- (length durs) 1)
                          until (>= (nth x sublens)
                                    len)
                          collect (nth x durs)))
         (durlen (car (last sublens))))
    (cond 
      ((not shortlist) (list len));; len shorter than first duration
      ((>= len durlen);; len after last attack  
       (append (butlast durs) 
               (list (+ (car (last durs)) (- len durlen)))))
      (t (append shortlist
                 (list (- len (apply #'+ shortlist))))))))

;; PATT-TO-SUM -- return sequence of pattern's "next"s that sum to len
; (patt-to-sum (makecyc '(2 3)) 9) = (2 3 2 2)
;; added Oct. 2006
(defun patt-to-sum (patt len)
  (let ((rawvec
	 (loop 
	     for dur = (next patt)
	     collect dur into durvec
	     sum dur into dursum
	     while (< dursum len)
	     finally (return durvec))))
    (squeezedurs rawvec len)))

;;; FRAG -- returns a fragment
(defun frag (pits durs len &optional (chan 0))
  (let* ((sdurs (squeezedurs durs len))
         (spits (subseq pits 0 (length sdurs))))
    (process for x in spits
             for dur in sdurs
             until (= (now) len)
             output (multievent 'midi :keynum
                                :keynum x
                                :time (now)
				:channel chan
                                :duration dur)
             wait dur)))

; FRAGS -- the main process (calls fragments)
(defun frags (pits durs lengths &optional (chan 0))
  (process for len in lengths
           sprout (frag pits durs len chan)
           wait len))

;; ISO -- returns pits/durs in isorhythm
(defun iso (pits durs)
  (process 
    with pitcyc = (makecyc pits)
    for dur in (copylist durs
                         (/ (lcm (length pits) (length durs))
                            (length durs)))
    output (multievent 'midi :keynum
                       :keynum (next pitcyc)
                       :time (now)
                       :duration dur)
    wait dur))

;; TROPE -- interrupts process to insert trope on "test"
;; trope begins simultaneously with 'passed' test
;; "testpd" defaults to testing on current pit [otherwise dur]
(defun trope (inpits indurs tropits trodurs test &optional (testpd nil))
  (let* ((pl (plists inpits indurs))
         (pits (first pl))
         (durs (second pl)))
    (process for pit in pits
             for dur in durs
             if (funcall test (if testpd dur pit)) 
             sprout (splay (list pit) dur)
             and sprout (splay tropits trodurs)
             and wait (- (max dur (apply #'+ trodurs)) dur)
             else
             sprout (splay (list pit) dur)
             wait dur)))

;; DURWEIGHT -- making durvec from pitvec (chords last longer)
(defun durweight (pitvec &optional (basedur 1))
  (if (pattern? basedur)
    (loop for x in pitvec collect
          (apply #'+ (next basedur (if (listp x) (length x) 1))))
    (loop for x in pitvec collect
          (* (if (listp x) (length x) 1) basedur))))

;; LEAPDUR -- making durvec from pitvec (leaps last longer)
;; added January 2006
(defun leapdur (pitvec &optional (basedur 1) (melscale 1))
  (let* ((melin (mapcar #'abs (melint pitvec)))
         (mults
          (loop for m in melin collect 
                (cond ((< m (* melscale 3)) 1)
                      ((< m (* melscale 5)) (pick 1 2))
                      (t (pick 2 3))))))
    (if (pattern? basedur)
        (loop for x in mults collect
              (apply #'+ (next basedur x)))
        (transp mults basedur #'*))))

;; FILTER -- returns list according to "test"
;; failed entries removed
(defun filter (etest alist)
  (no-nils (loop for n in alist collect
        (if (funcall etest n) n))))

;; FILTERPOS -- returns list of positions according to "test"
;; failed positions omitted 
(defun filterpos (etest alist)
  (no-nils (loop for n to (- (length alist) 1) collect
		 (if (funcall etest (nth n alist)) n))))

;; EXTRACT -- returns slotlist according to "test"
;; failed entries return 'r
(defun extract (etest alist)
  (loop for n in alist collect
        (if (funcall etest n) n 'r)))

;; EXTRACT-PAIRS -- returns slotlist according to "test"
;; failed entries return 'r
(defun extract-pairs (etest alist)
  (let ((places
	 (remove-duplicates
	  (loop for n to (- (length alist) 2) append
		(if (funcall etest (nth n alist) (nth (+ n 1) alist)) 
		    (list n (+ n 1)))))))
    (loop for n to (- (length alist) 1) collect 
	  (if (member n places) (nth n alist) 'r))))

;; SLOWLINE -- returns slotlist from list by cycled durvec
;; ['1' now allowed in 'durvec' - Aug. 2007]
(defun slowline (alist durvec &optional (offset 0))
  (let* ((shortlist (subseq alist offset (length alist)))
         (prefix (loop repeat offset collect 'r))
         (mp (not-flat (make-poly shortlist durvec))))
    (no-nils (append prefix
                     (loop for x in mp 
                           collect (car x)
                           append (fill (cdr x) 'r))))))

;; EXTRACT-LIST -- returns slots in a list that follow a sequence 
(defun extract-list (xlist biglist)
  (loop for p in biglist collect
	(if (eql p (car xlist)) (pop xlist) 'r)))

;; EXTRACT-PATT -- returns slots in a list that follow a pattern
(defun extract-patt (xpatt biglist)
  (extract-list  (next xpatt (length biglist)) biglist))

;; SUBLINEPITS -- ez macro for extract/slowline
(defmacro sublinepits (extracted) 
  `(first (slots->durs ,extracted)))

;; SUBLINEDURS -- ez macro for extract/slowline
(defmacro sublinedurs (extracted) 
  `(second (slots->durs ,extracted)))

;; APPLY-ACROSS -- apply to corresponding members of two lists
(defun apply-across (op list1 list2)
  (let* ((leneq (len-eql list1 list2))
         (leneq1 (first leneq))
         (leneq2 (second leneq)))
    (loop for x to (- (length leneq1) 1) collect
          (funcall op (nth x leneq1) (nth x leneq2)))))

;; ORNADURS -- divides (accelerates) durations for all sublists
;; June 2007: 'treeflag' added
(defun ornadurs (inpits indurs &optional (treeflag nil))
  (let* ((pl (plists inpits indurs))
         (pits (first pl))
         (durs (second pl)))
    (if treeflag
	(loop for n to (- (length pits) 1) collect
	      (let* ((nthpit (nth n pits))
		     (nthdur (nth n durs))
		     (nlen (if (listp nthpit) (length nthpit) 1)))
		(loop repeat nlen collect (/ nthdur nlen))))
	(loop for n to (- (length pits) 1) append
	      (let* ((nthpit (nth n pits))
		     (nthdur (nth n durs))
		     (nlen (if (listp nthpit) (length nthpit) 1)))
		(loop repeat nlen collect (/ nthdur nlen)))))))

;; some stepwise-chordal stuff

;; TRAVERSE-PTS -- utility for "fromto"
(defun traverse-pts (list1 list2)
  (loop for x to (- (length list1) 1) collect
        (screamer-user::all-btwn (nth x list1) (nth x list2))))

;; MATCH2LISTS -- returns all bipartite matches btwn 2 lists
; (match2lists '(1 (2 3)) '(5 6)) = ((1 5) (1 6) ((2 3) 5) ((2 3) 6))
;; added Feb. 2006
;; June 2006: "flatflag" combines sublists
;; [default = preserves sublists]
(defun match2lists (inlist1 inlist2 &optional (flatflag nil))
  (loop for x in inlist1 append
        (loop for y in inlist2 collect
              (if flatflag
                  (cond ((and (listp x) (listp y)) (append x y))
                        ((listp x) (append x (list y)))
                        ((listp y) (cons x y))
                        (t (list x y)))
                  (list x y)))))

;; ALL-COMBOS -- bipart-matches a series of lists
;; added Feb. 2006
; (all-combos '((9 2 3) (10 30) (5 6)) 'flat) = 
; ((9 10 5) (9 10 6) (9 30 5) (9 30 6) 
;  (2 10 5) (2 10 6) (2 30 5) (2 30 6)
;  (3 10 5) (3 10 6) (3 30 5) (3 30 6))
;; June 2006: added "flatflag"
(defun all-combos (inlists &optional (flatflag nil))
  (cond ((eql 1 (length inlists))
         inlists)
        ((eql 2 (length inlists))
         (match2lists (first inlists) (second inlists) flatflag))
        (t (match2lists 
            (if flatflag (car inlists)
                (loop for c in (car inlists) collect (list c)))
            (all-combos (cdr inlists) flatflag) 
            'flat))))

;; FROMTO -- all intermediate lists (btwn & incl) two lists of equal length
;; order is important! -- each member moves to the corresponding member
;; fixed Feb. 2006 to use 'all-combos' instead of 'do-iter'
(defun fromto (list1 list2)
  (all-combos
   (traverse-pts list1 list2)
   'flat))

;; STEP-INCREM -- single move between lists
;; utility for 'fromto-stepper'
(defun step-increm (slist elist)
  (let* ((listdiffs (map 'list #'- elist slist))
         (onlydiffidx (no-nils (loop for n to (- (length listdiffs) 1) collect
                  (if (not (eql 0 (nth n listdiffs))) n))))
         (stepidx (pickl onlydiffidx)))
    (loop for n to (- (length slist) 1) collect
          (if (eql n stepidx) (+ (nth n slist) (/ (nth n listdiffs)
                                                 (abs (nth n listdiffs))))
                                 (nth n slist)))))

; FROMTO-STEPPER -- more refined 'fromto'
; avoids retracing steps from 'slist' to 'elist'
; [randomly selected set of steps] 
; (fromto-stepper '(0 9) '(5 6)) 
;  = ((0 9) (1 9) (2 9) (3 9) (4 9) (4 8) (5 8) (5 7) (5 6))
(defun fromto-stepper (slist elist)
  (let* ((listdiffs (map 'list #'- elist slist))
         (absdiffs (loop for x in listdiffs collect (abs x)))
         (totdiff (apply #'+ absdiffs)))
    (cons slist
          (when (plusp totdiff)
            (fromto-stepper (step-increm slist elist) elist)))))

;; FROMTO-AND-BACK -- 'fromto-stepper' back & forth
(defun fromto-and-back (list1 list2)
  (append (fromto-stepper list1 list2)
	  (fromto-stepper list2 list1)))

;; EACH-DISTANCE -- list of distances btwn each member
(defun each-distance (list &optional (ordered-flag nil))
         (mapcar (lambda (x y) (listdist x y ordered-flag))
                 list (rest list)))

;; TOTAL-DISTANCES -- total distance btwn each successive member of a list
;; formerly 'city-blocks' - ordered or unordered
(defun total-distances (list &optional (ordered-flag nil))
  (apply #'+ (each-distance list ordered-flag)))

;; REPLACED BY 'EACH-DISTANCE' ABOVE
;; CITY-BLOCKS -- measure of difference btwn lists
;; returns a list of step-by-step differences
;; (defun city-blocks (list)
;;   (mapcar #'listdist list (rest list)))

;; BESTPATH -- finds permutation of a list with 
;; smoothest (city-block/stepwise) path from first to last entry
;; solved by brute force -- very slow for > 9 chords (10 chds = 30 min.)
;; note: this is a Traveling Salesman Problem (TSP) with
;; "city-blocks" as distance
;; see "TSP-CHORDS" for larger sets
(defun bestpath (alist &optional (ordered-flag nil))
  (let* ((list1 (first alist))
         (list2 (car (last alist)))
         (ftperms (loop for x in 
                        (permutations (butlast (cdr alist))) collect
                        (append (list list1) x (list list2)))))
    (first
     (sort ftperms
           (lambda (x y) (< (total-distances x ordered-flag)
                            (total-distances y ordered-flag)))))))

;; CONCORDE-EDGEWEIGHTS -- utility for TSP-CHORDS
;; returns edgeweight matrix -- for large TSP problems 
(defun concorde-edgeweights (chdlist)
  (let* ((rawlist
	  (loop for c in chdlist collect
		(loop for d in chdlist collect (listdist c d))))
	 (newmax
	  (* 2 (apply #'max (flatten rawlist)))))
    (cons 
     (cons 0 (copylist (list newmax) (length chdlist)))
     (mapcar (lambda (x) (cons newmax x)) rawlist))))

;; RUN-CONCORDE -- utility for TSP-CHORDS
;; runs "concorde.pl" & writes to output file
(defun run-concorde (input-file output-file)
    (uiop:run-program "concorde.pl" 
		     (list input-file)
;		   :if-output-exists :supersede
		   :output output-file))

;; TSP-CHORDS -- rearranges chordlist for minimal movement between chords
;; note: all chords are reshuffled (including first & last) for best fit
(defun tsp-chords (chdlist)
  (let* ((infilename
	  (concatenate 'string "/tmp/"
		       (string (symbol-name (gensym "inconc")))))
	 (outfilename
	  (concatenate 'string "/tmp/"
		       (string (symbol-name (gensym "outconc")))))
	 (writebm
	  (write-output-to-file (concorde-edgeweights chdlist) infilename))
	 (runbm
	  (run-concorde infilename outfilename))
	 (output-idxs
	  (flatten
	   (make-list-from-file outfilename))))
    (progn
      (uiop:run-program "rm" (list infilename))
      (uiop:run-program "rm" (list outfilename))
      (loop for n in (cdr output-idxs) collect (nth (- n 1) chdlist)))))

;; POPOUT -- removes random items from a list
;; utility for 'lenfit'
(defun popout (alist &optional (holes 1))
  (let ((noluck (random (length alist))))
    (if (eql holes 1)
      (no-nils (loop for n to (- (length alist) 1) collect
                     (if (eql n noluck) 'nil (nth n alist))))
      (popout (popout alist (- holes 1))))))

;; POPIN -- adds random repeat to a list
;; utility for 'lenfit'
(defun popin (alist &optional (fills 1))
  (let ((goodluck (random (length alist))))
    (if (eql fills 1)
      (loop for n to (- (length alist) 1) append
            (if (eql n goodluck)  
              (loop repeat 2 collect (nth n alist))
              (list (nth n alist))))
      (popin (popin alist (- fills 1))))))

;; FIT-LENGTH -- truncates or extends end to fit 'alist' to sum 'newlen'
(defun fit-length (alist newlen)
  (let* ((alsum (sum alist))
	 (blast (butlast alist))
	 (blsum (sum blast)))
    (if (< (sum alist) newlen) 
	(append blast (list (- newlen blsum)))
	(fit-length blast newlen))))

;; LENFIT -- randomly truncate or repeat entries to match len
;; note: first & last entries stay the same
(defun lenfit (alist len)
  (let ((innerlist (butlast (cdr alist)))
        (list-s (list (first alist)))
        (list-e (last alist))
        (lendiff (- len (length alist))))
    (cond ((minusp lendiff) 
           (append list-s (popout innerlist (- (length alist) len)) list-e))
          ((plusp lendiff)
           (append list-s (popin innerlist (- len (length alist))) list-e))
          (t alist))))

;; CONTAINSV -- T if chord contains subset of given prime-form 
;; "prime-only-flag" looks for only strict transpositions, not inversions
(defun containsv (chd tstform &optional (prime-only-flag nil))
  (let* ((ssets (subsets-len chd (length tstform))))
    (if prime-only-flag
      (intersection (alltransp tstform) 
                    (loop for x in ssets collect (mod12 x))
                    :test #'list-eql)
      (member (prime-form tstform) (mapcar #'prime-form ssets) 
              :test #'list-eql))))

;; PRIMEFILT -- filters list for pc vector
;; [vector may be smaller than matched chord]
;; "prime-only-flag"= T returns only strict transpositions, not inversions 
(defun primefilt (alist pcvec &optional (prime-only-flag nil))
  (no-nils (loop for chd in alist collect
                 (if (containsv chd pcvec prime-only-flag)
                   chd))))
;
;; REPEATER -- repeats each entry cyclically
;; (repeater (indices 5) '(2 3)) = (0 0 1 1 1 2 2 3 3 3 4 4)
(defun repeater (inlist rpter)
  (let* ((adj-rpter (if (and (listp rpter)
                             (< (length rpter) (length inlist)))
                      (makecyc rpter) rpter))
         (pl (plists inlist adj-rpter))
         (ins (first pl))
         (rps (second pl)))
    (loop for n to (- (length ins) 1) append
          (loop repeat (nth n rps) collect (nth n ins)))))


;; ------------ added since August 2005 ------------------

;; LISTSUB -- replaces 'olds' with 'news' in 'inlist' 
(defun listsub (news olds inlist)
  (sublis (pairlis olds news) inlist))

;; SHUFFLE-ALL -- shuffles all lists in 'alist'
(defun shuffle-all (alist)
  (mapcar #'shuffle (not-flat alist)))

;; RARPEGG -- play members of list w/no immediate repeats
(defun rarpegg (alist len)
  (let* ((idxheap (new heap of (indices (length alist))))
        (idxlist (next idxheap len)))
    (loop for x in idxlist collect (nth x alist))))

;; RAND-ARPEGG -- apply 'rarpegg' to all sublists
;; similar to 'arpegg' but selects randomly from sublists
;; 'lens' can be integer, list, or pattern
(defun rand-arpegg (alist lens)
  (let ((listvec (if (listp lens) (makecyc lens))))
    (loop for x in alist collect
          (rarpegg x (cond  
                       ((pattern? lens) (next lens))
                       ((listp lens) (next listvec))
                       (t lens))))))

;; SUMSORT -- sorts list of lists by sum
;; non-destructive
(defun sumsort (alist)
  (let ((pholder (loop for x in alist collect x)))
    (sort pholder (lambda (x y) 
                    (< (apply #'+ x) (apply #'+ y))))))

;; SUMSORT-DN -- descending 'sumsort'
;; non-destructive
(defun sumsort-dn (alist)
  (let ((pholder (loop for x in alist collect x)))
    (sort pholder (lambda (x y) 
                    (and (> (apply #'+ x) (apply #'+ y)))))))

;; TOPSORT -- sorts list of lists by highest member
;; non-destructive
(defun topsort (alist)
  (let ((pholder (loop for x in alist collect x)))
    (sort pholder (lambda (x y) 
                         (< (apply #'max x) (apply #'max y))))))

;; TOPSORT-DN -- descending 'topsort'
;; non-destructive
(defun topsort-dn (alist)
  (let ((pholder (loop for x in alist collect x)))
    (sort pholder (lambda (x y) 
                         (> (apply #'max x) (apply #'max y))))))

;; TOPSUMSORT -- sorts sums within each max [ascending]
(defun topsumsort (alist)
  (let* ((tsort (topsort alist))
         (maxies (remove-duplicates 
                  (loop for x in tsort collect (apply #'max x))))
         (tsubs (loop for x in maxies collect
                      (filter (lambda (lst) 
				(eql x (apply #'max lst)))
			      tsort))))
    (loop for ts in tsubs append (sumsort ts))))

;; TOPSUMSORT-DN -- sorts sums within each max [descending]
(defun topsumsort-dn (alist)
  (let* ((tsort (topsort-dn alist))
         (maxies (remove-duplicates 
                  (loop for x in tsort collect (apply #'max x))))
         (tsubs (loop for x in maxies collect
                      (filter (lambda (lst) 
				(eql x (apply #'max lst)))
			      tsort))))
    (loop for ts in tsubs append (sumsort-dn ts))))

;; --- MTSPACE -- 
;; rhythmic method for transitioning between subdivisions

;; building a graph as a function -- returns letters
;; initval = starting index as it appears in the graph (A=0, etc.)
(defun mtspacef (&optional (initval 0))
  (new graph :of                                   ; for 2&3:  initval
       `((A :id A :to ,(new weighting of '(B F H K))) ; 1          0
         (B :id B :to ,(new weighting of '(A C E)))   ; 2          1
         (C :id C :to ,(new weighting of '(B D)))     ; 4          2
         (D :id D :to ,(new weighting of '(C)))       ; 8          3
         (E :id E :to ,(new weighting of '(B F)))     ; 6          4

         (F :id F :to ,(new weighting of '(A E G)))   ; 3          5
         (G :id G :to ,(new weighting of '(F)))       ; 9          6

         (H :id H :to ,(new weighting of '(A I L)))   ; 1/2        7
         (I :id I :to ,(new weighting of '(J H)))     ; 1/4        8
         (J :id J :to ,(new weighting of '(I)))       ; 1/8        9

         (K :id K :to ,(new weighting of '(A L)))     ; 1/3        10
         (L :id L :to ,(new weighting of '(H K))))    ; 1/6        11
       :starting-node-index initval))

;; fast 'mtspace' only
;; initval = starting index as it appears in the graph (A=0, etc.)
(defun fastspace (&optional (initval 0))
  (new graph :of                                   ; for 2&3:  initval
       `((A :id A :to ,(new weighting of '(H K))) ; 1              0
         (H :id H :to ,(new weighting of '(I L)))   ; 1/2          1
         (I :id I :to ,(new weighting of '(J H)))     ; 1/4        2
         (J :id J :to ,(new weighting of '(I)))       ; 1/8        3
         (K :id K :to ,(new weighting of '(A L)))     ; 1/3        4
         (L :id L :to ,(new weighting of '(H K))))    ; 1/6        5
       :starting-node-index initval))

;; RHYTPAIRVEC -- constructions lookup vector for two subdivs
;; note: subdivs should be relatively prime
(defun rhytpairvec (int1 int2)
  (let ((num1 (min int1 int2))
        (num2 (max int1 int2)))
  (list 1 num1 (* num1 num1) (* num1 num1 num1) (* num1 num2)
        num2 (* num2 num2)
        (/ num1) (/ (* num1 num1)) (/ (* num1 num1 num1))
        (/ num2) (/ (* num1 num2)))))

;; MTRPTLEN -- multiplier for a fast subdivision
(defun mtrptlen (tactlen int1 int2)
  (let ((rpvec (rhytpairvec int1 int2)))
    (case (position tactlen rpvec) 
      (0 1)
      (1 1)
      (2 1)
      (3 1)
      (4 1)
      (5 1)
      (6 1)
      (7 int1)
      (8 int1)
      (9 int1)
      (10 int2)
      (11 (pick int1 int2)))))

;; MTS-SUBS -- makes vector of rhythms
(defun mts-subs (inlist rpvec)
  (listsub rpvec '(A B C D E F G H I J K L) inlist))

;; MT-RHYVEC -- constructs rhythm vector with random-length tactus areas
(defun mt-rhyvec (int1 int2 len &optional (mtinit 0))
  (let ((subs (mts-subs 
               (next (fastspace mtinit) len)
               (rhytpairvec int1 int2))))
    (loop for sub in subs append
          (loop repeat (* (mtrptlen sub int1 int2)
                          (+ 3 (random 5)))
            collect sub))))

;; MT-RHYAREAS -- constructs rhythm vector according to 
;; 'lens' = lengths of each section (integers)
(defun mt-rhyareas (int1 int2 lens &optional (mtinit 0))
  (let* ((lenslen (length lens))
         (subs (mts-subs
                (next (fastspace mtinit) lenslen)
                (rhytpairvec int1 int2))))
    (loop for n to (- lenslen 1) append
          (loop repeat (/ (nth n lens)
                          (nth n subs))
            collect (nth n subs)))))

;; 

;; EMBED --- transform each point into a transposed figure
;; adjusted Feb. 2006
; (embed '(2 1) '(4 3 2)) = ((6 5 4) (5 4 3))
(defun embed (targetmel figure &optional (flatp nil))
  (let ((raw (loop for x in targetmel collect (transp figure x))))
    (if flatp (flatten raw) raw)))

;; EXPAND -- uses any # of embedded figures
;; revised Feb. 2006
;;   (expand '((2 1) (5 6) (4 3 2))) 
;; = (((11 10 9) (12 11 10)) ((10 9 8) (11 10 9)))
(defun expand (lists &optional (flatp nil))
  (cond ((eql 1 (list-length lists)) (car lists))
        ((eql 2 (list-length lists)) (embed (car lists) (cadr lists) flatp))
        (t (embed (car lists) 
                  (expand (cdr lists)) 
                  flatp))))

;; SELF-EXPAND -- self-embeds melody by a 'factor' [idx]
;; fixed June 2007
(defun self-expand (mel indx &optional (flatp nil))
  (deepfunc (lambda (x) (transp x (* -1 (- indx 1) (car mel))))
	    (expand (loop repeat indx collect mel) flatp)))

;; contour stuff

;; LISTMEAN -- quick mean
(defun listmean (alist)
  (round (/ (apply #'+ alist) (length alist))))

;; CONTOUR-EQUIV -- determining equivalent contour 
;; by converting large durations into repeats
; (contour-equiv '(60 67 62) '(2 9 4)) = (60 67 67 62)
(defun contour-equiv (inpits inlens)
  (let* ((rptindx (hits->ints inlens))
         (lm (listmean rptindx))
         (lens (loop for x in rptindx collect
                     (+ 1 (floor (/ x lm))))))
    (loop for n to (- (length inpits) 1) append
          (loop repeat (nth n lens) collect (nth n inpits)))))

;; TAKE-SUBCONTOURS
;; returns two lists:
;; 1. contours of each selected sublist
;; 2. 'slower' contour of subcontour initval sequence
;(take-subcontours '(51 50 52 54 54 54 54 57 56 55) '(3))
; =  (((1 0 2) (0 0 0) (0 2 1) (0)) (0 1 1 2))
(defun take-subcontours (alist clens)
  (let* ((mp (make-poly alist clens))
         (minics (loop for x in mp collect
                       (take-contour x)))
         (minics-inits (loop for m in mp collect 
                             (car m))))
    (list minics (take-contour minics-inits))))

;; TAKE-NTN-CONTOUR
;; note-to-note "up/down" contour [M. Friedmann's CAS]
;; (take-ntn-contour '(5 4 9 2)) = (-1 1 -1)
(defun take-ntn-contour (alist)
  (let* ((mi (melint alist)))
    (loop for x in mi collect
          (cond ((plusp x) 1)
                ((minusp x) -1)
                (t 0)))))

;; GIVE-NTN-CONTOUR -- gives contour to melody
(defun give-ntn-contour (melody ntn-ctr &optional (modlen 12))
  (if (eql (length ntn-ctr) (- (length melody) 1))
      (let ((melt (mapcar (lambda (x) (mod x modlen)) (melint melody))))
	(melint->line (car melody)
		      (map 'list
			   (lambda (m c) (if (plusp c)
					     m
					     (* -1 (- modlen m))))
			   melt
			   ntn-ctr)))))

;; WAVY -- alternating +1/-1 list
(defun wavy (len &optional (updown nil))
  (transp
   (next (makecyc '(1 -1)) len)
   (if updown -1 1) #'*))

;; ZIGZAG -- applying 'wavy' contour to list 
(defun zigzag (mel &optional (updown nil) (modlen 12))
  (give-ntn-contour mel (wavy (- (length mel) 1) updown) modlen))

;; ZIGZAG-BY-SIZE -- change direction when larger than prev intvl
(defun zigzag-by-size (inpits)
  (let* ((absmel (mapcar #'abs (melint inpits)))
	 (dircyc (makecyc '(1 -1))))
    (give-ntn-contour 
     inpits
     (flatten
      (loop for mlist in 
	    (not-flat
	     (gather-pits 
	      (lambda (a b) (eql b 0))
	      (map 'list
		   (lambda (a b) (if (> a b) 1 0))
		   absmel (append (cdr absmel) (list 0)))))
	    collect (copylist (next dircyc) (length mlist)))))))

;; ZIGZAG-NARMOUR -- change direction when ic 6 or above
(defun zigzag-narmour (inpits)
  (let* ((absmel (mapcar #'abs (melint inpits)))
	 (dircyc (makecyc '(1 -1))))
    (give-ntn-contour 
     inpits
     (flatten
      (loop for mlist in 
	    (not-flat
	     (gather-pits 
	      (lambda (a b) (eql b 0))
	      (map 'list
		   (lambda (a b) (if (> a 5) 1 0))
		   absmel (append (cdr absmel) (list 0)))))
	    collect (copylist (next dircyc) (length mlist)))))))

;; note -- smooth transitions btwn contours:
;(bestpath
; (fromto '(-1 1 -1) '(1 1 0)))
;; OR
; (fromto-stepper '(-1 1 -1) '(1 1 0))



;; ntn->clist
;; [unique solution, but not the only possibility]
;; (ntn->clist '(-1 -1 -1 0 1 -1)) = (3 2 1 0 0 1 0)
(defun ntn->clist (antn)
  (take-contour (melint->line 0 antn)))

;; see NTN->CLISTS above [nondeterministic; returns all clists]

;; SAME-SHAPE
;; replicates the contour of "frag" in the domain of "alist"
;; returns list of lists
(defun same-shape (frag alist)
  (let* ((antn (take-ntn-contour frag))
         (srtdlist (remove-duplicates (safesort alist)))
         (binsize (length srtdlist))
         (all-clists (ntn->clists antn binsize)))
    (loop for cl in all-clists collect
          (listsub srtdlist (indices binsize) cl))))

;; C-HEIGHT -- height of a contour = highest index
(defun c-height (acontour)
  (+ 1 (apply #'max acontour)))

;; C-WIDTH -- width of a contour = (max)-(min)+1
;; (c-width '(5 3 4))
(defun c-width (acontour)
  (+ 1 (- (apply #'max acontour)
          (apply #'min acontour))))

;; C-DISTANCE -- total # of steps in a contour
; (c-distance '(5 2 3)) = 4
(defun c-distance (acontour)
    (apply #'+ (mapcar #'abs (melint acontour))))

;; SMOOTH->JUMPY
;; sorts contour list by ascending c-distance
(defun smooth->jumpy (cntrlist)
  (let ((pholder (loop for x in cntrlist collect x)))
    (sort pholder #'< :key #'c-distance)))

;; JUMPY->SMOOTH
;; sorts contour list by descending c-distance
(defun jumpy->smooth (cntrlist)
  (let ((pholder (loop for x in cntrlist collect x)))
    (sort pholder #'> :key #'c-distance)))

;; SORT-BY-WIDTH
;; sorts contour list by width
(defun sort-by-width (cntrlist)
  (let ((pholder (loop for x in cntrlist collect x)))
    (sort pholder #'< :key #'c-width)))

;; SORT-BY-START
;; sorts list of lists by first member
(defun sort-by-start (listlist)
  (let ((pholder (loop for x in listlist collect x)))
    (sort pholder #'< :key #'car)))

;; TEMPO-SHAPE -- returns 'mini' duration list
;; 'totlen' is assumed to be an integer
;; any remainder is given to the last div
(defun tempo-shape (divlist totlen)
  (let* ((divmult (floor (/ totlen (length divlist)))))
    (loop for x in divlist append
          (loop repeat divmult append
                (ferney '(1) (list x))))))

;; RESCLASSVEC -- building texture/stress vector from resclasses
;; (resclassvec 3 5)=(2 0 0 1 0 1 1 0 0 1 1 0 1 0 0)
(defun resclassvec (&rest rclasses)
  (let* ((rlcm (apply #'lcm rclasses))
         (lcmidcs (cdr (indices rlcm)))
         (rcvecs 
          (loop for rc in rclasses collect
                (cons 1
                      (loop for n in lcmidcs collect
                            (if (eql 0 (mod n rc)) 1 0))))))
    (loop for n to (- rlcm 1) collect
          (loop for rcv in rcvecs sum (nth n rcv)))))

;; MEL-STRESS -- uses 'stresslist' to determine whether to attack
;; makes slot list w/rests
;; "skewfactor" = 0 to 10; default 8 (rescales to .1--.9 spct)
;; *Note* stress 0 always becomes a rest
(defun mel-stress (mel stresslist &optional (skewfactor 8))
  (let* ((smax (apply #'max stresslist))
         (smin (apply #'min stresslist))
         (skewspread (* .05 skewfactor))
         (spctvec (loop for x in stresslist 
                        collect 
                        (if (eql x 0) 0
			    (rescale x smin smax
				     (- .5 skewspread)
				     (+ .5 skewspread)))))
         (spctcyc (new cycle of spctvec)))
    (no-nils
     (loop while mel collect
	   (if (odds (next spctcyc)) (pop mel) 'r)))))

;; PICK-STRESS -- uses 'stresslist' to determine whether to attack
;; makes slot list w/rests; picks from list for each attack
;; numvec = list of lists, picked numbers [high to low stress]
;; length of numvec should equal the number of different entries in list 
;;    (excluding zero)
;; "skewfactor" = 0 to 10; default 8 (rescales to .1--.9 spct)
;; *Note* stress 0 always becomes a rest
;; Aug. 2006: fixed 'numvec' stress order
(defun pick-stress (len numvec stresslist &optional (skewfactor 8))
  (let* ((smax (apply #'max stresslist))
         (smin (apply #'min stresslist))
         (skewspread (* .05 skewfactor))
         (srtedstress (safesort (remove-duplicates stresslist)))
         (non0srtedstress (remove-if #'zerop srtedstress))
         (rawspctvec 
          (loop for x in srtedstress
                collect (if (eql x 0) 0
                            (rescale x smin smax
                                     (- .5 skewspread)
                                     (+ .5 skewspread)))))
         (spctvec (if (eql (length non0srtedstress)
                           (length srtedstress))
                      (cons 0 rawspctvec)
                      rawspctvec))
         (stresscyc (new cycle of stresslist)))
    (loop for nxtcyc in (next stresscyc len) collect
          (if (odds (nth nxtcyc spctvec))
              (pickl (nth (position nxtcyc non0srtedstress) numvec))
              'r))))

;; POLY-STRESS -- uses 'stresslist' to determine size of chord
;; returns texture vector
;; "skewfactor" = 0 to 10; default 8 (rescales to .1--.9 spct)
;; *Note* stress 0 always becomes a rest
(defun poly-stress (mel stresslist &optional (skewfactor 8))
  (let* ((smax (apply #'max stresslist))
         (smin (apply #'min stresslist))
         (skewspread (* .05 skewfactor))
         (mellen (length mel))
         (spctvec (loop for x in stresslist 
                        collect 
                        (if (eql x 0) 0
                            (rescale x smin smax
                                     (- .5 skewspread)
                                     (+ .5 skewspread)))))
         (spctcyc (new cycle of spctvec))
         (polyvec (loop for nxtsp = (next spctcyc)
                        for sp = (if (odds nxtsp) 
                                     (round (random (+ nxtsp 2))) 
                                     0)
                        collect sp
                        sum sp into sum
                        until (>= sum mellen))))
    (make-poly mel polyvec)))

;;; Viertu's "Diachro-measure"

;; CHROM -- returns a list's chromatic factor
(defun chrom (alist)
  (let* ((srtdlist (safesort (remove-duplicates (mod12 alist))))
         (adjlist (append srtdlist (list (+ 12 (first srtdlist))))))
    (length (remove 1 (melint adjlist)))))

;; DIA -- returns a list's diatonic factor
(defun dia (alist)
  (let* ((srtdlist (safesort (remove-duplicates (mod12 (modmult alist 7 12)))))
         (adjlist (append srtdlist (list (+ 12 (first srtdlist))))))
    (length (remove 1 (melint adjlist)))))

;; DIACHROM -- returns a list's "diachromatic" ratio
(defun diachrom (alist)
  (/ (dia alist) (chrom alist)))

;; DIACHROM-FILT -- returns sublist of vectors that have "diachromval"
(defun diachrom-filt (alist diachromval)
  (let ((rawlist
	 (filter
	  (lambda (x) (eql (diachrom x) diachromval))
	  alist)))
    (if rawlist rawlist (list 'r))))

;; SAME-DIACHRON -- returns all vectors with same diachrom factor 
;; can choose lengths of output [defaults to same length]
(defun same-diachrom (alist &optional (lens (length alist)))
  (diachrom-filt (subsets-len (indices 12) lens) (diachrom alist)))

;; DIACHROM-SETS -- makes sublists based on equal diachrom factors
;; -> most consonant *first*!
(defun diachrom-sets (alist)
  (let* ((dcs (safesort (remove-duplicates (mapcar #'diachrom alist)))))
    (loop for dc in dcs collect
	  (no-nils (loop for a in alist collect (if (eql (diachrom a) dc) a))))))

;;; misc. utilities

;; NORPT-RAND -- constructs nonrepeating vector
;; for use with 'weighting' e.g., "(new weighting of (norpt-rand (indices 10)))"
(defun norpt-rand (alist)
  (loop for x in alist collect
        (list x :max 1)))

;; plotter stuff

;; CORRECT-PITLIST -- utility; converts 1-lists into nums
(defun correct-pitlist (pits)
  (loop for x in pits collect
        (if (or (numberp x) (eql x 'r)) 
            x
            (if (eql 1 (length x)) (car x) x))))

;; SPLOTTER -- simple plot of pits/durs
;; fixed to allow rests [September 2008]
(defun splotter (inpits indurs)
  (let* ((plst (plists inpits indurs))
         (outpits (correct-pitlist (first plst)))
         (outdurs (second plst))
	 (sdurs (slots->durs outpits))
	 (rawpits (first sdurs))
	 (plotpits (flatten rawpits))
	 (plotatx (repeater 
		   (butlast (melint->line 0 (sum-across outdurs (second sdurs))))
		   (take-poly rawpits))))
    (plotter :view :point 
	     :y-axis :keynum 
	     :x-axis (axis :seconds :increment 4)
	     (loop for p in plotpits
		   for a in plotatx
		   collect a collect p))))

;;; QUANTRANPLOT -- places pattern-pits into plot
(defun quantranplot (input-pits rsegs quants &optional (flat-output nil))
  (let* ((qd-out (quantran rsegs quants 'rp))
         (qdurs (first qd-out))
         (qpoly (second qd-out))
         (inpits (if (pattern? input-pits) 
                     (if flat-output
                         (next input-pits (- (length rsegs) 1))
                         (next input-pits (apply #'+ qpoly)))
                     input-pits))
         (cpits (if flat-output 
                    inpits
                    (make-poly inpits qpoly)))
         (outplist (plists cpits qdurs))
         (outdurs (squeezedurs (second outplist) (car (last rsegs))))
         (outpits (correct-pitlist (first outplist)))
         (plotpits (flatten outpits))
         (plotatx (repeater 
                    (butlast (melint->line 0 outdurs))
                    (take-poly outpits))))
    (plotter :view :point 
             :y-axis :keynum 
             :x-axis (axis :seconds :increment 4)
             (loop for p in plotpits
                   for a in plotatx
                   collect a collect p))))

;; PLOT1D -- plotting atk points (useful for randsegs)
(defun plot1d (indata)
  (plotter :view :point :x-axis :seconds
           (loop for d in indata 
                 collect d collect .1)))

;; HISTPLOT -- quick bar graph of a histogram
(defun histplot (histog)
  (let ((hlen (length histog)))
    (plotter :view :bar-and-point
    (loop for n to (- hlen 1)
          collect n
          collect (nth n histog)))))

;; GET-X -- lists x-values in a plotter instance
(defun get-x (aplot)
  (let* ((plotdata (plotter-data aplot)))
    (no-nils
    (loop for n to (- (length plotdata) 1) collect
          (if (eql 0 (mod n 2)) (nth n plotdata))))))

;; GET-Y -- lists y-values in a plotter instance
(defun get-y (aplot)
  (let* ((plotdata (plotter-data aplot)))
    (no-nils
    (loop for n to (- (length plotdata) 1) collect
          (if (eql 1 (mod n 2)) (nth n plotdata))))))

;; more utilities

;; SUMPATT -- returns all "(next patt)"s that sum up to len
;; Sept. 2006: added 'nosqueeze' flag
(defun sumpatt (len patt &optional (nosqueeze nil))
  (loop until (> sumval len)
    for nxtval = (next patt) 
    collect nxtval into cycval
    sum nxtval into sumval
    finally (return (if nosqueeze cycval
			(squeezedurs cycval len)))))

;; DURFUNC -- generic linear function, for writing curves etc.
;; duration input: can take number, list, or pattern
;; (durfunc 4 1 '(* 20 (sin x))) 
;; = (0.0 16.829418 18.185947 2.8224 -15.13605)
(defun durfunc (totalen points funct)
  (let ((atx (typecase points 
               (pattern (sumpatt totalen points))
               (list (sumpatt totalen (makecyc points))))))
    (if (numberp points)
        (loop for p to totalen by points collect
              (eval `((lambda (x) ,funct) ,p)))
        (loop for p in (melint->line 0 atx) collect
              (eval `((lambda (x) ,funct) ,p))))))

;; TIMEFUNC -- computes funct at single point, or set of points
; example: "(timefunc (transp (indices 200) .1 #'*) '(sin x))"
(defun timefunc (points funct)
  (if (numberp points)
      (eval `((lambda (x) ,funct) ,points))
      (loop for p in points collect (timefunc p funct))))

;; CURVED-PATH -- path from 'start' to 'end' over 'len' with 'curve-idx' steepness
;; curve-idx -> negative=curved from below; positive=curved from above
;; returns expression; used in conjunction with "timefunc" 
(defun curved-path (start end curve-idx totalen)
  (let ((cindx (if (plusp curve-idx) curve-idx (/ 1 (* -1 curve-idx)))))
    (if (> start end)
	`(- ,start
	  (* 
	   ,(- start end)
	   (expt (/ x ,totalen) ,cindx)))
	`(+ ,start 
	  (* 
	   ,(- end start)
	   (expt (/ x ,totalen) (/ 1 ,cindx)))))))

;; some quantizing & other rhythmic functions

;; FLOATS->RATS -- utility; converts floats to rats
;; takes input as scalar, list, or pattern
(defun floats->rats (input)
  (cond ((pattern? input)
         (progn
           (setf
            (first
             (pattern-data input)) 
            (mapcar #'rationalize (first (pattern-data input))))
           input))
        ((listp input) (mapcar #'rationalize input))
        (t (rationalize input))))

;; QUANTDURS -- quantizes durs by additive rhythms
;; returns list: ((quantized durations) (poly vector))
;; takes quantizer 'qval-in' as pattern, list, or scalar
;; NOTE: rounds up sum-of-durs to next integer to avoid floating-pt problems!
;; "give-polyvec" flag added Sept. 2006
;; more fixes Sept. 2006
;; nasty fractions eliminated February 2008
(defun quantdurs (indurs qval-in &optional (give-polyvec nil))
  (let* ((qval (floats->rats qval-in))
         (durs (floats->rats indurs))
         (qpatt (cond ((pattern? qval) qval)
                      ((listp qval) (makecyc qval))
                      (t (makecyc (list qval)))))
         (totdurs (ceiling (apply #'+ (floats->rats durs))))
         (pattdurs (sumpatt totdurs qpatt 'nosqueeze))
         (atx (butlast (melint->line 0 durs)))
         (pattatx (set-difference (butlast (melint->line 0 pattdurs)) (list totdurs)))
         (qatx (loop for x in atx collect 
                     (neighbor x pattatx)))
         (qatx-norpt (norpt qatx))
	 (qdurs (melint (append qatx-norpt (list totdurs)))))
    (if give-polyvec
	(list qdurs (tievec qatx))
	qdurs)))

;; QUANTDURS-PATT -- quantizes each dur separately
;; akin to 'sampling with replacement' [each time different]
;; takes pattern or list [list becomes cycle]
;; returns list: ((quantized durations) (poly vector))
;; [some occasional funniness at the end ..] 
(defun quantdurs-patt (durlist qlev-in)
  (let* ((qlev (floats->rats qlev-in))
         (alist (butlast (melint->line 0 durlist)))
         (qlevel (if (listp qlev) (new cycle :of qlev) qlev))
         (qatx
          (safesort
           (loop for x in alist collect (quantize x (next qlevel))))))
    (list
     (melint (norpt qatx))
     (tievec qatx))))

;; DIVVER -- simple utility, used in 'multquant'
;; all atx to 1 for a divnum
(defun divver (divnum &optional (bignum 1))
  (loop repeat divnum collect (/ bignum divnum)))

;; MULTQUANT -- quantize among multiple subdivs
;; returns durations ...simply builds resultant of total length 'bignum'
;; accepts atoms for 'divlist' [Aug. 2006]
;; (multquant '(2 3) 4) = (4/3 2/3 2/3 4/3)
(defun multquant (divlist &optional (bignum 1))
  (if (numberp divlist) 
      (divver divlist bignum)
      (let ((divs (mapcar (lambda (x) (divver x bignum)) divlist)))
	(resultant divs))))

;; MULTQUANT-ATX -- presents 'multquant' as atk-points
;; (multquant-atx '(2 3)) = (0 1/3 1/2 2/3 1)
(defun multquant-atx (divs &optional (bignum 1))
  (melint->line 0 (multquant divs bignum)))

;; QUANTRAN -- quantizing ranseg/explseg data
;; returns (durs poly) list 
(defun quantran (rsegs quants &optional (returnpoly nil))
  (let ((qtran (quantdurs (melint (floats->rats rsegs)) quants 'rp)))
    (if returnpoly qtran (first qtran))))

;;; QUANTRANPLAY -- places pattern-pits into poly
(defun quantranplay (input-pits rsegs quants &optional (flat-output nil))
  (let* ((qd-out (quantran rsegs quants 'rp))
         (qdurs (first qd-out))
         (qpoly (second qd-out))
         (inpits (if (pattern? input-pits) 
                     (if flat-output
                         (next input-pits (- (length rsegs) 1))
                         (next input-pits (apply #'+ qpoly)))
                     input-pits))
         (cpits (if flat-output 
                    inpits
                    (make-poly inpits qpoly)))
         (outplist (plists cpits qdurs))
         (outdurs (squeezedurs (second outplist) (car (last rsegs)))))
    (splay (first outplist) outdurs)))

;; ATKPTS -- build atkptlist from list, scalar, or pattern
;; includes optional initial offset
(defun atkpts (totalen indurs &optional (offset 0))
  (let* ((durpatt (cond ((pattern? indurs) indurs)
                        ((listp indurs) (makecyc indurs))
                        (t (makecyc (list indurs)))))
         (nuatk nil)
         (rawdurs (squeezedurs
                   (cons offset
                         (loop while (< lastatk totalen) do
                               (setf nuatk (next durpatt))
                               collect nuatk into outvec
                               sum nuatk into lastatk
                               finally (return outvec)))
                   totalen)))
    (butlast
     (melint->line 0 (if (> offset 0) rawdurs (cdr rawdurs))))))

;; ANY-LCM -- computes lcm for non-integer types
(defun any-lcm (&rest nums)
  (* (reduce #'lcm (hits->ints nums))
     (fast-tact nums)))

;; PREP-RHYTHMS -- utility to add an offset if omitted
;; also build in rationalize
(defun prep-rhythms (dlist)
  (let ((durlist (loop for x in dlist collect
                       (if (listp x) (mapcar #'rationalize x)
                           (rationalize x)))))
    (if (member-if #'listp durlist) 
        durlist
        (list durlist 0))))

;; RESULTANT -- resultant of arbitrary # of rhythms (w/offsets)
;; input durations *must* be lists! but may include only one value
(defun resultant (durlist)
  (let* ((adjlists (loop for dlist in durlist collect (prep-rhythms dlist)))
         (lens (mapcar (lambda (x) (apply #'+ (first x))) adjlists))
         (totoffset (reduce #'+ (mapcar #'second adjlists)))
         (totalen (rationalize (+ totoffset (reduce #'any-lcm lens))))
         (atx 
          (safesort (remove-duplicates
                     (loop for alist in adjlists append
                           (atkpts totalen (first alist) (second alist)))))))
    (melint (append atx (list totalen)))))

;; GENERIC-CHAIN -- interposes binary func btwn members of a list
(defun generic-chain (funcname alist)
  (let ((results (map 'list (lambda (a b) (funcall funcname a b))
		      alist (cdr alist))))
    (append
     (loop for n to (- (length results) 1)
	   collect (nth n alist)
	   collect (nth n results))
     (last alist))))

;; GENERIC-LINKS -- result of func btwn consec pairs in list
(defun generic-links (funcname alist)
  (map 'list (lambda (a b) (funcall funcname a b))
       alist (cdr alist)))

;; PAIR-RESULTANT -- convenience for 'resultant-chain'
(defun pair-resultant (a b)
  (resultant (list a b)))

;; RESULTANT-CHAIN -- resultant interposed between rhythms 
;; [not flat! - employ repeater for extended use]
(defun resultant-chain (durlist-list)
  (generic-chain #'pair-resultant durlist-list))

; ERASEDURS -- removes durs from a rhythm
(defun erasedurs (edurs basedurs &optional (use-lcm nil))
  (let* ((ed (prep-rhythms edurs))
         (bd (prep-rhythms basedurs))
         (dlens (if use-lcm 
                    (any-lcm (apply #'+ (first ed))
                             (apply #'+ (first bd)))
                    (apply #'+ (first bd))))
         (totoff (if use-lcm 
                     (+ (second ed) (second bd))
                     (second bd)))
         (bdlen (+ dlens totoff)))
    (melint
     (append (list 0)
             (safesort
              (set-difference
               (atkpts bdlen (first bd) (second bd))
               (atkpts bdlen (first ed) (second ed))))
             (list bdlen)))))

;; EXTRACTDURS -- extracts 'edurs' from 'basedurs' [intersection]
(defun extractdurs (edurs basedurs &optional (use-lcm nil))
  (let* ((ed (prep-rhythms edurs))
         (bd (prep-rhythms basedurs))
         (dlens (if use-lcm 
                    (any-lcm (apply #'+ (first ed))
                             (apply #'+ (first bd)))
                    (apply #'+ (first bd))))
         (totoff (if use-lcm 
                     (+ (second ed) (second bd))
                     (second bd)))
         (bdlen (+ dlens totoff)))
    (melint
     (append
      (safesort
       (intersection
        (atkpts bdlen (first bd) (second bd))
        (atkpts bdlen (first ed) (second ed))))
      (list bdlen)))))

;; AVOIDURS -- avoid common attacks
(defun avoidurs (pladur backdur &optional (use-lcm nil))
  (erasedurs backdur (resultant (list pladur backdur)) use-lcm))

;; RYTE -- playing percussion rhythms
(defun ryte (pits durs &optional (atx 100) (chan 9))
  (let ((properpits (if (listp pits) pits (list pits))))
    (splay (next (makecyc properpits) atx)
           (makecyc durs)
           chan)))


;; some others

;; CHDS->LINES -- changes chords to lines
;; note: all chords in 'chdlist' must be the same length
(defun chds->lines (chdlist)
        (let ((slist (mapcar #'safesort chdlist)))
          (loop for n to (- (length (car slist)) 1) collect
                (loop for chd in slist collect (nth n chd))))) 

;; FILT-BY-MELINT -- extracts only specified intervals from a melody
;; returns slotlist
(defun filt-by-melint (amel intvs)
  (let* ((mlints 
          (mapcar #'mod-intv amel (nthcdr 1 amel)))
         (ints (if (listp intvs) intvs (list intvs)))
         (melintpos
          (reverse
           (no-nils
            (loop for n to (- (length mlints) 1) collect 
                  (if (member (nth n mlints) ints) n)))))
         (mplaces
           (safesort
            (remove-duplicates
             (loop for x in melintpos append (list x (+ x 1)))))))
    (loop for n to (- (length amel) 1) collect
          (if (member n mplaces) (nth n amel) 'r))))

;;; CHOOSER -- returns members of 'alist' from list of indices 'idxs'
;; now treats rests -- Jan. 2008
;; now treats sublists -- July 2008
(defun chooser (idxs alist)
  (loop for n in idxs collect
	(if (listp n) (chooser n alist)
	    (if (eql n 'r) 'r
		(nth n alist)))))

;; POSITIONS -- returns indices in list where 'num' occurs
;; e.g. (positions 2 '(3 2 2 1 2 2 4 3 2)) = (1 2 4 5 8)
;; added Sept. 2006
(defun positions (num alist)
  (no-nils
   (loop for n to (- (length alist) 1) collect 
	 (if (eql num (nth n alist)) n))))

;; DRUNKVEC -- non-repeating 'drunk' vector
(defun drunkvec (startpit len &optional (stepsize 1))
  (loop repeat len
    for r = startpit then (drunk r stepsize :avoid 0)
    collect r))

;; AARON4PT -- builds 4pt texture using 'au-contraire'
;; based on Pietro Aaron's construction method
;; returns each voice [dux, super, bassus, altus] as (pits tievec)
(defun aaron4pt (dux b-pit a-pit s-pit &optional (consvec '(0 3 4 7 8 9)))
  (let* ((superius (au-contraire dux s-pit 'oblq consvec))
         (bassus (au-contraire superius b-pit 'oblq consvec))
         (altus (au-contraire bassus a-pit 'oblq consvec)))
    (loop for v in (list dux superius bassus altus)
          collect (list (norpt v) (tievec v)))))

;; AARON3PT -- builds 3pt texture using 'au-contraire'
;; based on Pietro Aaron's construction method
;; returns each voice [dux, super, bassus] as (pits tievec)
(defun aaron3pt (dux b-pit s-pit &optional (consvec '(0 3 4 7 8 9)))
  (let* ((superius (au-contraire dux s-pit 'oblq consvec))
         (bassus (au-contraire superius b-pit 'oblq consvec)))
    (loop for v in (list dux superius bassus)
          collect (list (norpt v) (tievec v)))))

;; "list>" - another way to sort lists (useful for leaps etc.)

;; LIST>PLACE -- utility for 'LIST>'
(defun list>place (list1 list2)
  (let ((lu (list>-util list1 list2)))
    (if lu 
        (if (> lu -1)
            lu
            (list>place (butlast list1) (butlast list2)))
        0)))

;; LIST>-UTIL -- utility for 'LIST>'
(defun list>-util (list1 list2)
  (if (and list1 list2)
      (let* ((slist1 (safesort list1))
             (slist2 (safesort list2))
             (last1 (car (last slist1)))
             (last2 (car (last slist2))))
        (cond ((> last1 last2) 0)
              ((< last1 last2) 1)
              (t -1)))))

;; LIST> -- comparing two lists -- predicate!
;; compares max, then each internal max, etc.
(defun list> (list1 list2)
      (eql 0 (list>place list1 list2)))

;; LIST<SORT -- lists sorted from small to large, as per 'list>'
(defun list<sort (lists)
  (reverse
   (sort lists #'list>)))

;; EVERY-PCTRANSP -- returns all pc-transp (& inversions) of a chd
(defun every-pctransp (chd)
  (let ((nf (normal-form (mod12 chd))))
    (remove-duplicates
    (mod12 (append (loop for x to 11 collect (transp nf x))
            (loop for x to 11 collect (transp (invert-chd nf) x))))
    :test #'list-eql)))

;; PCSUBSET -- flag
;;  1 = chd1 is contained in chd2
;; -1 = (invert-chd chd1) is contained in chd2
;; nil = chd1 not in chd2 
(defun pcsubset (chd1 chd2)
  (let* ((invidx (inverse-idx chd1))
         (int1 (melint (prime-form chd1)))
         (int1i (reverse int1))
         (int2 (melint (prime-form chd2)))
         (subs2 (subsequences int2 (length int1))))
    (cond ((member int1 subs2 :test #'seq-eql) (if (eql invidx 1) 1 -1))
          ((member int1i subs2 :test #'seq-eql) (if (eql invidx 1) -1 1))
          (t nil))))

;; NEAREST-PCFORM -- nearest pctransp to a given chd, regardless of length
;; (nearest-pcform '(60 63 65 66) '(0 1 4)) = (62 65 66)
(defun nearest-pcform (chd pcform &optional (ordflag nil))
  (let* ((nchd (normal-form (mod12 chd)))
         (every-pcform (every-pctransp pcform))
         (mindist
          (loop for epc in every-pcform minimize (listdist nchd epc))))
    (matchreg
     (car (member-if 
           (lambda (x) (eql (listdist nchd x) mindist))
           every-pcform))
     chd ordflag)))

;; POISSONVEC -- creates n-trials list for probability p
;; tlevel = floor; maxval = (clipped) max
(defun poissonvec (p n &optional (tlevel 0) (maxval nil))
  (let ((nomax
	 (transp
	  (loop repeat n collect
		(cllib:gen-poisson-variate (coerce p 'double-float)))
	  tlevel)))
    (if maxval (clip-hi maxval nomax) nomax)))

;; POISSON->CODE -- creates 0-1 codeword from poisson
;; for use with 'code->slots' for pits/rests etc.
(defun poisson->code (fact len &optional (div 1) (treeflag nil))
  (let* ((pvec (clip-hi div (poissonvec fact len)))
	 (pcode (loop for p in pvec collect
		      (shuffle (append (copylist (list 1) p)
				       (copylist (list 0) (- div p)))))))
    (if treeflag pcode (flatten pcode))))

;; DENSITY->CODE
;; returns code of length n with density p
(defun density->code (p n)
  (let ((nbr1s (round (* n p))))
    (shuffle
     (append (copylist (list 1) nbr1s)
	     (copylist (list 0) (- n nbr1s))))))

;; RESCALE-ALL -- quick sugar
(defun rescale-all (alist oldmin oldmax newmin newmax)
  (mapcar (lambda (x) (rescale x oldmin oldmax newmin newmax))
	  alist))

; RESCALE-Q -- quick rescale
;; takes min & max from invec
(defun rescale-q (inlist newmin newmax)
  (rescale-all inlist 
	       (apply #'min inlist)
	       (apply #'max inlist)
	       newmin
	       newmax))

;; RANVEC -- a bunch of rans
;; May 2009: added min/max & type
(defun ranvec (len &optional (min 0) (max 1) (rtype :uniform))
  (loop repeat len 
    collect (ran :below max :from min :type rtype)))

;; RANVEC-BETA -- ranvec with beta distribution
;; added May 2009
(defun ranvec-beta (len a-fact b-fact &optional (min 0) (max 1)) 
  (mapcar (lambda (x) (rescale x 0 1 min max))
	  (loop repeat len 
	    collect (ran :type :beta :a a-fact :b b-fact))))

;; RANVEC-GAUSSIAN -- ranvec with gaussian (normal) distribution
;; added May 2009
(defun ranvec-gaussian (len &optional (min 0) (max 1))
  (let ((rawvec (loop repeat len collect (ran :type :gaussian))))
    (rescale-all
     (clip-lo -3 
	      (clip-hi 3 rawvec))
     -3 3 min max)))

;; TRUEVALS -- converts t & nil to tval & nilval
(defun truevals (vec nilval tval)
  (mapcar (lambda (x) (if x tval nilval))
	  vec))

;; ODDSVALS -- converts prob. vector to its binary choice
(defun oddsvals (vec nilval tval)
  (truevals (mapcar #'odds vec) nilval tval))

;; RAN-CHOICES -- choosing from list using ranvec
(defun ran-choices (len alist &optional (rantype :uniform))
  (chooser
   (mapcar #'floor (ranvec len 0 (length alist) rantype))
   alist))

;; BETA-CHOICES -- choosing from list using ranvec-beta
(defun beta-choices (len a-fact b-fact alist)
  (let ((alen (length alist)))
    (chooser
     (mapcar 
      (lambda (x) (if (>= x alen) (floor (- x 1)) (floor x)))
      (ranvec-beta len a-fact b-fact 0 alen))
     alist)))

;; GAUSSIAN-CHOICES -- choosing from list using gaussian dist.
(defun gaussian-choices (len alist)
  (let ((alen (length alist)))
    (chooser
     (mapcar 
      (lambda (x) (if (>= x alen) (floor (- x 1)) (floor x)))
      (ranvec-gaussian len 0 alen))
     alist)))

;; SHADE-INTO -- using interp & odds to go from startval to endval 
(defun shade-into (len startval endval)
  (oddsvals
   (loop for n from 1 to len collect
	 (interp n 1 0 len 1))
   startval endval))

;; WORK WITH 'BEATS'

;; DIV-DUR -- divides dur by div (returns list of eql durs)
(defun div-dur (dur div)
  (loop repeat div collect (/ dur div)))

;; GET-SIMPLEBEATS -- returns all simple-rhythm divisors
(defun get-simplebeats (anum)
  (second (nth anum simplebeats)))

;; ALL-SECTIONS-WITH-DUR -- returns all sec-lengths that
;; can be divided by 'dur'
(defun all-sections-with-dur (dur)
  (no-nils
   (loop for n to 200
	 collect (if (member dur (second (nth n beats))) n))))

;; BEAT-INTERSECTION -- simplebeats common to two durations
(defun beat-intersection (num1 num2)
  (intersection (get-simplebeats num1) (get-simplebeats num2)))

;; REL-PRIMES -- all members relatively prime to each other
;; in subset of 'alist' with size 'len'
(defun rel-primes (alist &optional (len 2))
  (let ((subs (subsets-len alist len)))
    (no-nils
     (loop for s in subs collect
	   (if (and (not (member 1 s)) 
		    (not 
		     (member-if-not (lambda (x) (eql x 1))
				    (mapcar #'(lambda (x) (reduce #'gcd x))
					    (subsets-len s 2)))))
	       s)))))


;;; 'warped' pits etc.

;; EXPWARP -- 'warps' pits by expt factor
;; (above optional bass-note, or lowest note in chd)
(defun expwarp (pits factor &optional (bassnote nil))
      (let* ((orig-hz (remove-duplicates (hertz pits)))
	     (bn (if bassnote bassnote (apply #'min orig-hz)))
	     (hzdiffs (mapcar (lambda (x) (- x bn)) orig-hz)))
	(loop for n to (- (length orig-hz) 1) collect
	      (keynum
	       (+ bn (* (nth n hzdiffs) factor))
	       :hz 't))))

;; FREQWARP -- 'warps' frequencies by expt factor
;; (above optional bass-note, or lowest note in chd)
(defun freqwarp (pits factor &optional (bassnote nil))
      (let* ((orig-hz (remove-duplicates pits))
	     (bn (if bassnote bassnote (apply #'min orig-hz)))
	     (hzdiffs (mapcar (lambda (x) (- x bn)) orig-hz)))
	(loop for n to (- (length orig-hz) 1) collect
	       (+ bn (* (nth n hzdiffs) factor)))))

;; HZWARP -- changing pits by +/- same hz 
;; note: negative frequencies appear as positive
(defun hzwarp (pits factor) 	 
  (let ((orig-hz (remove-duplicates (hertz pits))))
    (filter #'plusp
	    (loop for n to (- (length orig-hz) 1) collect
		  (keynum 
		   (abs (+ (nth n orig-hz) factor))
		   :hz 't :through *scale*)))))

;; FREQMULT -- takes & returns keynums
;; multiplies corresponding freq by a factor
(defun freqmult (inlist tfactor &optional (roundflag nil))
  (if (listp (car inlist))
      (mapcar (lambda (x) (freqmult x tfactor roundflag)) inlist)
      (if roundflag (remove-duplicates 
		     (mapcar #'round
			     (hz->keys (transp (hertz inlist) tfactor #'*))))
	  (hz->keys (transp (hertz inlist) tfactor #'*)))))

;; SCALE-SPECTRUM-TOP -- rescales spectrum when top keynum changes
(defun scale-spectrum-top (spectrum newtop &optional (roundflag nil))
  (let* ((spect (safesort spectrum))
	 (inhz (mapcar #'hertz spect))
	 (oldspan (- (car (last inhz)) (car inhz)))
	 (newspan (- (hertz newtop) (car inhz))))
    (mapcar (lambda (x) (if roundflag (round (keynum x :hz t)) (keynum x :hz t)))
	    (melint->line 
	     (car inhz)
	     (rescale-all (melint inhz) 0 oldspan 0 newspan)))))

;; SCALE-FSPECTRUM-TOP -- rescales spectrum when top keynum changes - freq version
(defun scale-fspectrum-top (spectrum newtop)
  (let* ((spect (safesort spectrum))
	 (inhz spect)
	 (oldspan (- (car (last inhz)) (car inhz)))
	 (newspan (- newtop (car inhz))))
    (melint->line 
     (car inhz)
     (rescale-all (melint inhz) 0 oldspan 0 newspan))))

;; SCALE-SPECTRUM-LOW -- rescales spectrum when bottom keynum changes
(defun scale-spectrum-low (spectrum newlo &optional (roundflag nil))
  (let* ((spect (safesort spectrum))
	 (inhz (mapcar #'hertz spect))
	 (oldspan (- (car (last inhz)) (car inhz)))
	 (newspan (- (car (last inhz)) (hertz newlo))))
    (mapcar (lambda (x) (if roundflag (round (keynum x :hz t)) (keynum x :hz t)))
	    (melint->line 
	     (hertz newlo)
	     (rescale-all (melint inhz) 0 oldspan 0 newspan)))))

;; SCALE-FSPECTRUM-LOW -- rescales spectrum when bottom freq changes -- freq. version
(defun scale-fspectrum-low (spectrum newlo)
  (let* ((spect (safesort spectrum))
	 (inhz spect)
	 (oldspan (- (car (last inhz)) (car inhz)))
	 (newspan (- (car (last inhz)) newlo)))
	    (melint->line 
	      newlo
	     (rescale-all (melint inhz) 0 oldspan 0 newspan))))

;; HZ->KEYS -- utility to convert hz (num or list) to keynums
(defun hz->keys (input &optional (roundflag nil))
  (cond ((listp input) 
	 (mapcar (lambda (x) (hz->keys x roundflag)) 
		 input))
	((numberp input) 
	 (if roundflag
	     (keynum input :hz 't :through *scale*)
	     (keynum input :hz 't)))
	(t 'r)))

;; KEY-HARMONICS -- returns specified harmonics of keynum
;; May 2009: simplified to include fractional harmonics
;; 'trusort' sorts keys by their proximity to true freq.
;; note: key-harmonics 22 + 23 are generally the same!
(defun key-harmonics (pit harms &optional (trusort nil))
  (if (numberp harms) 	     
      (round (keynum (* (hertz pit) harms) :hz t))
      (let* ((rawkhs
	      (mapcar
	       (lambda (x) 
		 (keynum (* (hertz pit) x) :hz t))
	       harms)))
	(round-all
	 (if trusort 
	     (sort rawkhs
		   (lambda (a b)
		     (< (abs (cadr (multiple-value-list (round a))))
			(abs (cadr (multiple-value-list (round b)))))))
	     rawkhs)))))

;; FREQ-HARMONICS -- returns list of harmonics, freqs in/out
(defun freq-harmonics (infreq harmlist)
  (mapcar (lambda (harm) 
	    (if (eql harm 1) infreq
		(nth (- harm 1) (harmonics 1 harm :hertz infreq))))
	  harmlist))

;; KEY-FM -- fm-spectrum to/from keynums
(defun key-fm (pit mratio index &optional (sidebands-idx nil))
  (remove-duplicates
   (mapcar #'round
	   (fm-spectrum (hertz pit) mratio index 
			:spectrum :keynum
			:all-sidebands sidebands-idx))))

;; KEY-RM -- rm-spectrum to/from keynums
(defun key-rm (keys1 keys2 &optional (minkey 1) (maxkey 127))
  (remove-duplicates
   (mapcar #'round
	   (rm-spectrum keys1 keys2 
			:minimum minkey
			:maximum maxkey))))

;; DUR-DENSITY -- avg # atx per beat in a list of durs
(defun dur-density (durs)
  (/ (length durs)
     (apply #'+ durs)))

;; BINS-BY-VAL -- returns set of 2-lists according to mapped function
;; useful to 'pickl' out according to value
;; (bins-by-val (indices 20) (lambda (x) (mod x 3))) =
;; ((2 (2 5 8 11 14 17)) (0 (0 3 6 9 12 15 18)) (1 (1 4 7 10 13 16 19)))
(defun bins-by-val (alist func)
  (let* ((vals (remove-duplicates
		(mapcar (lambda (a) (funcall func a)) 
			alist))))
    (loop for v in vals collect
	  (list v 
		(no-nils 
		 (loop for a in alist collect 
		       (if (eql (funcall func a) v) a)))))))

;; PICK-BY-VAL -- uses 'bins-by-val' to choose by value 
;; from a 'bins-by-val' list
;; (pick-by-val 1
;;  '((2 (2 5 8 11 14 17)) (0 (0 3 6 9 12 15 18)) (1 (1 4 7 10 13 16 19))))
;; = 13
(defun pick-by-val (val binlist)
  (pickl (second (assoc val binlist))))

;; GET-DENSITIES -- returns all densities from sets of durs
(defun get-densities (durlists)
  (safesort
   (mapcar #'first
	   (bins-by-val durlists'dur-density))))

;; ANY-EQL --  list/atoms comparison convenience
(defun any-eql (a b)
  (cond ((and (numberp a) (numberp b)) (eql a b))
	((and (listp a) (listp b)) (list-eql a b))))

;; PTCOLLECT-UTIL -- utility for 'relgraph'
(defun ptcollect-util (pt combos)
  (no-nils
   (mapcar (lambda (c) (if (member pt c) (position c combos))) combos)))

;; RELGRAPH -- produces sequences of 1,2-len divs for biglen
;; (next (relgraph 5 '(4 3)) 7) = ((3 4) 4 (4 5) 5 (3 5) 3 (3 20))
(defun relgraph (biglen 
		 &optional (startdiv (pickl (nth 0 (nth biglen simplebeats)))))
  (let* ((pts (reverse
	       (set-difference
		(nth 0 (nth biglen simplebeats))
		(list 1))))
	 (combos (rel-primes pts 2))
	 (pclist (append pts combos)))
    (new graph 
      :of
      (append
       (loop for p in pts collect
	     `(,p 
	       :id ,(position p pclist :test #'any-eql) 
	       :to ,(new weighting 
			 :of (transp (ptcollect-util p combos) (length pts)))))
       (loop for c in combos collect
	     `(,c :id ,(position c pclist :test #'any-eql)
	       :to ,(new weighting 
			 :of (mapcar 
			      (lambda (x) (position x pclist :test #'any-eql)) 
			      c)))))
      :starting-node-index (position startdiv pclist :test #'any-eql))))

;; RELGRAPH->DURS -- returns durs from relgraph
(defun relgraph->durs (biglen nbr-sections 
			      &optional (startdiv biglen))
  (mapcar (lambda (x) (resultant (loop for y in x collect (div-dur biglen y))))
	  (not-flat
	   (next (relgraph biglen startdiv) nbr-sections))))


;; SLONIM -- version where initial chord determines the rest
;; takes 'startchd' (first accompaniment chord) and melody;
;; returns list of chords (accompaniment to line)
;; chd member determined by contour
;; "nomerge" = merge with melody (omits melody by default)
;; "nomatch" = don't match register of 'firstacc' in chords [instead return mod12s]
(defun slonim (firstacc melody &optional (nomerge nil) (nomatch nil))
  (let* ((firstmel (car melody))
	 (startchd (cons firstmel firstacc))
	 (tnchd (tn-type startchd))
	 (mstart-idx (position (mod12 firstmel) (normal-form startchd)))
	 (chdmems 
	  (mapcar (lambda (x) (mod x (length startchd)))
		  (melint->line mstart-idx
				(subst 1 0 (take-ntn-contour melody)))))
	 (chds (alltransp startchd))
	 (outchds			; tn-types match ok
	  (mapcar 
	   (lambda (n)
	     (car (member-if 
		   (lambda (x)
		     (eql (mod12 (nth n melody))
			  (nth (nth n chdmems) x)))
		   chds)))
	   (indices (length melody))))
	 (chds-minus-mel
	  (loop for n to (- (length outchds) 1) collect
		(set-difference (nth n outchds) (list (mod12 (nth n melody))))))
	 (outchds2 
	  (if nomatch 
	      chds-minus-mel 
	      (matchreg-chds (cons firstacc (cdr chds-minus-mel))))))
    (if nomerge 
	(list melody outchds2)
	(merge-slots (list melody outchds2)))))

; END-WITH -- gives sublist of list (or pattern) that ends with a value
;; returns 'nil' if pattern returns 100x without "item" (crude solution)
(defun end-with (input item)
  (if (listp input)
      (let ((alist input))
	(if (member item alist)
	    (append 
	     (loop for a in alist
		   until (eql item a)
		   collect a)
	     (list item))))
      (let ((patlist
	     (loop for nx = (next input)
		   for n to 99
		   collect nx
		   until (eql item nx))))
	(if (< (length patlist) 100)
	    patlist))))

;; 'morphing' arpeggiation...

;; NORPT-WEIGHTS
;; quick func that adds 'max 1' for use in weighting
(defun norpt-weights (alist)
  (loop for x in alist collect `(,x :max 1)))

;; NORPT-RANDPATT -- makes nonrepeating wgt pattern of alist
;; 'periodlen' can generate random range on the fly 
;; w/ e.g. "(pickl (indices 4 4))"
(defun norpt-randpatt (alist &optional (periodlen 1))
  (new weighting :of (norpt-weights alist)
       :for (pval periodlen)))

;; MORPH-ARPG-PAIR -- morphing by shifting weights from chd1 to chd2
;; [outputs midi seq]
;; len = total len; durs = durs assigned to pits 
;; 'perlen' lets you complete figures (periods) when picked
(defun morph-arpg-pair (chd1 chd2 len durs &optional (perlen1 1) (perlen2 1))
  (let* ((fig1 (norpt-randpatt chd1 perlen1))
	 (fig2 (norpt-randpatt chd2 perlen2))
	 (dpatt (if (pattern? durs) durs (makecyc durs)))
	 (durvec (patt-to-sum dpatt len)))
    (process 
      while durvec
      for w = (interp (now) 0 1 len 0)
      for thisfig = (odds w (next fig1 t) (next fig2 t))
      for thisdurs = (no-nils 
		      (loop repeat (length thisfig) collect (pop durvec)))
      sprout (splay thisfig thisdurs)
      wait (apply #'+ thisdurs))))

;e.g., using cycle for perlen2 
;(events
; (let ((mcyc (new cycle of '(1 2 3))))
;   (morph-arpg-pair '(60 61 62) (indices 5 70) 15 (new weighting :of '(.125 .25))
;		    (pickl (indices 2 1)) (next mcyc)))
; "out.midi" :play 'nil)

;; MORPH-ARPEGG -- sequential 'morph-arpg-pair' along chdlist
;; returns midi
;; same 'perlen' for all entries (otherwise use chain of 'morph-arpg-pair')
(defun morph-arpegg (chds inlens durs 
			  &optional (perlen 1))
  (let* ((dpatt (if (pattern? durs) durs (makecyc durs)))
	 (lens (if (listp inlens) inlens (copylist (list inlens) 
						   (- (length chds) 1)))))
    (process 
      for n to (- (length chds) 2)
      sprout (morph-arpg-pair (nth n chds)
			      (nth (+ n 1) chds)
			      (nth n lens)
			      dpatt
			      perlen
			      perlen)
      wait (nth n lens))))

;; SMORPH-PAIR -- morphs from chd1 to chd2 in 'len' slots -- returns slots
(defun smorph-pair (chd1 chd2 len &optional (perlen1 1) (perlen2 1))
  (let* ((fig1 (norpt-randpatt chd1 perlen1))
	 (fig2 (norpt-randpatt chd2 perlen2))
	 (slotcount 0))
    (subseq 
     (loop while (< slotcount len)
       for w = (interp slotcount 0 1 len 0)
       for thisfig = (odds w (next fig1 t) (next fig2 t))
       sum (length thisfig) into slotcount
       append thisfig)
     0 len)))

;; SMORPH -- slotted morph across list
;; Sept. 2008: takes single val for 'rawlens' 
(defun smorph (chds rawlens &optional (perlen 1))
  (let ((inlens (if (numberp rawlens) 
		    (copylist (list rawlens) (length chds))
		    rawlens)))
    (if (eql 2 (length chds))
	(smorph-pair (first chds) (second chds) inlens perlen perlen)
	(let ((lens (if (listp inlens) 
			inlens 
			(copylist (list inlens) (- (length chds) 1)))))
	  (loop for n to (- (length chds) 2)
		append (smorph-pair (nth n chds)
				    (nth (+ n 1) chds)
				    (nth n lens)
				    perlen
				    perlen))))))

;; RECURZ -- generalized recursion; returns chain 
;; 'input' always a list
(defun recurz (func input len)
  (cond ((eql len 1) input)
	((eql len 2) (list input (funcall func input)))
	(t 
	 (let ((prev (recurz func input (- len 1))))
	   (append prev (list (funcall func (car (last prev)))))))))

;; RANDCHAIN -- like recurz, but picks from generated list
(defun randchain (func input len)
  (cond ((eql len 1) input)
	((eql len 2) (list input (pickl (funcall func input))))
	(t 
	 (let ((prev (randchain func input (- len 1))))
	   (append prev (list (pickl (funcall func (car (last prev))))))))))

;; segmentation, subsequences etc. (June 2008)

(defun contains-seq (aseq alist)
  (member aseq
	  (subsequences alist (length aseq))
	  :test #'seq-eql))

(defun contains-set (aset alist)
  (member aset
	  (subsequences alist (length aset))
	  :test #'list-eql))

(defun contains-test (funcname aset alist)
  (member aset
	  (subsequences alist (length aset))
	  :test funcname))

;; FREEZE -- make subseqs of all stray atoms 
(defun freeze (alist)
  (gather-pits 
   (lambda (a b) (and (numberp a) (numberp b)))
   alist))

;; SUBSEQ-FREEZE -- builds segments from matching subseq
(defun subseq-freeze (aseq alist)
  (if (contains-seq aseq alist)
      (let* ((seqstart (position (car aseq) alist))
	     (seqend (+ seqstart (length aseq)))
	     (segids
	      (remove-duplicates 
	       (safesort (list 0 (length alist) seqstart seqend)))))
	(mapcar (lambda (x) (subseq alist (first x) (second x)))
		(map 'list #'list segids (cdr segids))))))

;; SUBSET-FREEZE -- builds segments from matching set
(defun subset-freeze (aset alist)
  (if (contains-set aset alist)
      (let* ((seqstart (apply #'min 
			      (mapcar (lambda (x) (position x alist))
				      aset)))
	     (seqend (+ seqstart (length aset)))
	     (segids
	      (remove-duplicates 
	       (safesort (list 0 (length alist) seqstart seqend)))))
	(mapcar (lambda (x) (subseq alist (first x) (second x)))
		(map 'list #'list segids (cdr segids))))))

;; COMMONSEG -- finding common row-segment btwn 2 rows
(defun commonseg (rowa rowb)
  (car
   (sort 
    (no-nils
     (loop for n from 2 to 6 collect
	   (car
	    (intersection 
	     (subsequences rowa n)
	     (subsequences rowb n)
	     :test #'list-eql))))
    (lambda (a b) (> (length a) (length b))))))

;; COMMONSEG-FREEZE
;; freeze pair of rows by common seg
(defun commonseg-freeze (rowa rowb)
  (let ((cseg (commonseg rowa rowb)))
    (if cseg
	(list (subset-freeze cseg rowa)
	      (subset-freeze cseg rowb))
	(list rowa rowb))))

;; SEG-DISTANCE -- distance btwn rows as a function of common seg length
(defun seg-distance (rowa rowb)
  (- 7 (length (commonseg rowa rowb))))

;; SEGLEN-CONCORDE-EDGEWEIGHTS -- utility for 'rows-by-seglen'
;; computes seglen distance matrix for a set of rows
(defun seglen-concorde-edgeweights (rowlist)
  (let* ((rawlist
	  (loop for c in rowlist collect
		(loop for d in rowlist collect (seg-distance c d))))
	 (newmax
	  (* 2 (apply #'max (flatten rawlist)))))
    (cons 
     (cons 0 (copylist (list newmax) (length rowlist)))
     (mapcar (lambda (x) (cons newmax x)) rawlist))))

;; ROWS-BY-SEGLEN - rows arranged by longest common segments (TSP)
(defun rows-by-seglen (rowlist)
  (let* ((infilename
	  (concatenate 'string "/tmp/"
		       (string (symbol-name (gensym "inconc")))))
	 (outfilename
	  (concatenate 'string "/tmp/"
		       (string (symbol-name (gensym "outconc")))))
	 (writebm
	  (write-output-to-file 
	   (seglen-concorde-edgeweights rowlist) infilename))
	 (runbm
	  (run-concorde infilename outfilename))
	 (output-idxs
	  (flatten
	   (make-list-from-file outfilename))))
    (progn
      (uiop:run-program "rm" (list infilename))
      (uiop:run-program "rm" (list outfilename))
      (loop for n in (cdr output-idxs) collect (nth (- n 1) rowlist)))))

;; SWAP-CONCORDE-EDGEWEIGHTS -- utility for 'rows-by-swap'
;; computes swap distance matrix for a set of rows
;; [swap distance is found in "transforms.lisp"]
(defun swap-concorde-edgeweights (rowlist)
  (let* ((rawlist
	  (loop for c in rowlist collect
		(loop for d in rowlist collect (placedist c d))))
	 (newmax
	  (* 2 (apply #'max (flatten rawlist)))))
    (cons 
     (cons 0 (copylist (list newmax) (length rowlist)))
     (mapcar (lambda (x) (cons newmax x)) rawlist))))

;; ROWS-BY-SWAP - rows arranged by closest swap distance (TSP)
(defun rows-by-swap (rowlist)
  (let* ((infilename
	  (concatenate 'string "/tmp/"
		       (string (symbol-name (gensym "inconc")))))
	 (outfilename
	  (concatenate 'string "/tmp/"
		       (string (symbol-name (gensym "outconc")))))
	 (writebm
	  (write-output-to-file 
	   (swap-concorde-edgeweights rowlist) infilename))
	 (runbm
	  (run-concorde infilename outfilename))
	 (output-idxs
	  (flatten
	   (make-list-from-file outfilename))))
    (progn
      (uiop:run-program "rm" (list infilename))
      (uiop:run-program "rm" (list outfilename))
      (loop for n in (cdr output-idxs) collect (nth (- n 1) rowlist)))))

;; "bestmatch" = most compact note-by-note match of two melodies (by consn-p)

;; NON-MATCHES -- nomatch idx pairs 
;; passed to 'bestmatch.pl' or 'nondet' for "bestmatch"
;; put output into a text file & call by ">bestmatch.pl len1 len2 filename" 
(defun non-matches (list1 list2)
  (transpose-matx
   (no-nils 
    (loop for n to (- (length list1) 1) append
	  (loop for m to (- (length list2) 1) collect
		(if (not (consn-p (nth n list1) (nth m list2))) 
		    (list n m))))))) 

;; MATCH-SLOTS -- matches two lines using slot vector
(defun match-slots (list1 list2 mslots) 
  (let ((tplen (+ 1 (apply #'max (flatten mslots)))))
    (merge-slots
     (map 'list 
	  (lambda (a b) 
	    (tpoints a b tplen))
	  (list list1 list2)
	  mslots))))

;; RUN-BMATCH -- utility for 'bestmatch'
;; runs "bestmatch.pl" & writes to output file
(defun run-bmatch (len1 len2 input-file output-file)
    (uiop:run-program "bestmatch.pl" 
		     (list (format nil "~a" len1)
			   (format nil "~a" len2)
			   input-file)
		   :if-output-exists :supersede
		   :output output-file))

;; BESTMATCH -- most-compact consonant match of two lines by consonance
(defun bestmatch (line1 line2)
  (let* ((infilename
	  (concatenate 'string "/tmp/"
		       (string (symbol-name (gensym "inlispext")))))
	 (outfilename
	  (concatenate 'string "/tmp/"
		       (string (symbol-name (gensym "outlispext")))))
	 (writebm
	  (write-output-to-file (non-matches line1 line2) infilename))
	 (runbm
	  (run-bmatch (length line1) (length line2) infilename outfilename))
	 (matchidxs
	  (make-list-from-file outfilename)))
    (progn
      (uiop:run-program "rm" (list infilename))
      (uiop:run-program "rm" (list outfilename))
      (match-slots line1 line2 matchidxs))))

;; PERLES -- corresponding sum-row
(defun perles (row &optional (sum 12))
  (mod12 
   (mapcar 
    (lambda (x) (- sum x)) 
    (mod12 row))))

;; POLY->SLOTS
;; makes lines out of poly vector
;; lists lines from most to least active
(defun poly->slots (alist)
  (let* ((nflat (not-flat alist))
	 (maxlen
	  (apply #'max (mapcar #'length nflat))))
    (transpose-matx
     (mapcar (lambda (x) (add-rests x maxlen)) nflat))))

;; BUILD-UP -- builds up (slotted) list starting from top
(defun build-up (alist)
  (let ((listlen (length alist)))
    (loop for n from 1 to listlen 
	  collect
	  (add-rests
	   (subseq alist 0 n)
	   listlen))))

;; BUILD-DOWN -- returns successive cdr's (slots), ending at "last"
(defun build-down (alist)
  (mapcar #'reverse
	  (reverse
	   (build-up (reverse alist)))))

;; BUILD-SNAKE -- builds slotted list, then back
(defun build-snake (alist)
  (let ((listlen (length alist)))
    (mapcar (lambda (x) (add-rests (subseq alist 0 x) listlen))
	    (transp (snake listlen (- (* 2 listlen) 1)) 1))))

;; BUILD-THRU -- builds slotted list, then cdr's to end
(defun build-thru (alist)
  (append 
   (build-up alist)
   (cdr 
    (build-down alist))))

;;; OVERLAP-PAIR - overlapping start/end of two lists
(defun overlap-pair (list1 list2 &optional (olen 1))
  (cond
    ((plusp olen)
     (append (butlast list1 olen)
	     (mapcar #'reverse
		     (merge-slots (list (last list1 olen)
					(subseq list2 0 olen))))
	     (nthcdr olen list2)))
    ((eql olen 0) (append list1 list2))
    (t (append list1 (copylist (list 'r) (abs olen)) list2))))

;; OVERLAP -- overlapping list-of-lists by number, list, or pattern 
(defun overlap (lists &optional (olen 1))
  (let* ((olenpatt (if (pattern? olen) olen (makecyc olen)))
	 (olenlist (next olenpatt (- (length lists) 1))))
    (case (length lists) 
      (1 lists)
      (2 (overlap-pair 
	  (first (last lists 2)) (car (last lists)) (pop olenlist)))
      (t
       (let ((smb (overlap (butlast lists) olenlist)))
	 (overlap-pair smb
		       (car (last lists))
		       (car (last olenlist))))))))

;; HEX-COMBI-P -- test for inversional hexachordal combinatoriality
;; note: all untransposed, unsorted hexachords with this property
;; are found in "data/besthex.lisp"
(defun hex-combi-p (myrow)
  (let* ((firsthalf (first (make-poly myrow 6)))
	 (allinvs (alltransp (mod12 (mapcar (lambda (x) (- 12 x)) myrow)))))
    (> (length 
	(remove-duplicates
	 (mapcar (lambda (x) (list-eql firsthalf (second x)))
		 (mapcar (lambda (x) (make-poly x 6)) allinvs))))
       1)))

;; HEX-COMBI-PARTNER -- returns inversion with hexachordal combinatoriality 
;; [when possible]
(defun hex-combi-partner (myrow)
  (let* ((firsthalf (first (make-poly myrow 6)))
	 (allinvs (alltransp (mod12 (mapcar (lambda (x) (- 12 x)) myrow)))))
    (flatter
     (no-nils 
      (mapcar (lambda (x) (if (list-eql firsthalf (second x)) 
			      (flatten x)))
	      (mapcar (lambda (x) (make-poly x 6)) allinvs))))))

;; OTHERHEX -- produces second (unsorted) hexachord to make row
(defun otherhex (hexachord)
  (set-difference (indices 12) hexachord))

;; RANDHEXROW -- randomly generated inversional/hexichord combinatorial row
;; needs "data/besthex.lisp" loaded
(defun randhexrow ()
  (let* ((phex (pickl besthex)))
    (append (shuffle phex) (shuffle (otherhex phex)))))

;; NEXTRICHAIN -- utility for 'richain'
;; computes overlapping ri-form 
(defun nextrichain (rform)
  (if (listp rform)
      (mod12
       (transp-to (car (last (butlast rform))) 
		  (reverse (invert-dk rform 12))))))

;; RICHAIN -- retrograde-inversion chain w/2-member overlap
;; flattened version truncates last 2 for use in cycles
(defun richain (inrow &optional (treeflag nil))
  (let ((treeform
	 (cons inrow
	       (loop for iter = (nextrichain inrow) then (nextrichain iter)
		     and prev = inrow then iter
		     until (seq-eql iter inrow)
		     collecting iter))))
    (if treeflag
	treeform
	(butlast (flatten (overlap treeform 2)) 2))))

;; NONO-DISTRIBUTE -- distribute pitches in chronological order among
;; multiple rhythmic streams [may be long list, val, or pattern]
;; returns pit/dur pairs for each stream
(defun nono-distribute (pits indurs)
  (let* ((alldurs (mapcar (lambda (x) (second (plists pits x))) indurs))
	 (allatx 
	  (remove-redundancy (mapcar (lambda (x) (melint->line 0 x)) alldurs)))
	 (atklist (remove-duplicates (safesort (flatten allatx))))
	 (allpos
	  (loop for atx in allatx collect
		(mapcar (lambda (x) (position x atklist))
			atx)))
	 (pitvecs
	  (loop for p in allpos collect
		(no-nils (mapcar (lambda (x) (nth x pits)) p))))
	 (atkvecs
	  (loop for ax in allpos collect 
		(mapcar
		 (lambda (x) (nth x atklist))
		 ax))))
    (mapcar #'atx->durs
	    (transpose-matx (list pitvecs atkvecs)))))

;; CHORD-NEIGHBORS -- all notes a step away from chord tones
(defun chord-neighbors (chd)
  (flatten
   (if (numberp chd) 
       (transp '(-2 -1 1 2) chd)
       (mapcar #'chord-neighbors chd))))

;; MJTRIADS -- makes triads on half/whole step from note
(defun mjtriads (apit)
  (mod12
   (loop for n in 
	 (transp '(-2 -1 1 2) (mod12 apit))
	 append (list (transp '(0 3 7) n) (transp '(0 4 7) n)))))

;; MJTRIAD -- makes triad on half/whole step from note
(defun mjtriad (apit)
  (mod12
   (transp (pickl '((0 3 7) (0 4 7))) 
	   (+ (mod12 apit) (pickl '(-2 -1 1 2))))))

;; MJTRIAD2 -- any triad not containing note
(defun mjtriad2 (apit)
  (let ((alltriads
	 (append (alltransp '(0 3 7))
		 (alltransp '(0 4 7)))))
    (pickl
     (no-nils
      (mapcar 
       (lambda (x) (if (not (member (mod12 apit) x)) x))
       alltriads)))))

;; MJSEVENTHS -- any seventh chord not containing note
(defun mjsevenths (apit)
  (let ((allchds
	 (mod12
	  (append (alltransp '(0 3 7 10))
		  (alltransp '(0 4 7 10))
		  (alltransp '(0 4 7 11))
		  (alltransp '(0 3 6 10))))))
    (pickl
     (no-nils
      (mapcar 
       (lambda (x) (if (not (member (mod12 apit) x)) x))
       allchds)))))

;; SUBSEQ-PATH - gives subseq from 'startpt' to 'endpt'
;; [cycles thru if end comes earlier than start]
;; 'endflag' = incl. end in subseq
(defun subseq-path (startpt endpt aseq &optional (endflag nil)) 
  (let* ((aseq2 (append aseq aseq))
	 (startpos (position startpt aseq2)))
    (append
     (subseq aseq2
	     startpos
	     (position endpt aseq2 :start (+ 1 startpos)))
     (if endflag (list endpt)))))

;; SUBSEQ-BRANCH -- 'subseq-path' btwn elements in 'inseq'
(defun subseq-branch (inseq pathseq &optional (endflag nil))
  (map 'list
       (lambda (a b) (subseq-path a b pathseq endflag))
       inseq
       (cdr inseq)))

; CLOUDY -- 'randomizes' poly to single atx w/in each dur
; returns flat durlist
(defun cloudy (pits durs &optional (rantype :uniform) (randmin .01))
  (let* ((pl
	  (plists pits durs))
	 (realdurs (second pl))
	 (chdlens (take-poly pits)))
    (transp
     (flatten
      (map 'list
	   (lambda (rdur clen) 
	     (ransegs clen :type rantype :sum (- rdur (* randmin clen))))
	     realdurs
	     chdlens))
     randmin)))

; HETEROPHONY -- returns varied durlist [by shifting atx]
(defun heterophony (pits durs)
  (let* ((atx
	  (melint->line 0
			(car (cdr (plists pits (mapcar #'float durs)))))))
    (melint (cons 0 (cdr (safesort (map 'list #'between atx (cdr atx))))))))

;; WEDGE-IDX -- seq from middle to boundaries of a list
;; (wedge-idx 6) = (3 2 4 1 5 0)
(defun wedge-idx (len &optional (offset 0))
  (let* ((midpoint (floor (/ len 2)))
	 (ilen (indices len)))
    (transp
     (subseq
      (interlock (subseq ilen midpoint len)
		 (reverse (subseq ilen 0 midpoint))
		 1 1)
      0 len)
     offset)))

;; WEDGE
(defun wedge (alist)
  (nths (wedge-idx (length alist)) alist)) 

;; VOICED-WELL -- if wider diffs at bottom
(defun voiced-well (alist)
  (let* ((inlist (safesort alist))
	 (diffs
	  (map 'list
	       (lambda (a b) (- b a))
	       inlist (cdr inlist))))
    (if (apply #'> diffs) alist)))

;; VOICED-WELL-KEYS -- 'voiced-well' to/from keynums
(defun voiced-well-keys (alist)
  (let* ((inlist (safesort (mapcar #'hertz alist)))
	 (diffs
	  (map 'list
	       (lambda (a b) (- b a))
	       inlist (cdr inlist))))
    (if (apply #'> diffs) alist)))

;; DURWARP -- warping subdivs
;; fractional atkpts raised to a power (btwn. 0 & 1)
;; accel (descending durs) by default
(defun durwarp (divs totalen warpfactor &optional (ritflag nil))
  (let ((rawdurs 
	 (transp
	  (melint
	   (mapcar 
	    (lambda (x) (expt x (- 1 warpfactor)))
	    (melint->line 0 (divver divs 1))))
	  totalen #'*)))
    (if ritflag (reverse rawdurs) rawdurs)))

;; DURWARP-CLUMP -- accel/rit over totalen (or totalen+1 if odd)
;; [returns even # atx only]
(defun durwarp-clump (atx totalen warpfactor)
  (let ((divs (round (/ atx 2))))
    (append
     (durwarp divs (/ totalen 2) warpfactor)
     (durwarp divs (/ totalen 2) warpfactor 'rvs))))

;; FERNWARP -- warping all ferneys
(defun fernwarp (mlens subdivs warpfactor &optional (ritflag nil))
  (let* ((mlens-cyc (new cycle of mlens))
         (subdivs-cyc (new cycle of subdivs)))
    (flatten (loop until (and (eop? mlens-cyc)
			      (eop? subdivs-cyc))
	       collect (durwarp (next subdivs-cyc)
				(next mlens-cyc) 
				warpfactor
				ritflag)))))

;; RESCALE-DURS -- rescale a durlist to match new sum
(defun rescale-durs (durs newlen)
  (mapcar 
   (lambda (x) (* x (/ newlen (sum durs))))
   durs))

;; RESCALE-KEYHARMS -- rescale a keynum's harmonic series
(defun rescale-keyharms 
    (harm1 harm2 basekey rescalekey 
	   &optional (roundflag nil))
  (let ((rawkeys
	 (hz->keys (harmonics harm1 harm2 
			      :hertz (hertz basekey) 
			      :rescale (hertz rescalekey)))))
    (if roundflag (mapcar #'round rawkeys) rawkeys)))

;; AXIS-INVERSION
;; inverts pitches around axis
(defun axis-inversion (input axis)
  (if (numberp input)
      (+ axis 
	 (- axis input))
      (mapcar (lambda (x) (axis-inversion x axis))
	      input)))

;; HARMONICS-DOWN -- wraps harmonics DOWN from given pitch
(defun harmonics-down (harm1 harm2 hipitch)
  (reverse (harmonics harm1 harm2 :keynum hipitch :invert t :undertones t)))

;; CRESC -- increasing amps over list
(defun cresc (alist &optional (minamp .1) (maxamp .8))
  (let* ((listlen (length alist)))
    (mapcar 
     (lambda (x) (interp x 0 minamp (- listlen 1) maxamp))
     (indices listlen))))

;; DIM -- decreasing amps over list
(defun dim (alist &optional (minamp .1) (maxamp .8))
  (reverse (cresc alist minamp maxamp)))

;; CURVED-DURS 
;; accel/convex (0-1); rit/concave (>1) 
(defun curved-durs (alist factor)
  (let* ((listlen (length alist))
	 (totalen (sum alist))
	 (atx (melint->line 0 alist)))
    (rescale-all 
     (melint
      (mapcar (lambda (x) (expt x factor))
	      (rescale-all atx 0 totalen 0 1)))
     0 1 0 totalen)))

;; REPLACE-PATH -- replacement transform from list1 to list2 
;; result includes list1 but not list2 (unless 'list2flag' present)
;; both lists must have same length
(defun replace-path (list1 list2 &optional (list2flag nil))
  (let* ((listlen (length list1))
	 (swaps (shuffle (indices listlen)))
	 (swaplists
	  (loop for n from 1 to (- listlen 1) collect
		(subseq swaps 0 n)))
	 (rawlist
	  (remove-duplicates 
	   (cons list1
		 (loop for swaplist in swaplists collect
		       (loop for x in (indices listlen) collect
			     (if (member x swaplist) (nth x list2) (nth x list1)))))
	   :test #'list-eql)))
    (if list2flag (append rawlist (list list2)) rawlist)))

;; figures = same-length (accent code) & (integral duration) vectors
;; accents can be used for velocity or texture

;; ALL-FIGS 
;; all figures (accent code + integer durations) of total len & nbratx
(defun all-figs (len nbratx)
  (let* ((atkcodes
	  (mapcar 
	   (lambda (y) (cons 1 y))
	   (filter 
	    (lambda (x) (eql (codeweight x) (- nbratx 1)))
	    (all-codewords (- len 1)))))
	 (dursets
	  (mapcar #'code->durs atkcodes))
	 (accsets
	  (mapcar #'durs->code
		  (remove-duplicates
		   (flatter
		    (mapcar
		     #'permutations
		     (eql-summer nbratx '(2 3))))
		   :test #'seq-eql))))
    (match2lists accsets dursets)))

; RAND-FIG -- same-length (accent) & (integral duration) vectors
(defun rand-fig (len nbratx)
  (pickl (all-figs len nbratx)))

; alternate name
(defun randfigure (len nbratx)
  (rand-fig (len nbratx)))

;; END-TO-END -- last of each sublist starts the next one
;; used like 'make-poly'
(defun end-to-end (alist polylens)
  (let* ((subidx
	  (not-flat (make-poly (indices (length alist)) polylens))))
    (mapcar
     (lambda (x) (nths x alist))
     (map 'list #'transp
	  subidx
	  (reverse (indices (length subidx) (* -1 (- (length subidx) 1))))))))

;; LISTS->E2E
;; converts lists to end-to-end lists, by copying last to next list
(defun lists->e2e (input)
  (let ((inlists (not-flat input)))
    (cons (car inlists)
	  (map 'list (lambda (a b) (cons (car (last a)) b))
	       inlists
	       (cdr inlists)))))

;; POLYSTRUMS
;; gives 'strummed' (long ending) durations for phrases
(defun polystrums (inlists &optional (endmin 3) (endmax 7) (treeflag nil))
  (let ((rawout
	 (mapcar (lambda (x) 
		   (append
		    (copylist (list 1) (- x 1))
		    (list (between endmin (+ 1 endmax)))))
		 (take-poly inlists))))
    (if treeflag rawout (flatten rawout))))

;; HELD-LINES
;; takes sublists (ordered chords); returns pit/dur pairs 
(defun held-lines (chds)
  (let* ((sorted-chds (mapcar #'safesort chds))
	 (sorted-lines (transpose-matx sorted-chds))
	 (sorted-pos
	  (map 'list
	       (lambda (c sc)
		 (flatten (mapcar (lambda (x) (positions x sc)) c)))
	       chds
	       sorted-chds))
	 (slotted-lines
	  (loop for n to (- (length (car chds)) 1) collect
		(let ((chdcyc (makecyc (nth n sorted-lines))))
		  (mapcar (lambda (x) (if (eql n x) (next chdcyc) 'r)) (flatten sorted-pos))))))
    (mapcar #'slots->durs 
	    slotted-lines)))

;; RANGE -- returns min & max
(defun range (alist)
  (if (listp (car alist))
      (mapcar #'range alist)
      (list (apply #'min alist)
	    (apply #'max alist))))

;; KNOCKDOWN -- utility for 'not-too-poly'
;; breaks down numbers > 4
(defun knockdown (x)
  (if (< x 5) x (shuffle (list 2 (knockdown (- x 2))))))

;; NOT-TOO-POLY -- makes smaller chords (max 4)
(defun not-too-poly (alist)
  (make-poly (flatten alist)
	     (flatten (mapcar #'knockdown (take-poly alist)))))

;; PLACE-IDS -- gives unique pit idx vector corresponding to melody
;; e.g. (place-ids '(11 10 11 11 12 10 12)) => (0 1 0 0 2 1 2)
;; added Jan. 2012
(defun place-ids (mel) 
  (let* ((allpits (remove-duplicates mel))
	 (sallpits (sort-to-other allpits mel)))
    (listsub (indices (length allpits)) sallpits mel)))

; EXTRACT-INTERVALS -- takes list & pc-interval; returns all pairs related by that pc-interval
(defun extract-intervals (inlist testint &optional (modlen 12))
  (filter (lambda (x) (eql testint (abs-intv (first x) (second x) modlen)))
	  (subsets-len inlist 2)))

;; AUTOTRANSP -- all chords of same structure w/ at least one c.t.
(defun autotransp (inchd)
  (let* ((inchd-cntr (take-contour inchd))
	 (srtchd (safesort inchd))
	 (span (- (car (last srtchd)) (first srtchd))))
    (mapcar (lambda (y) (give-contour-to-set inchd-cntr y))
	    (filter (lambda (x) (intersection x srtchd))
		    (loop for tlevel from (- (car srtchd) span) to (car (last srtchd)) collect
			  (transp (transp-to 0 srtchd) tlevel))))))

;; POLY->SUBLISTS
;; gathers sublists of poly structure (2 1 1 1) etc.
(defun poly->sublists (polylist)
  (let* ((rawpos
	  (filterpos (lambda (x) (> x 1)) (take-poly polylist)))
	 (goodpos
	  (if (eql 0 (car rawpos)) rawpos (cons 0 rawpos))))
    (make-poly polylist (melint goodpos))))


;; need this apparently
; (in-package :cm)




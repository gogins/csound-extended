

;; REWRITE.LISP -- using CM's rewrite function
;; some general functions

(defun codeword (codelen &optional (wgts 1))
 (let ((weights
         (if (numberp wgts) (list wgts) wgts)))
   (loop for x from 0 to (- codelen 1)
         collect (if (member x weights) 1 0))))

;; WEIGHTED-CODEWORD -- random codeword of specified length & weight
(defun weighted-codeword (codelen weight)
  (shuffle (append (copylist (list 1) weight)
		   (copylist (list 0) (- codelen weight)))))

(defun random-codeword (codelen)
  (shuffle (cons 1 (loop repeat (- codelen 1) collect (pick 1 0)))))

(defun full-codeword (codelen)
  (loop repeat codelen collect 1))

(defun empty-codeword (codelen)
  (loop repeat codelen collect 0))

(defun initial-codeword (codelen)
  (cons 1 (empty-codeword (- codelen 1))))

(defun complement-codeword (cw)
  (mapcar (lambda (x) (expt (- x 1) 2)) cw))

(defun codeweight (cw)
  (count 1 cw))

;; ONESCODE -- replaces all nonzero entries with 1
(defun onescode (cword)
  (mapcar (lambda (x) (if (zerop x) 0 1))
	  cword))

;; ALL-CODEWORDS of length 'len', sorted by ascending weight
(defun all-codewords (len)
  (sort 
   (all-combos
    (loop repeat len collect (list 0 1)))
   (lambda (a b) (< (sum a) (sum b)))))

;; MAKELEN-CODE -- changes code length, by subseq or appending "0"s
(defun makelen-code (len cw)
  (let ((cwlen (length cw)))
    (cond ((eql cwlen len) cw)
	  ((> cwlen len) (subseq cw 0 len))
	  (t (append cw (copylist (list 0) (- len cwlen)))))))

;; BITWSWITCH: 0->1 & 1->0
(defun bitswitch (x)
  (abs (- (* x x) 1)))

;; CW-ROTS -- all rotations (no dupes) of a codeword
(defun cw-rots (cw)
  (remove-duplicates (allrots cw) :test #'seq-eql))

;; MERGE CODEWORDS -- texture sum
;; returns cw of max length
(defun merge-codewords (cw1 cw2)
  (let ((maxlen (apply #'max (list (length cw1) (length cw2)))))
    (clip-hi 1 
	     (map 'list #'+ 
		  (makelen-code maxlen cw1)
		  (makelen-code maxlen cw2)))))

;; CODEMAP -- re-writing 1s and 0s in a codeword
;; useful for fig accent vectors -> texture
(defun codemap (cword &optional 
			       (map1 (new weighting :of '(2 3)))
			       (map0 1))
  (let* ((codelen (length cword))
	 (len1 (codeweight cword))
	 (len2 (- codelen len1))
	 (new1s
	  (second
	   (plists (indices len1) map1)))
	 (new0s
	  (second
	   (plists (indices len2) map0))))
	  (flatter
	   (merge-slots
	    (list
	     (tpoints new1s (positions 1 cword) codelen)
	     (tpoints new0s (positions 0 cword) codelen))))))

;; DURS->CODE -- converting duration list to codeword
;; fixed Jan. 2011
(defun durs->code (durs)
  (let ((posns 
	 (melint->line 0 durs)))
    (mapcar (lambda (x) (if (member x posns) 1 0))
	    (indices (sum durs)))))

;; CODE->DURS -- converting codeword to list of integer gaps (skipping zeros)
(defun code->durs (cword)
  (second
   (slots->durs
    (mapcar (lambda (x) (if (eql x 0) 'r x)) cword))))

;; CODE->PAIRS -- returns array of coordinate pairs of slots with 1
(defun code->pairs (ary)
  (let ((xlen (- (first (array-dimensions ary)) 1))
        (ylen (- (second (array-dimensions ary)) 1)))
    (no-nils
    (loop for x to xlen append
          (loop for y to ylen collect
                (if (= (aref ary x y) 1) (list x y)))))))

;; PLACES->CODEWORD -- returns codeword with '1's in each place
(defun places->codeword (plist len)
  (loop for n to (- len 1) collect
	(if (member n plist) 1 0)))

;; PAIRS->PIT7 -- converts list of pairs to pits mod7
(defun pairs->pit7 (alist)
  (loop for x in alist collect
        (+ (mod (* (first x) 7) 12)
           (* 12 (mod (second x) 7)))))

;; PAIRS->PIT5 -- converts list of pairs to pits mod5
(defun pairs->pit5 (alist)
  (loop for x in alist collect
        (+ (mod (* (first x) 5) 12)
           (* 12 (mod (second x) 7)))))

;; MTRULES -- morse-thue rules
(defparameter mtrules '((0 -> (0 1))
                  (1 -> (1 0))))

;; RW-NEXT -- returns next complete generation of rewrite
;; rwthing = rules; alist = input string
;; example: (rw-next mtrules '(1 0)) = (1 0 0 1)
(defun rw-next (rwthing alist)
  (let* ((this-rw (new rewrite of (append rwthing '((rw-end -> rw-end)))
                       :initially (append alist (list 'rw-end))))
         (sink (next this-rw (+ (length alist) 1))))
    (loop for x = (next this-rw) until (eql x 'rw-end) collect x)))

;; RWGEN -- returns arbitrary generation of rewrite
;; (rwgen mtrules '(1 0) 2) =  (1 0 0 1 0 1 1 0)
(defun rwgen (rwrules initgen gennbr)
  (case gennbr 
    (0 initgen)
    (1 (rw-next rwrules initgen))
    (t (rw-next rwrules (rwgen rwrules initgen (- gennbr 1))))))

(defun inverse-codeword (cw)
  (loop for x in cw collect (if (eql x 1) 0 1)))

(defun rw-gens (rw gens)
  (loop repeat gens collect (next rw t)))

;; CODE->SLOTS -- placing 'input' into 1-slots, rests otherwise
;; refined Oct. 2006
(defun code->slots (a-code input)
  (if (pattern? input)
      (loop for x in a-code collect
	    (if (eql x 0) 'r (next input)))
      (let ((a-cyc (makecyc input)))
	(loop for x in a-code collect
	      (if (or (eql x 0)
		      (eop? a-cyc))
		  'r (next a-cyc))))))

(defun slots->code (slotlist)
  (loop for x in slotlist collect
        (if (eql x 'r) 0 1)))

;; ...and now for some automata ....

;; AUTO-1D -- 1-dimensional CA example
;; from Flake, "Computational Beauty" p. 233
;; 'start-idx' can be number or list
;; example
; (code->slots (flatten (rw-gens (auto-1d 5) 5)) (indices 20))
; =  (0 R R R R 1 2 R R R 3 4 5 R R 6 R 7 8 R 9 10 11 12 R)
(defun auto-1d (a-number &optional (starts 0))
  (new rewrite :of '((0 :id 0)
                     (1 :id 1))
       :initially (codeword a-number starts)
       :rules '(
                (0 (0) 0 -> 0)
                (0 (0) 1 -> 1)
                (0 (1) 0 -> 1)
                (0 (1) 1 -> 1)
                (1 (0) 0 -> 1)
                (1 (0) 1 -> 1)
                (1 (1) 0 -> 1)
                (1 (1) 1 -> 0)
                (1 -> 1)
                (0 -> 0)
                )
       :for a-number))

;; LIFE1D -- generates (firstnum, secondnum) life automata 
;; for periodlen*cycles 
;; (life1d 50 'r 5 5 '(1 2)) = 
;; (R R 50 50 50 R R R 50 50 R 50 R R 50 R R R R 50 R 50 50 R 50)
(defun life1d (firstnum secondnum periodlen cycles &optional (starts 0))
  (let ((intern-rw 
         (new rewrite 
           :of `((,firstnum :id 0)
                 (,secondnum :id 1))
           :initially (codeword periodlen starts)
           :rules '(
                    (0 (0) 0 -> 0)
                    (0 (0) 1 -> 1)
                    (0 (1) 0 -> 1)
                    (0 (1) 1 -> 1)
                    (1 (0) 0 -> 1)
                    (1 (0) 1 -> 1)
                    (1 (1) 0 -> 1)
                    (1 (1) 1 -> 0)
                    (1 -> 1)
                    (0 -> 0)))))
    (next intern-rw (* periodlen cycles))))


;; life2d stuff does not use 'rewrite', but included here anyway

;; NEIGHBORSUM -- utility for 'life2d'
;; sums all entries surrounding entry with coordinates (x y) in an array 
(defun neighborsum (x y ary)
  (let* ((xlen (first (array-dimensions ary)))
         (ylen (second (array-dimensions ary))))
    (- 
     (loop for a from -1 to 1 sum
           (loop for b from -1 to 1 sum
                 (aref ary (mod (+ a x) xlen) (mod (+ b y) ylen))))
     (aref ary x y))))

;; LIFECELL -- utility for 'life2d'
;;  performs basic rule for a cell; returns cell's new value
(defun lifecell (x y ary)
  (let ((ns (neighborsum x y ary)))
    (case (aref ary x y) 
      (1 (if (or (< ns 2) (> ns 3)) 0 1))
      (0 (if (= ns 3) 1 0))
      (t (aref ary x y)))))

;; LIFE2D -- generates next-generation array by life rules 
(defun life2d (ary)
  (let* ((xlen (first (array-dimensions ary)))
         (ylen (second (array-dimensions ary)))
         (arylist (loop for x to (- xlen 1) collect
                            (loop for y to (- ylen 1) collect 
                                  (lifecell x y ary)))))
    (make-array (array-dimensions ary)
                :initial-contents arylist)))

;; LIFE2DGEN -- returns nth-generation life array 
(defun life2dgen (initarray gennbr)
  (case gennbr         
    (0 initarray)
    (1 (life2d initarray))
    (t (life2d (life2dgen initarray (- gennbr 1))))))

;; RAND01ARRAY -- generates 0-1 array of specified 'density' of 1s
(defun rand01array (xsize ysize density)
  (let* ((places (* xsize ysize))
         (onecount (floor (* places density)))
         (entries 
          (append
           (loop repeat onecount collect 1)
           (loop repeat (- places onecount) collect 0))))
    (make-array 
     (list ysize xsize)
     :initial-contents
     (make-poly (shuffle entries) (list xsize)))))

;;; melodic stuff

;; RW-STEPPER -- selfsim melodic steps in one long list
(defun rw-stepper (rules startnum len &optional (rate 1))
  (let ((pat (new range :initially startnum 
                  :stepping (new rewrite
                              of rules))))
    (loop repeat len collect (next pat))))


;; some step-rules
(defparameter steprules1 '((1 -> (1 -1 1))
                     (-1 -> (-1 -1 1))))


;; HEY LOOK! -- use generations of steps
;; (melint->line 50 (rwgen steprules1 '(1 -1) 4))

;; RWMANGLE -- short/long rhythmic rewrite
;; from T. Johnson, p. 108
;; (rwmangle .25 3 7) = (0.25 3 3 0.25 0.25 3 0.25)
(defun rwmangle (smalldur largedur len)
  (let ((intern-rw 
         (new rewrite 
           :of `((,smalldur :id 1)
                 (,largedur :id 2))
           :initially '(1)
           :rules '((2 -> 1 1 2)
                    ((1 1) -> 2 2)))))
    (next intern-rw len)))


;; added May 2005
;; from Allouche & Shallit 2003

;; Jacobs "Mephisto Waltz" infinite word -- fourth-power-free!
;; p. 25
(defparameter mephistorules '((0 -> (0 0 1))
                        (1 -> (1 1 0))))

;; p. 25
(defparameter cubefreerules '((0 -> (0 0 1))
                        (1 -> (0 1 1))))

; p. 27 (#30)
;; limit h^n(a) is squarefree
(defparameter sfree3rules
 '((0 -> (0 1 2 0 1))
    (1 -> (0 2 0 1 2 1))
    (2 -> (0 2 1 2 0 2 1))))

; p. 27 (#32)
;; limit h^n(a) is squarefree
(defparameter sfree4rules
'((0 -> (0 1 2 3))
  (1 -> (1 0 2 3))
  (2 -> (2 0 1 3))
  (3 -> (2 1 0 3))))

; p. 33 
; non-uniform morphism
; fixed point is squarefree
(defparameter hawksfreerules
 '((0 -> (1 0 2 1 2 0 2 1 0 1 2 1 0 2 0))
   (1 -> (1 0 2 1 0 1 2 1 0 2 1 2 0 2 1 0 2 0))
   (2 ->  (1 0 2 1 2 0 1 0 2 0 1 2 1 0 2 0))))

; p. 33
; 13-uniform morphism
; fixed point is squarefree
(defparameter morph13rules
  '((0 -> (0 1 2 1 0 2 1 2 0 1 2 1 0))
    (1 -> (1 2 0 2 1 0 2 0 1 2 0 2 1))
    (2 -> (2 0 1 0 2 1 0 1 2 0 1 0 2))))

; p. 34
; 12-uniform morphism
; fixed point is squarefree
(defparameter morph12rules
  '((0 -> (2 0 2 1 2 0 1 0 2 1 0 1))
    (1 -> (2 0 1 0 2 1 2 0 2 1 0 1))
    (2 -> (2 1 0 2 1 2 0 1 2 1 0 1))))

; p. 34
; non-uniform
; fixed point is squarefree
(defparameter pleasantsrules
 '((0 -> (0 1 2 0 1))
    (1 -> (0 2 0 1 2 1))
    (2 -> (0 2 1 2 0 2 1))))

; p. 176
; Baum-Sweet sequence
; then must apply 'bscoding'
; e.g. (bscoding (rwgen bsrules '(a) 3))
(defparameter bsrules
'((0 -> (0 1))
  (1 -> (2 1))
  (2 -> (1 3))
  (3 -> (3 3))))
(defun bscoding (bslist)
  (sublis '((0 . 1) (1 . 1) (2 . 0) (3 . 0)) bslist))

;; PROUHET -- generalized prouhet morphism
(defun prouhet (len)
  (loop for n to (- len 1) collect 
	`(,n -> ,(rotate-list (indices len) n))))

;; TOWER OF HANOI (p. 177)
; a  = move from 1 to 2
; ai = move from 2 to 1
; b  = move from 2 to 3
; bi = move from 3 to 2
; c  = move from 3 to 1
; ci = move from 1 to 3

(defparameter hanoirules
  '((a -> (a ci))
    (b -> (c bi))
    (c -> (b ai))
    (ai -> (a c))
    (bi -> (c b))
    (ci -> (b a))))

; example: (hanoi 'a 2) = (a ci b)
(defun hanoimoves (move size)
  (butlast (rwgen hanoirules (list move) size)))

(defun hanoimap (hsym triplet)
  (let* ((t1 (first triplet))
         (t2 (second triplet))
         (t3 (third triplet))
         (doit  (case hsym 
                  ('a (push (pop t1) t2))
                  ('b (push (pop t2) t3))
                  ('c (push (pop t3) t1))
                  ('ai (push (pop t2) t1))
                  ('bi (push (pop t3) t2))
                  ('ci (push (pop t1) t3)))))
    (list t1 t2 t3)))

(defun hanoichain (triplet hlist)
  (cons triplet
        (when hlist
          (hanoichain
           (hanoimap (first hlist) triplet) (cdr hlist)))))

; utility to determine move from start & end indices
(defun idmove (start-id end-id)
  (let ((idlist (list start-id end-id)))
    (cond
      ((and (eq start-id 0) (eq end-id 1)) 'a)
      ((and (eq start-id 1) (eq end-id 0)) 'ai)
      ((and (eq start-id 1) (eq end-id 2)) 'b)
      ((and (eq start-id 2) (eq end-id 1)) 'bi)
      ((and (eq start-id 2) (eq end-id 0)) 'c)
      ((and (eq start-id 0) (eq end-id 2)) 'ci))))

; the main function
(defun hanoi (list1 list2 list3 start-id end-id movesize)
  (let ((triplet (list list1 list2 list3)))
    (hanoichain triplet (hanoimoves (idmove start-id end-id) movesize))))

;; Peano space-filling curve (p. 202)

(defparameter peanorules
  '((S1 -> (S1 R1 R2))
    (S2 -> (S2 L1 L2))
    (R1 -> (S2 L1 L2))
    (R2 -> (S1 S2 S1))
    (L1 -> (S1 R1 R2))
    (L2 -> (S2 S1 S2))))  

(defun peanocoding (plist)
  (sublis '((S1 . S) (S2 . S) (S3 . S)
            (R1 . R) (R2 . R) (R3 . R)
            (L1 . L) (L2 . L) (L3 . L)) plist))

; Fibonacci morphism (p. 212)
; fixed point of f(0)
(defparameter fiborules
  '((0 -> (0 1))
    (1 -> 0)))

;; CYCLIC TOWER OF HANOI (p. 243)
;; like TOH; only a,b,c are used (not inverses)
;; with coding (after morphism) to remove subscripts
; a1 = move from 1 to 3
; a2 = move from 2 to 3
; b1 = move from 2 to 1
; b2 = move from 3 to 1
; c1 = move from 3 to 2
; c2 = move from 1 to 2

(defparameter c-hanoirules
  '((a1 -> (a1 b2 a1))
    (b1 -> (b1 c2 b1))
    (c1 -> (c1 a2 c1))
    (a2 -> (b1 a1))
    (b2 -> (c1 b1))
    (c2 -> (a1 c1))))

(defun c-hanoicoding (chlist)
  (sublis '((a1 . a) (a2 . a)
            (b1 . b) (b2 . b)
            (c1 . c) (c2 . c)) chlist))

; example: (c-hanoimoves 'c2 2) = (A B A C A)
(defun c-hanoimoves (move size)
  (butlast (c-hanoicoding (rwgen c-hanoirules (list move) size))))

(defun c-hanoimap (hsym triplet)
  (let* ((t1 (first triplet))
         (t2 (second triplet))
         (t3 (third triplet))
         (doit  (case hsym 
                  ('a (push (pop t1) t2))
                  ('b (push (pop t2) t3))
                  ('c (push (pop t3) t1)))))
    (list t1 t2 t3)))

(defun c-hanoichain (triplet hlist)
  (cons triplet
        (when hlist
          (c-hanoichain
           (c-hanoimap (first hlist) triplet) (cdr hlist)))))

; utility to determine move from start & end indices
(defun c-idmove (start-id end-id)
  (let ((idlist (list start-id end-id)))
    (cond
      ((and (eq start-id 0) (eq end-id 1)) 'c2)
      ((and (eq start-id 1) (eq end-id 0)) 'b1)
      ((and (eq start-id 1) (eq end-id 2)) 'a2)
      ((and (eq start-id 2) (eq end-id 1)) 'c1)
      ((and (eq start-id 2) (eq end-id 0)) 'b2)
      ((and (eq start-id 0) (eq end-id 2)) 'a1))))

; the main function
; example: (c-hanoi '(1 2 3) '() '() 0 1 2)
; = (((1 2 3) NIL NIL) ((2 3) (1) NIL) ((2 3) NIL (1)) ((3) (2) (1))
;   ((1 3) (2) NIL) ((3) (1 2) NIL))
(defun c-hanoi (list1 list2 list3 start-id end-id movesize)
  (let ((triplet (list list1 list2 list3)))
    (c-hanoichain triplet (c-hanoimoves (c-idmove start-id end-id) movesize))))

;;; PAPERFOLDING from set of 4 (p. 304)
;; fixed point at a is interior sequence of paperfolding
;; p(n) = 4n for all n >= 1
(defparameter paperfoldrules
   '((0 -> (0 1))
    (1 -> (2 1))
    (2 -> (0 3))
    (3 -> (2 3))))


;; TENTMAP
;; 's' should be between 1 & 2
;; 'initval' should be between 0 & 1
(defun tentmap (n s initval)
  (if (= 0 n) initval 
      (let ((prev-tentmap (tentmap (- n 1) s initval)))
	(if (<  prev-tentmap 0.5)
	    (* s prev-tentmap)
	    (* s (- 1 prev-tentmap))))))

;; TENTMAPS -- vector of 'tentmap' from 0 to n-1
(defun tentmaps (n s initval)
  (loop for num to (- n 1) collect
	(tentmap num s initval)))
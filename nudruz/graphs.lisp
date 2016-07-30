;; GRAPHS.LISP
;; some CM graphs from group theory

;; Drew Krause, 2004
;; drkrause@mindspring.com
;; www.wordecho.org

;; D4-GRAPH: dihedral group (4) -- size 4
(defparameter d4-graph (new graph :of
		      `((a :id 1 :to ,(new weighting of '(2 3)))
			(b :id 2 :to ,(new weighting of '(1 4)))
			(c :id 3 :to ,(new weighting of '(1 4)))
			(d :id 4 :to ,(new weighting of '(2 3))))))

;; TRANS8C-GRAPH: transitive group (8) id 3 -- size 8
(defparameter trans8c-graph (new graph :of
			   `((a :id 1 :to ,(new weighting of '(3 5 8)))
			     (b :id 2 :to ,(new weighting of '(3 6 8)))
			     (c :id 3 :to ,(new weighting of '(1 2 7)))
			     (d :id 4 :to ,(new weighting of '(5 6 8)))
			     (e :id 5 :to ,(new weighting of '(1 4 7)))
			     (f :id 6 :to ,(new weighting of '(2 4 7)))
			     (g :id 7 :to ,(new weighting of '(3 5 6)))
			     (h :id 8 :to ,(new weighting of '(1 2 4))))))

;; TRANS6A-GRAPH: transitive group (6) id 1 -- size 6
;; "kinda cyclic"
(defparameter trans6a-graph (new graph :of
			   `((a :id 1 :to ,(new weighting of '(2 6)))
			     (b :id 2 :to ,(new weighting of '(1 3)))
			     (c :id 3 :to ,(new weighting of '(2 4)))
			     (d :id 4 :to ,(new weighting of '(3 5)))
			     (e :id 5 :to ,(new weighting of '(4 6)))
			     (f :id 6 :to ,(new weighting of '(1 5))))))

;; TRANS6B-GRAPH: transitive group (6) id 2 -- size 6
;; "no steps"
(defparameter trans6b-graph (new graph :of
			   `((a :id 1 :to ,(new weighting of '(3 4 5)))
			     (b :id 2 :to ,(new weighting of '(4 5 6)))
			     (c :id 3 :to ,(new weighting of '(1 5 6)))
			     (d :id 4 :to ,(new weighting of '(1 2 6)))
			     (e :id 5 :to ,(new weighting of '(1 2 3)))
			     (f :id 6 :to ,(new weighting of '(2 3 4))))))

;; TRANS6C-GRAPH: transitive group (6) id 3 --- size 12
(defparameter trans6c-graph (new graph :of
		      `((a :id 1 :to ,(new weighting of '(4 7 11)))
			(b :id 2 :to ,(new weighting of '(3 8 12)))
			(c :id 3 :to ,(new weighting of '(2 5 10)))
			(d :id 4 :to ,(new weighting of '(1 6 9)))
			(e :id 5 :to ,(new weighting of '(3 7 11)))
			(f :id 6 :to ,(new weighting of '(4 8 12)))
			(g :id 7 :to ,(new weighting of '(1 5 9)))
			(h :id 8 :to ,(new weighting of '(2 6 10)))
			(i :id 9 :to ,(new weighting of '(4 7 12)))
			(j :id 10 :to ,(new weighting of '(3 8 11)))
			(k :id 11 :to ,(new weighting of '(1 5 10)))
			(l :id 12 :to ,(new weighting of '(2 6 9))))))

;; SMALL12A-GRAPH: small group (12) id 1 --- size 12
(defparameter small12a-graph (new graph :of
		      `((a :id 1 :to ,(new weighting of '(2 3 4 5 8)))
			(b :id 2 :to ,(new weighting of '(1 3 5 6 10)))
			(c :id 3 :to ,(new weighting of '(1 2 5 7 11)))
			(d :id 4 :to ,(new weighting of '(1 6 7 8 9)))
			(e :id 5 :to ,(new weighting of '(1 2 3 9 12)))
			(f :id 6 :to ,(new weighting of '(2 4 7 9 10)))
			(g :id 7 :to ,(new weighting of '(3 4 6 9 11)))
			(h :id 8 :to ,(new weighting of '(1 4 10 11 12)))
			(i :id 9 :to ,(new weighting of '(4 5 6 7 12)))
			(j :id 10 :to ,(new weighting of '(2 6 8 11 12)))
			(k :id 11 :to ,(new weighting of '(3 7 8 10 12)))
			(l :id 12 :to ,(new weighting of '(5 8 9 10 11))))))

;; SMALL12B-GRAPH: small group (12) id 2 --- size 12
(defparameter small12b-graph (new graph :of
		      `((a :id 1 :to ,(new weighting of '(2 3 4 6 7)))
			(b :id 2 :to ,(new weighting of '(1 4 5 6 9)))
			(c :id 3 :to ,(new weighting of '(1 5 7 8 10)))
			(d :id 4 :to ,(new weighting of '(1 2 6 8 11)))
			(e :id 5 :to ,(new weighting of '(2 3 8 9 10)))
			(f :id 6 :to ,(new weighting of '(1 2 4 10 12)))
			(g :id 7 :to ,(new weighting of '(1 3 9 11 12)))
			(h :id 8 :to ,(new weighting of '(3 4 5 10 11)))
			(i :id 9 :to ,(new weighting of '(2 5 7 11 12)))
			(j :id 10 :to ,(new weighting of '(3 5 6 8 12)))
			(k :id 11 :to ,(new weighting of '(4 7 8 9 12)))
			(l :id 12 :to ,(new weighting of '(6 7 9 10 11))))))

;; ALT4-GRAPH: alternating group (4) --- size 12
(defparameter alt4-graph (new graph :of
		      `((a :id 1 :to ,(new weighting of '(2 3 5 7)))
			(b :id 2 :to ,(new weighting of '(1 3 9 10)))
			(c :id 3 :to ,(new weighting of '(1 2 4 11)))
			(d :id 4 :to ,(new weighting of '(3 5 6 11)))
			(e :id 5 :to ,(new weighting of '(1 4 6 7)))
			(f :id 6 :to ,(new weighting of '(4 5 8 0)))
			(g :id 7 :to ,(new weighting of '(1 5 8 9)))
			(h :id 8 :to ,(new weighting of '(6 7 9 0)))
			(i :id 9 :to ,(new weighting of '(2 7 8 10)))
			(j :id 10 :to ,(new weighting of '(2 9 11 0)))
			(k :id 11 :to ,(new weighting of '(3 4 10 0)))
			(l :id 0 :to ,(new weighting of '(6 8 10 11))))))

;; NONREPEATING-GRAPH
;; graph with no loops; takes a list
(defmacro nonrepeating-graph (vals)
  `(new weighting of
    (loop for x in ,vals collect
	   (list x :max 1))))

;; NEIGHBOR-NODELIST
;; utility for 'neighbor-graph'
(defun neighbor-nodelist (mylist &optional (leapfactor 3))
  (let* ((listlen (length mylist)))
    (loop for n to  (- listlen 1) collect
	  (let ((v (nth n mylist)))
	    `(,v :id ,v :to
	      ,(new weighting :of 
		    (mapcar (lambda (x)
			      (list (nth x mylist) :weight
				    (if (eql n x) 0
					(max 0
					     (- leapfactor
						(abs (- x n)))))))
			    (indices listlen))))))))

;; NEIGHBOR-GRAPH
;; graph that favors neighbors over 'leaps'
;; leapfactor determines width of allowed leaps (generally)
(defmacro neighbor-graph (alist &optional (leapfactor 3))
`(new graph of (neighbor-nodelist ,alist ,leapfactor)))

;; TAXI-NODELIST
;; utility for 'taxi-graph'
(defun taxi-nodelist (sroot)
  (let* ((listlen (* sroot sroot))
	 (masterlist
	  (match2lists (indices sroot) (indices sroot)))
	 (place+steps
	  (loop for m in masterlist collect
		(mapcar (lambda (x) (position x masterlist :test #'seq-eql))
			(cons m
			      (set-difference 
			       (filter 
				(lambda (x) (or (eql (first m) (first x))
						(eql (second m) (second x))))
				masterlist)
			       (list m)))))))
    (loop for x in place+steps collect
	  `(,(car x) :id ,(car x) :to
	    ,(new weighting :of (cdr x))))))

;; TAXI-GRAPH
;; vertical & horizontal moves within a square 
;; [moves only 'vertically' & 'horizontally'] 
;; eg '(1 0) --> (1 3) or (2 0), but not (2 2)
;; takes square root ie, length of side; # chords
;; returns indices (integers)
;; can be used for bzmult-simple chord matrices
(defmacro taxi-graph (sroot)
`(new graph of (taxi-nodelist ,sroot)))

;; BZ-MATX -- complete matrix of bzmult-simple
;; takes list of chords
(defun bz-matx (chds)
  (loop for chd in chds append
	(mapcar 
	 (lambda (x) (bzmult-simple chd x))
		 chds)))

;; BZ-TAXIPATH
;; generates taxi-graph path of 'chds' of length 'len'
(defun bz-taxipath (chds len)
  (chooser
   (next (taxi-graph (length chds)) len)
   (bz-matx chds)))


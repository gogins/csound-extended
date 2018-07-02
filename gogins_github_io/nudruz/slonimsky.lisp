(in-package :cm)

;; patterns from the Slonimsky "Thesaurus"

;; 12-tone patterns
(defparameter patt500
    (flatten
     (expand '((0 3 6 9) (1 0 5)))))

(defparameter patt503
    (flatten
     (expand '((0 3 6 9) (1 0 8)))))

(defparameter patt505
    (flatten
     (expand '((0 3 6 9) (2 0 7)))))

(defparameter patt508
    (flatten
     (expand '((0 3 6 9) (2 0 10)))))

(defparameter patt512
    (flatten
     (expand '((0 3 6 9) (4 0 8)))))

(defparameter ttone-patts
    (list patt500 patt503 patt505 patt508 patt512))

;; SLONIM-TT -- returns a randomly-generated 12-tone slonimsky pattern
;; note: length unpredictable, 1st member may not be 0, could include >12 or <0
(defun slonim-tt ()
  (let* ((tm (+ 1 (random 10)))       
	 (tlevels (mod12 (transp (indices 12) tm #'*)))
	 (fig 
	  (loop for si in (shuffle (indices tm)) collect
		(pickl
		 (filter 
		  (lambda (x) (eql si (mod x tm)))
		  (indices 13 -6))))))
    (flatten (expand (list tlevels fig)))))

;; SLONIM-PATH -- randomly-generated 'slonim-tt' path from pit1 to pit2
;; returns start+end if difference > 20
(defun slonim-path (pit1 pit2)
  (let* ((sstt (slonim-tt))
	 (ttsubs (loop for n from 5 to 11 append
		       (subsequences sstt n)))
	 (pdiff (- pit2 pit1))
	 (matchmels 
	  (no-nils (mapcar (lambda (x) (if (eql pdiff (- (car (last x)) (car x))) x))
			   ttsubs))))
    (if (> (abs (- pit1 pit2)) 20)
	(list pit1 pit2)
    (if matchmels 
	(transp-to pit1 (pickl matchmels))
	(slonim-path pit1 pit2)))))

;; SLONIM-EXPAND -- 'slonim-path' applied to entire melody
(defun slonim-expand (melody &optional (treeflag nil))
  (let ((out-tree
	 (loop for n to (- (length melody) 2) collect
	       (butlast (slonim-path (nth n melody)
				     (nth (+ n 1) melody))))))
    (if treeflag out-tree (flatten out-tree))))

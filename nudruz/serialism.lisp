
;; 'mallalieu.lisp' = mallalieu (selfsim) & other rows

;; fun with mallalieu rows

(defparameter mallalieu '(0 1 4 2 9 5 11 3 8 10 7 6))
(defparameter mallalieu-5 '(0 5 8 10 9 1 7 3 4 2 11 6))
(defparameter mallalieu-7 '(0 7 4 2 3 11 5 9 8 10 1 6))
(defparameter mallalieu-11 '(0 11 8 10 3 7 1 9 4 2 5 6))

;; MALLAL-DOUBLER -- doubles row at 'startpitz' at rate 'ifactorz'
;; startpitz,ifactorz can be lists (must be eql length) or single vals
;; returns either merged slots (default) or sdur vecs
;; includes 'mrow' in answer (merged, or first of sdur vecs)
(defun mallal-doubler (mrow startpitz ifactorz &optional (sd-flag nil))
  (let* ((startpits (if (listp startpitz) startpitz (list startpitz)))
	 (ifactors (if (listp ifactorz) ifactorz (list ifactorz)))
	 (longest-ifac (apply #'max ifactors))
	 (mplacez
	  (loop for ifactor in ifactors collect
		(loop for n in (transp (transp (indices 12) ifactor #'*) (- ifactor 1))
		      collect  (mod n 13))))
	 (mpitz
	  (loop for mplaces in mplacez collect
		(mapcar (lambda (x) (nth x mrow)) mplaces)))
	 (rowvecz 
	  (loop for ifactor in ifactors collect
		(copylist mrow ifactor)))
	 (nuslotz 
	  (map 'list
	       (lambda (startpit mpits rowvec)
		 (transp-to startpit (extract-list mpits rowvec)))
	       startpits
	       mpitz
	       rowvecz))
	 (as-slots
	  (merge-slots (append rowvecz nuslotz)))
	 (as-sd
	  (cons
	   (list (copylist mrow longest-ifac) (copylist (list 1) (* (length mrow) longest-ifac)))
	   (mapcar #'slots->durs nuslotz))))
    (if sd-flag as-sd as-slots)))

;; MALLAL-INTDOUBLER -- doubles row at 'dblintz' at rate 'ifactorz'
;; dblintz,ifactorz can be lists (must be eql length) or single vals
;; returns either merged slots (default) or sdur vecs
;; includes 'mrow' in answer (merged, or first of sdur vecs)
(defun mallal-intdoubler (mrow dblintz ifactorz &optional (sd-flag nil))
  (let* ((dblints (if (listp dblintz) dblintz (list dblintz)))
	 (ifactors (if (listp ifactorz) ifactorz (list ifactorz)))
	 (longest-ifac (apply #'max ifactors))
	 (mplacez
	  (loop for ifactor in ifactors collect
		(loop for n in (transp (transp (indices 12) ifactor #'*) (- ifactor 1))
		      collect  (mod n 13))))
	 (mpitz
	  (loop for mplaces in mplacez collect
		(mapcar (lambda (x) (nth x mrow)) mplaces)))
	 (rowvecz 
	  (loop for ifactor in ifactors collect
		(copylist mrow ifactor)))
	 (nuslotz 
	  (map 'list
	       (lambda (dblint mpits rowvec)
		 (transp (extract-list mpits rowvec) dblint))
	       dblints
	       mpitz
	       rowvecz))
	 (as-slots
	  (merge-slots (append rowvecz nuslotz)))
	 (as-sd
	  (cons
	   (list (copylist mrow longest-ifac) (copylist (list 1) (* (length mrow) longest-ifac)))
	   (mapcar #'slots->durs nuslotz))))
    (if sd-flag as-sd as-slots)))

;; OTHER ROWS
;; Lutoslawski etc

;; COMPLETIONP -- check to see if rows have aggregate completion (any mod)
(defun completionp (inlist &optional (modlen 12))
  (mapcar (lambda (x) (list-eql (indices modlen) x))
	  (mod12 (make-poly inlist modlen 'fit))))

(defparameter lutos1
    (butlast (mod12 (melint->line 0 (copylist '(6 11) 12)))))

(defparameter lutos2
    (butlast (mod12 (melint->line 0 (copylist '(2 5 2 7) 3)))))

;; limited interval rows -- from minion

(defparameter row-6e 
    (first
     (make-poly
      (mod12 (melint->line 0
			   (copylist (interlock (list 6) (list 11) 1 1) 30)))
      12)))

(defparameter fullrow-6e
    (flatten
     (subseq 
      (make-poly
       (mod12 (melint->line 0
			    (copylist (interlock (list 6) (list 11) 1 1) 30)))
       12)
      0 2)))

(defparameter row-23
    '(0 2 4 1 3 5 7 10 8 6 9 11))

(defparameter row-25
    '(0 2 4 6 1 3 8 10 5 7 9 11))

;; '(2 2 5) melint
;; complete/full as is
(defparameter row-225-trich
    '(0 2 4 9 11 1 6 8 10 3 5 7))

(defparameter row-252-trich
    (butlast (mod12 (melint->line 0 (copylist '(2 5 2) 4)))))

(defparameter row-522-trich
    (butlast (mod12 (melint->line 0 (copylist '(5 2 2) 4)))))

(defparameter row-34
    '(0 4 1 10 2 11 8 5 9 6 3 7))

(defparameter row-34-tetra
    '(0 3 6 9 1 4 7 10 2 5 8 11))

(defparameter row-3t-tetra
    '(0 3 6 9 7 10 1 4 2 5 8 11))

;; complete/full by itself
(defparameter row-35
    '(0 3 6 1 4 7 10 5 8 11 2 9))

;; '(4 4 5) melints
(defparameter row-45-trich
    '(0 4 8 1 5 9 2 6 10 3 7 11))

(defparameter fullrow-45-trich
    (flatten
     (subseq
      (make-poly  
       (mod12 (melint->line 0
			    (copylist '(4 4 5) 50)))
       12 'fit)
      0 3)))

;; carter symmetrical-inversion row 1

(defparameter carter1 (mod12 (melint->line 0 '(2 7 4 3 1 6 11 9 8 5 10))))

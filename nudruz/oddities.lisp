;; ASYM-RHYTS.LISP -- asymmetrical rhythms [after Chemillier, Truchet, et. al.]
;; those with 'rhythmic oddity' property

;; ODDITY-IDX -- takes durs; returns number of atx that split totalen evenly
(defun oddity-idx (alist)
  (let ((totalen (apply #'+ alist)))
    (if (oddp totalen) 0
	(let* ((div2 (/ totalen 2))
	       (atkpts (melint->line 0 alist))
	       (pairs (subsets-len atkpts 2)))
	  (- 
	   (count div2
		  (mapcar (lambda (x) (mod (apply #'- x) totalen)) pairs))
	   (if (member div2 atkpts) 1 0))))))

;; functions selecting 'oddities'

;; ODDITIES-LEN -- all oddities of total durlen 'len'
(defun oddities-len (len)
  (no-nils
   (mapcar
    (lambda (x) (if (eql (apply #'+ x) len) x))
    oddities)))

;; ODDITIES-ATX -- all oddities with same # atx
(defun oddities-atx (atx)
  (no-nils
   (mapcar
    (lambda (x) (if (eql (length x) atx) x))
    oddities)))

;; ODDITIES - all listed asymmetrical rhythms (including rotations) -- all unique
;; nbr atx: (3 4 5 7 9 11)
;; durations: (8 11 12 16 18 20 22 24)
(define oddities
    '((2 3 3) (3 3 2) (3 2 3) (2 3 2 3 2) (3 2 3 2 2) (2 3 2 2 3) (3 2 2 3 2)
      (2 2 3 2 3) (2 3 2 2 3 2 2) (3 2 2 3 2 2 2) (2 2 3 2 2 2 3) (2 3 2 2 2 3 2)
      (3 2 2 2 3 2 2) (2 2 2 3 2 2 3) (2 2 3 2 2 3 2) (2 3 2 2 2 3 2 2 2)
      (3 2 2 2 3 2 2 2 2) (2 2 2 3 2 2 2 2 3) (2 2 3 2 2 2 2 3 2)
      (2 3 2 2 2 2 3 2 2) (3 2 2 2 2 3 2 2 2) (2 2 2 2 3 2 2 2 3)
      (2 2 2 3 2 2 2 3 2) (2 2 3 2 2 2 3 2 2) (2 3 2 2 2 2 3 2 2 2 2)
      (3 2 2 2 2 3 2 2 2 2 2) (2 2 2 2 3 2 2 2 2 2 3) (2 2 2 3 2 2 2 2 2 3 2)
      (2 2 3 2 2 2 2 2 3 2 2) (2 3 2 2 2 2 2 3 2 2 2) (3 2 2 2 2 2 3 2 2 2 2)
      (2 2 2 2 2 3 2 2 2 2 3) (2 2 2 2 3 2 2 2 2 3 2) (2 2 2 3 2 2 2 2 3 2 2)
      (2 2 3 2 2 2 2 3 2 2 2) (2 3 3 3) (3 3 3 2) (3 3 2 3) (3 2 3 3)
      (2 3 3 2 3 3 2) (3 3 2 3 3 2 2) (3 2 3 3 2 2 3) (2 3 3 2 2 3 3)
      (3 3 2 2 3 3 2) (3 2 2 3 3 2 3) (2 2 3 3 2 3 3) (2 3 3 2 3 2 3)
      (3 3 2 3 2 3 2) (3 2 3 2 3 2 3) (2 3 2 3 2 3 3) (3 2 3 2 3 3 2)
      (2 3 2 3 3 2 3) (3 2 3 3 2 3 2) (2 3 3 2 2 3 3 2 2) (3 3 2 2 3 3 2 2 2)
      (3 2 2 3 3 2 2 2 3) (2 2 3 3 2 2 2 3 3) (2 3 3 2 2 2 3 3 2)
      (3 3 2 2 2 3 3 2 2) (3 2 2 2 3 3 2 2 3) (2 2 2 3 3 2 2 3 3)
      (2 2 3 3 2 2 3 3 2) (2 3 3 2 2 3 2 3 2) (3 3 2 2 3 2 3 2 2)
      (3 2 2 3 2 3 2 2 3) (2 2 3 2 3 2 2 3 3) (2 3 2 3 2 2 3 3 2)
      (3 2 3 2 2 3 3 2 2) (2 3 2 2 3 3 2 2 3) (3 2 2 3 3 2 2 3 2)
      (2 2 3 3 2 2 3 2 3) (2 3 2 3 2 3 2 3 2) (3 2 3 2 3 2 3 2 2)
      (2 3 2 3 2 3 2 2 3) (3 2 3 2 3 2 2 3 2) (2 3 2 3 2 2 3 2 3)
      (3 2 3 2 2 3 2 3 2) (2 3 2 2 3 2 3 2 3) (3 2 2 3 2 3 2 3 2)
      (2 2 3 2 3 2 3 2 3) (2 3 3 3 3 3 3) (3 3 3 3 3 3 2) (3 3 3 3 3 2 3)
      (3 3 3 3 2 3 3) (3 3 3 2 3 3 3) (3 3 2 3 3 3 3) (3 2 3 3 3 3 3)
      (2 3 3 3 2 3 3 3 2) (3 3 3 2 3 3 3 2 2) (3 3 2 3 3 3 2 2 3)
      (3 2 3 3 3 2 2 3 3) (2 3 3 3 2 2 3 3 3) (3 3 3 2 2 3 3 3 2)
      (3 3 2 2 3 3 3 2 3) (3 2 2 3 3 3 2 3 3) (2 2 3 3 3 2 3 3 3)
      (2 3 3 3 2 3 3 2 3) (3 3 3 2 3 3 2 3 2) (3 3 2 3 3 2 3 2 3)
      (3 2 3 3 2 3 2 3 3) (2 3 3 2 3 2 3 3 3) (3 3 2 3 2 3 3 3 2)
      (3 2 3 2 3 3 3 2 3) (2 3 2 3 3 3 2 3 3) (3 2 3 3 3 2 3 3 2)
      (2 3 3 3 2 3 2 3 3) (3 3 3 2 3 2 3 3 2) (3 3 2 3 2 3 3 2 3)
      (3 2 3 2 3 3 2 3 3) (2 3 2 3 3 2 3 3 3) (3 2 3 3 2 3 3 3 2)
      (2 3 3 2 3 3 3 2 3) (3 3 2 3 3 3 2 3 2) (3 2 3 3 3 2 3 2 3)))
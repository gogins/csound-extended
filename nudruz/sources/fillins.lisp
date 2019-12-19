(in-package :cm)

;; This file adds features that were missing for various reasons in Common 
;; Music or in Drew Krause's code, or that I have thought worth adding 
;; without breaking the framework of Drew's or Common Music's code.

(defun map-subobjects (fn container &key key recurse test type)
"Maps FN to subobjects of CONTAINER, optionally recursively, optionally only 
to objects of TYPE. This function is declared in Common Music but not defined 
in Vinjar's release of Common Music 2."
  (let ((test (if type (lambda (x) (typep x type)) test)))
    (if key
        (if test
            (dolist (o (subobjects container))
              (if (and recurse (typep o <container>))
                  (map-subobjects fn o :key key :recurse recurse
                   :test test)
                  (when (funcall test o)
                    (funcall fn (funcall key o)))))
            (dolist (o (subobjects container))
              (if (and recurse (typep o <container>))
                  (map-subobjects fn o :key key :recurse recurse
                   :test test)
                  (funcall fn (funcall key o)))))
        (if test
            (dolist (o (subobjects container))
              (if (and recurse (typep o <container>))
                  (map-subobjects fn o :key key :recurse recurse
                   :test test)
                  (when (funcall test o) (funcall fn o))))
            (dolist (o (subobjects container))
              (if (and recurse (typep o <container>))
                  (map-subobjects fn o :key key :recurse recurse
                   :test test)
                  (funcall fn o)))))
    (values)))

(defun map-subcontainers (fn container &key key recurse)
"Maps FN to subcontainers of CONTAINER, optionally recursively, optionally 
only to objects of TYPE. This function is declared in Common Music but not 
defined in Vinjar's release of Common Music 2."
  (if key
      (dolist (o (subobjects container))
        (when (typep o <container>) (funcall fn (funcall key o)))
        (if recurse
            (map-subcontainers fn o :key key :recurse recurse)))
      (dolist (o (subobjects container))
        (when (typep o <container>) (funcall fn o))
        (if recurse
            (map-subcontainers fn o :key key :recurse recurse))))
    (values))
    
(defun gen-poisson-variate (mean &optional (*random-state* *random-state*))
  "Generate a pseudo-random number from a Poisson distribution
with mean M:

               K   - M
              M  %E
       P(K) = --------
                 K!

 MEAN       = Mean (M) of the distribution, M >= 0
 STATE      = random state to use.

 The output is an integer.
"
  (declare (type (double-float 0d0) mean))
  (let ((threshold 30d0))
    (cond ((< mean threshold)
	   ;; Direct generation
	   (let ((limit (exp (- mean))))
	     (do ((prod (random 1d0))
		  (n 1 (+ n 1)))
		 ((<= prod limit)
		  (- n 1))
	       (declare (fixnum n)
			(type (double-float 0d0) prod))
	       (setf prod (* prod (random 1d0))))))
	  (t
	   ;; Indirect generation
	   (let* ((alpha #.(coerce 7/8 'double-float)) ; Suggested value
		  (order (floor (* alpha mean)))
		  (x (gen-gamma-variate (dfloat order))))
	     (declare (fixnum order))
	     (if (< x mean)
		 (+ order (gen-poisson-variate (- mean x)))
		 (gen-binomial-variate (1- order)
				       (/ mean x))))))))
  

(defgeneric duration-seconds (sequence) 
    (:documentation "Returns the duration of the sequence in seconds.")
    )

(defmethod duration-seconds ((sequence seq)) 
  (reduce #'max (mapcar #'(lambda (e) (+ (object-time e)(midi-duration e))) 
  (subobjects sequence :class 'midi))))





(in-package :cm)

;; This file adds features that were missing for various reasons in Common 
;; Music or in Drew Krause's code.

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
  
(if (not (boundp 'gen-poisson-variate))
    (defun gen-poisson-variate (lambda_ &optional (*random-state* *random-state*)) 
"Returns a random sample from the CLMATH:POISSON-RANDOM-NUMBER function. 
This function is used in the NUDRUZ system and used to be found in the CLOCC 
system, but as CLOCC lacks a good out of the box experience, CLOCC functions 
will be replaced by other fill-ins as they are identified."
        (clmath:poisson-random-number lambda_)))




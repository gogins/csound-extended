(in-package :cm)

;; This file adds features that were missing for various reasons in Common 
;; Music or in Drew Krause's code.

; Missing from Vinjar's Common Music code.

(defun map-subobjects (fn container &key key recurse test type)
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

; Missing from Vinjar's Common Music code.

(defun map-subcontainers (fn container &key key recurse)
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
  
; Fill-in from the clmath package if clocc's cllib is not available.

(if (not (boundp 'gen-poisson-variate))
    (defun gen-poisson-variate (lambda_ &optional (*random-state* *random-state*)) 
        (clmath:poisson-random-number lambda_)))




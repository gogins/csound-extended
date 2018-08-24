(in-package :cm)

;; DIFFS.LISP -- difference equations

;; ..use it like this..

; (define-diff diff2
;   (0 1 1)
;   ((lambda (x) (* x x))
;    (lambda (x) (mod x 3))
;    (lambda (x) (* -1 x)))
;   (lambda (x) (mod x 12)))

; (diff-vals 'diff2 0 5)

; first some expansion utilities 

;; MAKE-DIFF-INITS -- sets all inits for a diff equation
(defun make-diff-inits (varname initlist)
  (loop for r to (- (length initlist) 1) collect
        `((= ,varname ,r) ,(nth r initlist))))

;; MAKE-DIFF-FUNC -- sets the 'else' clause for a diff equation
;; vname = variable name
;; fname = function name
;; funclist = lambda functions for n, n-1, n-2 etc.
;; resultfunc = global lambda, applied after sum is computed
(defun make-diff-func (vname fname funclist 
                             &optional (resultfunc '(lambda (x) x)))
  (let ((funcsum      (append (list '+) 
			      (list (list (first funclist) vname))
			      (loop for y from 1 to (- (length funclist) 1) collect
				    (list (nth y funclist) `(,fname (- ,vname ,y)))))))
    `(t (,resultfunc ,funcsum))))

;; MAKE-DIFF -- combining inits & functions into a diff equation
(defun make-diff (vname fname inits funclist 
                                 &optional (resultfunc '(lambda (x) x)))
  (append (list 'cond) (make-diff-inits vname inits)
          (list (make-diff-func vname fname funclist resultfunc))))

;; DEFINE-DIFF -- main function
;; kudos (again) to Bob Coyne!
(defmacro define-diff (fname inits funclist
                             &optional (resultfunc '(lambda (x) x)))
  (let ((vname 'n))
  `(defun ,fname (,vname)
    ,(make-diff vname fname inits funclist resultfunc))))

;; DIFF-VALS -- returns diffs from start to end
(defun diff-vals (diffunc start end)
  (loop for i from start to end collect (funcall diffunc i)))


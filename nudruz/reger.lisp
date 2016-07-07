;; REGER.LISP
;; Reger model (after Harrison 1986 diss.)

;; useful as |x|>3 tonnetz-like transformations

; (load "nudruz.lisp")

(load "a-star.lisp")

;; RELPOS -- member in a relative position to another member
(defun relpos (chd mbr-idx relplace)
  (nth (mod (+ mbr-idx relplace) (length chd))
       chd)) 

;; July 2006: order-independent version (tn-type) used as default
;; non-nil 'ordflag' will generate different results

;; RGR-ASSOC -- result of 'replace' operation
;; output in normal form
(defun rgr-assoc (mbr-idx inchd &optional (ordflag nil))
  (let ((chd (if ordflag inchd (normal-form inchd))))
    (normal-form
     (loop for n to (- (length chd) 1) collect
           (if (eql n mbr-idx)
               (mod12 (+ 
                       (relpos chd mbr-idx -1)
                       (mod12 (- (relpos chd mbr-idx 1)
                                 (nth mbr-idx chd)))))
               (nth n chd))))))

;; RGR-ASSOC-ALL -- all rgr-assoc for a chord
(defun rgr-assoc-all (chd &optional (ordflag nil))
  (remove-duplicates
   (no-nils
    (mapcar (lambda (x) (if (eql (length x) 
				 (length (remove-duplicates chd)))
			    x))
	    (loop for n to (- (length chd) 1) collect
		  (rgr-assoc n chd ordflag))))
   :test #'list-eql))

;; RGR-RESET -- result of 'refit' operation
;; output in normal form
(defun rgr-reset (mbr-idx inchd &optional (ordflag nil))
  (let ((chd (if ordflag inchd (normal-form inchd))))
    (normal-form
     (loop for n to (- (length chd) 1) collect
           (if (eql n mbr-idx)
               (mod12 (+ 
                       (relpos chd mbr-idx -1)
                       (mod12 (- (relpos chd mbr-idx 2)
                                 (relpos chd mbr-idx 1)))))
               (nth n chd))))))

;; RGR-RESET-ALL -- all rgr-reset for a chord
(defun rgr-reset-all (chd &optional (ordflag nil))
  (remove-duplicates
   (no-nils
    (mapcar (lambda (x) (if (eql (length x) 
				 (length (remove-duplicates chd)))
			    x))
	    (loop for n to (- (length chd) 1) collect
		  (rgr-reset n chd ordflag))))
   :test #'list-eql))

;; RGR-PROSET -- result of 'project' operation
;; output in normal form
(defun rgr-proset (mbr-idx inchd &optional (ordflag nil))
  (let ((chd (if ordflag inchd (normal-form inchd))))
    (normal-form
     (loop for n to (- (length chd) 1) collect
           (if (eql n mbr-idx)
               (mod12 (+ 
                       (relpos chd mbr-idx -1)
                       (mod12 (- (relpos chd mbr-idx -1)
                                 (relpos chd mbr-idx -2)))))
               (nth n chd))))))

;; RGR-PROSET-ALL -- all rgr-proset for a chord
(defun rgr-proset-all (chd &optional (ordflag nil))
  (remove-duplicates
   (no-nils
    (mapcar (lambda (x) (if (eql (length x) 
				 (length (remove-duplicates chd)))
			    x))
	    (loop for n to (- (length chd) 1) collect
		  (rgr-proset n chd ordflag))))
   :test #'list-eql))

;; RGR-FMT -- sorted mod12 version of a chord
(defun rgr-fmt (chd)
  (safesort (mod12 chd)))


;; GEN-RGR apply a reger transform to/from any vector
;; always picks a vector that changes 
(defun gen-rgr (transform a-chd)
  (let* ((rfmt (rgr-fmt a-chd))
         (tformlist 
          (set-difference
           (funcall transform rfmt)
           (list rfmt)
           :test #'list-eql)))
    (matchreg 
     (closest-mod a-chd (pickl tformlist))
     a-chd)))

;;; RGRCHAIN -- successive transformations from list 
(defun rgrchain (a-chd rgrs)
  (cons a-chd
        (when rgrs
          (rgrchain 
           (gen-rgr (first rgrs) a-chd) (cdr rgrs)))))

;; RGRTRANSLIST --- list of randomly-generated transformations
(defun rgrtranslist (len)
  (loop repeat len collect
	(pickl '(rgr-assoc-all 
		 rgr-reset-all 
		 rgr-proset-all))))

;; RGRANDCHAIN -- sequence of random transformations
(defun rgrandchain (a-chd len)
  (rgrchain a-chd (rgrtranslist (- len 1))))

;; RGR-ALLDIM1 -- all 1-dimensional reger transformations
(defun rgr-alldim1 (chd)
  (remove-duplicates
   (append
    (rgr-assoc-all chd)
    (rgr-reset-all chd)
    (rgr-proset-all chd))
   :test #'seq-eql))

;; NFORM-RGR-PATH -- path from normform1 to normform2 (omits target chd)
;; returns normform1 if no applicable path
(defun nform-rgr-path (nform1 nform2)
  (let ((answer
         (a* nform1 nform2
             :Goal-Function #'seq-eql 
             :Child-Generator-Function #'rgr-alldim1
             :H-Function #'listdist)))
    (if (eql (length answer) (length (flatten answer)))
        (list answer)
        (butlast answer))))

;; RGR-PATH -- path from any chd to another (same length)
;; preserves registers
;; omits target chd
;; 'tendreg' is slow! default is 'matchreg' [tendreg optional..]
(defun rgr-path (chd1 chd2 &optional (tendflag nil))
  (let* ((n1 (normal-form chd1))
	 (n2 (normal-form chd2))
	 (npath (nform-rgr-path n1 n2))
	 (rawpath     (append
		       (cons chd1 (cdr npath))
		       (list chd2))))
    (if tendflag 
	(butlast (tendreg rawpath))
	(matchreg-chds rawpath))))

;; RGR-BRANCH -- applies rgr-path to each pair in a list of chds
;; (includes final chord)
;; can be list of chords or list of paths
(defun rgr-branch (chds &optional (flatflag nil) (tendflag nil))
  (let ((rawlist (append
		  (mapcar
		   (lambda (x y) (rgr-path x y tendflag))
		   chds (cdr chds)) 
		  (list (last chds)))))
    (if flatflag (apply #'append rawlist) rawlist)))


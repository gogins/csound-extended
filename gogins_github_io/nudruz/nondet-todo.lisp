(in-package :cm)

;; NONDET-TODO.LISP
;; nondeterministic forays for Common Music
;; needs screamer installed

;; Drew Krause, 2004
;; drkrause@mindspring.com
;; www.wordecho.org

;; load screamer etc.

;; to develop generically
;; (load "nudruz.lisp")
;;(use-package :screamer)

;; to develop within screamer environment
; MKG: Removed. (load "cminit.lisp")
; MKG: Removed. (in-package :screamer-user)

;; revert with "(in-package :cm)"

;; NOTE - entries in each list should not be equal


;; CONS-PAIRS-ND -- checks for melodies that pass stacked canon test
(defun cons-pairs-nd (vec-a vec-b &optional (consvec '(3 4 7 8 9 12)))
  (eval
   (cons 'andv
         (loop for n from 1 to (- vcs 1) collect
               (funcallv #'cantest invec tlevel n consvec)))))

;; SCANONS-ND -- finds all permissible melodies for stacked canons
;; mlen = length of melody
;; tlevel = canon interval
;; vcs = total number of voices in the canon
;; consvec = mod12 consonances
(defun scanons-nd (mlen tlevel vcs &optional (consvec '(3 4 7 8 9 12)))
  (all-values
   (let ((cvec (n-of-list mlen '(0 1 2 3 4 5 6 7 8 9 10 11))))
     (assert! (scan-constraint-nd cvec tlevel vcs consvec))
     (solution cvec 
               (reorder #'domain-size
                        #'(lambda (x) (declare (ignore x)) nil)
                        #'<
                        #'linear-force)))))


;; EMBELL-BACK -- takes a mono or poly list
;; returns triads in (x neighbor x) pattern
(defun embell-back (a-list span)
  (let* ((triadlist (make-poly (flatten a-list) '(3))))
    (loop for x in triadlist append
          (list x (pickl (embellchd x span)) x))))

;; EMBELL-ALL -- takes a mono or poly list
;; returns all neighbors following each triad
(defun embell-all (a-list span)
  (let* ((triadlist (make-poly (flatten a-list) '(3))))
    (loop for x in triadlist append
          (append (list x) (embellchd x span)))))

;; WIGGLE-UTIL: utility for wiggle-to-gen
(defun wiggle-util (wigpair)
   (flatten
    (loop for x from 0 to (- (length (first wigpair)) 1)
          collect (loop repeat (nth x (second wigpair)) 
                    collect (nth x (first wigpair))))))

;; WRAP-WIG: 'wiggle-to-gen' wrapper for wigline
;; example: (wrap-wig 40 43 20 '(4 5 -1)) = ((40 39 44) 3)
(defun wrap-wig (startnum endnum maxlen allowed-ints)
  (let ((wigwrap (wiggle-to startnum endnum maxlen allowed-ints)))
    (if wigwrap 
      (list 
       (butlast 
        (melint->line startnum 
                      (shuffle (nth (random (length wigwrap)) wigwrap))))
       (length wigwrap))
      (list startnum maxlen))))

;; stuff for midchord

(defun pairwise (list1 list2)
  (let ((n (length list1)))
    (if (= n 0) '()
        (cons (list (car list1) (car list2)) (pairwise (cdr list1) (cdr list2))))))

(defun safesort2 (a-list)
  (let ((templist (copy-list a-list)))
    (sort templist #'<)))

;; N-BETWEEN -- utility for midchord
; (n-between '((10 20) (30 35)) = ((screamer::an-integer-between 10 20) (screamer::an-integer-between 30 35))

(defun n-between (listpairs)
  (let ((n (length (first listpairs))))
    (if (= n 0) '()
         (cons (screamer::an-integer-between (first (car listpairs))
                                   (second (car listpairs)))
               (n-between (- n 1) listpairs)))))

(all-btwn 10 5)

(defun midchord (startc endc)
  (all-values
   (let* ((startchd (safesort2 startc))
          (endchd (safesort2 endc))
          (chdlen (length startchd))
          (multvars (n-between (pairwise startchd endchd))))
     (unless (and (not (list-eql multvars startc))
                  (not (list-eql multvars endc))) (fail))
     multvars)))

(n-between '((3 4 5) (2 6 7)))

(n-between '(

(defun midtriad (startc endc)
  (all-values
   (let* ((starts (safesort2 startc))
          (ends (safesort endc))
          (a (screamer::an-integer-between (first starts) (first ends)))
          (b (screamer::an-integer-between (second starts) (second ends)))
          (c (screamer::an-integer-between (third starts) (third ends))))
     (assert! (not (or (list-eql (a b c) starts)
                      (list-eql (a b c) ends))))
     (list a b c))))

(midtriad '(3 4 5) '(2 6 7))


;; INTERCHD!!?? -- any number of midchd steps between chords
(defun interchd (start-chord end-chord steps)
  (let ((new-chord (midchord start-chord end-chord)))
    (if (<= steps 1)
      (list new-chord)
      (cons new-chord (interchd new-chord end-chord (1- steps))))))

(interchd '(10 20 6) '(1 13 18) 4)


(load "tonnetz.lisp")
(defun starter-tz (a-chd) 
        (if (= (inverse-idx a-chd) 1)
          (either pm lm rm) (either ipm ilm irm)))

(defun next-tz (a-tz)
        (cond (pm (either ilm irm))
              (lm (either ipm irm))
              (rm (either ipm ilm))
              (ipm (either lm rm))
              (ilm (either pm rm))
              (irm (either pm lm))))

(defun make-a-tz-chain (a-chd chainlen)
  (if (= chainlen 1)
    (list (starter-tz a-chd))
    (append (make-a-tz-chain a-chd (- chainlen 1))
            (list (next tz (last (make-a-tz-chain a-chd (- chainlen 1))))))))

(defun tz-to (a-chd targetpit &optional (maxlen 10))
  (one-value
   (let* ((len (screamer::an-integer-between 0 maxlen))
         (tzgen (tzchain a-chd (make-a-tz-chain a-chd len)))
          (lastz (last tzgen)))
     (unless (member targetpit lastz)
       (fail))
     tzgen)
   a-chd))

; trial: (tz-to '(12 28 31) 13)


(define pcycle (new range from 0 to 30))
(define multithing
  (process repeat 60
        output (multievent 'midi :keynum
			     :keynum 60
			     :time (now)
			     :duration 1)
	wait 1))


;; melody matching

(defun melmatch (mel1 mel2)


(defun n-queensv (n)
 (solution
  (let ((q (make-array n)))
   (dotimes (i n) (setf (aref q i) (an-integer-betweenv 1 n)))
   (dotimes (i n)
    (dotimes (j n)
     (if (> j i)
	 (assert!
	  (notv (funcallv #'attacks? (aref q i) (aref q j) (- j i)))))))
   (coerce q 'list))
  (reorder #'domain-size
	   #'(lambda (x) (declare (ignore x)) nil)
	   #'<
	   #'linear-force)))



;; store them in a midi file
(events multithing "multithing.midi")

#-(or poplog akcl)
(screamer:define-screamer-package :screams (:use :iterate))

#-(or poplog akcl)
(in-package :screams)

#+(or poplog akcl)
(use-package :iterate)

(defun pythagorean-triples (n)
 (all-values
  (let ((a (screamer::an-integer-between 1 n))
        (b (screamer::an-integer-between 1 n))
        (c (screamer::an-integer-between 1 n)))
   (unless (= (+ (* a a) (* b b)) (* c c)) (fail))
   (list a b c))))


(defun attacks? (qi qj distance)
 (or (= qi qj) (= (abs (- qi qj)) distance)))
 
(defun check-queens (queen queens &optional (distance 1))
 (unless (null queens)
  (if (attacks? queen (first queens) distance) (fail))
  (check-queens queen (rest queens) (1+ distance))))
 
(defun n-queens (n &optional queens)
 (if (= (length queens) n)
     queens
     (let ((queen (screamer::an-integer-between 1 n)))
      (check-queens queen queens)
      (n-queens n (cons queen queens)))))

(defun n-queensv (n)
 (solution
  (let ((q (make-array n)))
   (dotimes (i n) (setf (aref q i) (an-integer-betweenv 1 n)))
   (dotimes (i n)
    (dotimes (j n)
     (if (> j i)
	 (assert!
	  (notv (funcallv #'attacks? (aref q i) (aref q j) (- j i)))))))
   (coerce q 'list))
  (reorder #'domain-size
	   #'(lambda (x) (declare (ignore x)) nil)
	   #'<
	   #'linear-force)))

(defun a-subset-of (x)
 (if (null x)
     nil
     (let ((y (a-subset-of (rest x)))) (either (cons (first x) y) y))))

(defun a-partition-of (x)
 (if (null x)
     x
     (let ((y (a-partition-of (rest x))))
      (either (cons (list (first x)) y)
	      (let ((z (a-member-of y)))
	       (cons (cons (first x) z) (remove z y :test #'eq :count 1)))))))

(defstruct (node (:conc-name nil) (:print-function print-node))
  name next-nodes (visited? nil) (visits 0))

(defun print-node (node stream print-level)
 (declare (ignore print-level))
 (princ (name node) stream))

(defun simple-path (u v)
 (if (visited? u) (fail))
 (local (setf (visited? u) t))
 (either (progn (unless (eq u v) (fail)) (list u))
         (cons u (simple-path (a-member-of (next-nodes u)) v))))

(defun k-simple-path (u v k)
 (if (= (visits u) k) (fail))
 ;; This can't be (LOCAL (INCF (VISITS U))) since Lucid screws up here.
 (local (setf (visits u) (1+ (visits u))))
 (either (progn (unless (eq u v) (fail)) (list u))
         (cons u (k-simple-path (a-member-of (next-nodes u)) v k))))

(defun graph ()
 (let ((a (make-node :name 'a))
       (b (make-node :name 'b))
       (c (make-node :name 'c))
       (d (make-node :name 'd))
       (e (make-node :name 'e))
       (f (make-node :name 'f)))
  (setf (next-nodes a) (list b c))
  (setf (next-nodes b) (list a d e))
  (setf (next-nodes c) (list a d e))
  (setf (next-nodes d) (list b c f))
  (setf (next-nodes e) (list b c f))
  (setf (next-nodes f) (list d e))
  (list (all-values (simple-path a f))
	(all-values (k-simple-path a f 2)))))

(defstruct (boolean-variable (:conc-name nil)) (value :unassigned) noticers)

(defun notb (x)
 (let ((z (make-boolean-variable)))
  (local (push #'(lambda () (set-value x (not (value z)))) (noticers z))
         (push #'(lambda () (set-value z (not (value x)))) (noticers x)))
  z))

(defun andb (x y)
 (let ((z (make-boolean-variable)))
  (local
   (push #'(lambda ()
            (cond ((value x)
                   (unless (eq (value y) :unassigned) (set-value z (value y)))
                   (unless (eq (value z) :unassigned) (set-value y (value z))))
                  (t (set-value z nil))))
         (noticers x))
   (push #'(lambda ()
            (cond ((value y)
                   (unless (eq (value x) :unassigned) (set-value z (value x)))
                   (unless (eq (value z) :unassigned) (set-value x (value z))))
                  (t (set-value z nil))))
         (noticers y))
   (push #'(lambda ()
            (cond ((value z) (set-value x t) (set-value y t))
                  (t (if (eq (value x) t) (set-value y nil))
                     (if (eq (value y) t) (set-value x nil)))))
         (noticers z))
   z)))

(defun orb (x y) (notb (andb (notb x) (notb y))))

(defun set-value (variable value)
 (cond ((eq (value variable) :unassigned)
        (local (setf (value variable) value))
        (dolist (noticer (noticers variable)) (funcall noticer)))
       (t (unless (eq (value variable) value) (fail)))))

(defun boolean-solution (variables)
 (if (null variables)
     '()
     (let ((variable (first variables)))
      (when (eq (value variable) :unassigned)
       (set-value variable (either t nil)))
      (cons (value variable) (boolean-solution (rest variables))))))

(defun sat-problem ()
 (all-values
  (let ((x (make-boolean-variable))
	(y (make-boolean-variable))
	(z (make-boolean-variable)))
   (set-value (andb (orb x (notb y)) (orb y (notb z))) t)
   (boolean-solution (list x y z)))))


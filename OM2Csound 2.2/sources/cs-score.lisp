;;;===================================================
;;;
;;; OM2Csound
;;; Control of Csound sound synthesis from OpenMusic
;;;
;;; see www.csounds.com
;;;
;;; CSOUND SCORE
;;; Authors/Contributors: Laurent Pottier, Karim Haddad, Jean Bresson 
;;; (c) IRCAM 1993-2010
;;;===================================================


(in-package :om)


;=========================================
; TABLES
;=========================================
(om::defmethod! table ((table number) 
                       (time number) 
                       (size number) 
                       (gen  number) 
                       (pargen list))
  :icon 141
  :indoc '("table ID" "time (sec)" "size" "GEN number" "values")
  :initvals (list 1 0 4096 10 (list 1))
  :doc "Csound ftable declaration.

   <table>  table number or ID
   <time>   moment, in seconds, for triggering off the table
   <size>   number of points in the table
   <gen>    number of the GEN subroutine
   <pargen> list containing auxiliary parameters or values for the GEN subroutine (can be connected to PARGEN function).
"
  
  (list (append
         (list 'f table time size gen)
         pargen)))


(om::defmethod! paramXY ((Xlist  list) (Ylist  list))
                :icon 140
                :indoc '("Xlist" "Ylist")
                :initvals (list (list 0 5) (list 1 2))
                :doc "Formats two lists (<xlist> and <ylist>) as table parameters."
                (let ((lstxy (list Xlist Ylist ))(auxlist) (x) (y))
                  (setf x (om::x->dx (first lstxy)))
                  (setf y (second lstxy))
                  (dotimes (z (length x))
                    (push (nth z y) auxlist)
                    (push (nth z x) auxlist))
                  (push (nth (- (length y) 1) y) auxlist)
                  (reverse auxlist)))

(om::defmethod! pargen03 ((xmin number) (xmax number) &rest coeffs)
  :icon 140
  :indoc '("min x value" "max x value" "list of coefficients")
  :initvals (list 1 1 1)
  :doc "Formats table data for a GEN03.

GEN03 is a polynomial evaluated in x over a fixed interval and with specified coefficients.
        
       <xmin> and <xmax> are the left and right values of the x interval over which the polynomial is defined.
       <coeffs> are the coefficients of the polynomial C0 + C1*x + C2*x^2 + . . . + Cn*x^n
"

(om::flat (list xmin xmax coeffs)))

(defmethod! om-scale/max ((list list) (max number))
  :doc "scales <list> (may be tree) so that its max becomes <max>. Trees must be well-formed: The children of a node must be either all leaves or all nonleaves. " 
  (om::less-tree-mapcar #'(lambda (x y) (om::om* x (/ y (list-max x)))) list max t))


; compat 
(om::defmethod! pargen57 (bpf pnts y-min y-max ndec) (pargen05-07 bpf pnts y-min y-max ndec))

(om::defmethod! pargen05-07 ((bpf bpf) (pnts number) (y-min number) (y-max number) (ndec number))
  :icon 140
  :indoc '("a BPF object" "number of points" "min y value" "max y value" "number of decimals")
  :initvals (list (make-instance 'bpf) 2048 0 1 3)
  :doc "Formats table data for a GEN05 or a GEN07.

GEN05 constructs functions from segments of exponential curves. 
GEN07 constructs functions from segments of straight lines. 
       
        <bpf>      defines the function segments
        <pnts>     number of points in the table
        <y-min>    minimum y value for scaling (for GEN05 must not be zero, and must be alike in sign. No restriction for GEN09). 
        <y-max>    maximum y value for scaling
        <ndec>     number of decimals (i.e. precision) in the formed parameters.
"
   
  (let* ((Ly (copy-list (om::x-points bpf)))
         (Lx (copy-list (om::y-points bpf))))
    (paramxy (om::om-round (om-scale/max Ly pnts))
             (om::om-round (om-scale Lx y-min y-max) ndec))))



(om::defmethod! pargen09 ((pn t) (str t) (phs t) (Npart number))
  :icon 140
  :indoc '( "partial number(s)" "strength or amplitude(s)" "phase(s)" "number of partials")
  :initvals (list 1 1 0 1)
  :doc "Formats table data for a GEN09.
      
GEN09 generates a composite waveform made up of weighted sums of simple sinusoids (partials).

      <pn>    Partials number relative to a fundamental. 
              Must be positive, but need not necessarily a whole number (non-harmonic partials are permitted).
              Partials may be in any order.
      <str>   Strength of the partials.
      <phs>   Initial phase of partials.
      <Npart> number of partials demanded.

All parameters (except <npart>) can be simple numbers or lists.
"
(let ((res))
  (dotimes (n Npart)
    (push (if (atom pn) pn (nth n pn)) res)
    (push (if (atom str) str (nth n str)) res)
    (push (if (atom phs) phs (nth n phs)) res))
  (reverse res)))


(om::defmethod!  pargen15 ((xint number) (xamp number)  (Ho list) (Phs list))
  :icon 140
  :indoc '("xint" "xamp" "Ho" "Phs")
  :initvals (list 1 1 '(1 2) '(1 2))
  :doc "Formats table data for a GEN15.
  
GEN15 creates two tables of stored polynomial functions, suitable for use in phase quadrature operations
      
       <xint>  left and right values [-xint, +xint] of the x interval over which the polynomial is to be drawn.
       <xamp>  amplitude scaling factor of the sinusoid input that is expected to produce the following spectrum.
       <ho>    list of relative intensities of the harmonics
       <phs>   list of phases in degrees.
"
(om::flat (append (list xint xamp)
                  (om::mat-trans (list Ho Phs)))))


;=========================================
; SCORE STATEMENTS
;=========================================

(om::defmethod! i-statements (instr onsets durs &rest params)
  :icon 131
  :indoc '("instrument number" "onset(s) [sec.]" "duration(s) [sec.]" "other synthesis parameters")
  :initvals '(1 (0) (1) nil)
  :doc 	"Formats i statements in a Csound score.

   <instr>    number of the instrument (must be defined in the Csound orchestra)
   <onsets>    Attack time(s) 
   <durs>     Duration of the event(s) 
   <params>  other Csound parameters (p4, p5, ...)

All arguments can be numbers or lists (with the same number of elements).

 
"
  (loop for onset in (list! onsets) 
          for n = 0 then (+ n 1) collect 
          (append 
           (list 'i (if (atom instr) instr (nth n instr))
                 onset (if (atom durs) durs (nth n durs)))
           (loop for par in params append 
                 (if (atom par) (list par) 
                   (if (atom (nth n par))
                       (list (nth n par))
                     (loop for ppp in par collect (nth n ppp))))) 
                   )))


(om::defmethod! instrument0 (instr onsets durs &rest params)
  :icon 131
  :doc 	"DEPRECATED see i-STATEMENTS"
  (apply 'i-statements (append (list instr onsets durs) params)))

(om::defmethod! instrument1 (instr onsets durs p4 p5 others)
  :icon 131
  :doc 	"DEPRECATED see i-STATEMENTS"
  (apply 'i-statements (append (list instr onsets durs p4 p5) others)))


(defun c-c (Model Data)
  "Reproduit la structure de Model avec les donnees Data."
  (if (atom Data) (setf Data (list Data)))
  (if (null Model) nil
    (if (atom  Model)
        (car (flat Data))
      (cons (c-c (car Model) (car Data))
            (c-c (cdr Model)
                 (if (cdr Data) (cdr Data)
                   (list (car Data))))))))


;; One arguments (by default, p5, the 2nd element in <params>) can define the global structure and number of elements.
(om::defmethod! make-obj-snd (Lins Ldats Ldurs Lp4 Lp5 &rest Lpn)
  :icon 131
  :doc 	"Formats i statements in a Csound score using OM data.

   <Lins>    number of the instrument (must be defined in the Csound orchestra)
   <Ldats>    Attack time(s) 
   <Ldurs>     Duration of the event(s) 
   <Lp4>   other Csound parameter (p4, e.g. amplitude)
   <Lp5>   other Csound parameters (p5, e.g. frequency): THIS PARAMETER DEFINES THE STRUCTURE OF THE SCORE
   <Lpn>  additional parameter(s)

All arguments can be numbers or lists (with the same number of elements).
<Lp5> can be a list of lists as for OM CHORD_SEQ parameters format. In thsi case, the other parameter's lists are adapted, if needed, to fit with this pattern.
"

    (let* ((model Lp5)
           (oo (if model (flat (c-c model Ldats)) Ldats))
           (dd (if model (flat (c-c model Ldurs)) Ldurs))
           (ii (if model (flat (c-c model Lins)) Lins))
           (pp (remove nil 
                     (append (list (flat (c-c model Lp4))
                                   (flat model))
                             (loop for item in Lpn collect (flat (c-c model item)))))))
      (apply 'i-statements (append (list ii oo dd) pp))))
              


;=========================================
; WRITE SCO
;=========================================


;Corrected 1/2009 by kh
(defun print-seq-aux (seq dest)
  (cond ((equal 'i seq)
         (format dest "~D " seq #\tab))
        ((stringp seq)
         (format dest "~s " seq #\tab))
        ((integerp seq)
         (format dest "~D~a " seq #\tab))
        ((numberp seq)
         (format dest "~4,4f~a " (float seq) #\tab))
        (t 
         (format dest "~D " seq #\tab))))

(defun print-comment (seq dest coin)
"imprime un commentaire sur la liste seq, vers dest, si coin est nil. Rend t"
  (cond ((not (listp  seq)) nil)
        ((endp seq) nil)
        ((and (equal 'i (car seq)) (null coin))
         (progn (format dest "\;")
           (dotimes (n (1- (length seq)) t)
             (format dest "p~D~a" (1+ n) #\tab))
           (format dest "~%")
           t))
        ((equal 'i (car seq)) t)
        (t nil)))

(defun printscoseq (seq dest)
  (if (atom seq)
      (print-seq-aux seq dest)
    (while seq
      (print-seq-aux (pop seq) dest))))

(defvar *lastcsdfile* nil)

(defun flat-max-1 (l)
  (loop for item in l append
        (if (listp (car item))
            (flat-max-1 item)
          (list item)
          )))

(om::defmethod! write-csound-score (out data &rest more)
  :icon 135
  :indoc '("filename" "score data" "more data")
  :initvals '(file nil nil)
  :menuins '((0 (("text output" 'no-file) ("file output" 'file))))
  :doc "Writes the Csound score data in a file.
        
        <out> can be a pathname or a file name. If the name only is given, the file will be written in the OM 'temp files' folder.
        If <out> is nil, a dialog will allow to choose a file manually.
        If <out> is 'no-file, the function returns the score as a list of Csound code lines.

        <data> is Csound score data coming from table or score-statements (or a list)
        <more> allows to connect other Csound score data.
"
  
  (let ((cs-data (flat-max-1 (remove nil (append (list! data) more))))

        (filename (handle-new-file-exists 
                   (cond ((equal out 'no-file) nil)
                         ((pathnamep out) out)
                         ((stringp out) (tmpfile out :type "sco"))
                         (t (om-choose-new-file-dialog :directory (or *lastcsdfile* *om-tmpfiles-folder*)
                                                       :prompt "Save csound score..." :types '("Csound score Files" "*.sco")))))))
    (if filename
        (progn 
          (format t "Writing CSOUND SCORE : ~s" (namestring filename))
          (with-open-file  (fd filename :direction :output :if-does-not-exist :create)       
            (let ((glou nil))
              (loop for item in cs-data do
                    (setf glou (print-comment item fd glou))
                    (printscoseq item fd)
                    (format fd "~%"))
              (format fd "e")
              ))
          (probe-file filename))
      (let ((rep nil)
             (glou nil))
            (loop for item in cs-data do
                  (with-open-stream (s (make-string-output-stream))
                    (setf glou (print-comment item s glou))
                    (printscoseq item s)
                    (push (get-output-stream-string s) rep)))
            (push "e" rep)
            (reverse rep)
      )
    )))


(om::defmethod! editsco ((data t)  &rest lst?)
  :icon 135
  :doc "DEPRECATED: use WRITE-SCOUND-SCORE"
  (write-csound-score nil (append (list! data) lst?)))
  
(om::defmethod! auto-editsco ((data t) &optional (name "my-score"))
  :icon 135
  :doc "DEPRECATED: use WRITE-SCOUND-SCORE"
  (write-csound-score name data))








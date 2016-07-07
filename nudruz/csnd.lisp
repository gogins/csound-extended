;; CSND.LISP -- writing to csound
;; todo: going from midi to csound

;; need these
(defobject csnd-inst (i)
  (amp freq opt1 opt2 opt3)
  (:parameters time dur amp freq opt1 opt2 opt3))

(defobject csnd-inst2 (i)
  (amp freq)
  (:parameters time dur amp freq))

(define sco-header
        "f 1  0 16384 10    1")


;; CSPLAY -- writes sco files
;; functions for pits, durs, amp, i-nbr, & 3 optional parameters
;; 'inpits','indurs' like 'splay'
;; optional params could be patt, val, or list
;; OR could be quoted function(s) of:
;; "p" = [cpits], "d" = [cdurs], "a" = [catx]
;; note: above variable names apply to the entire 'p', 'd', & 'a' lists 
;; invoices=nil means separate channels assigned by poly texture
(defun csplay (inpits indurs 
		       &optional (invel .35) (invoices 1) 
		       (inopt1 1) (inopt2 2) (inopt3 3))
  (let* ((pl (plists inpits indurs))
	 (pits (first pl))
	 (durs (second pl))
	 (atx (pitsdurs->atx pits durs))
	 (realpits (norests inpits))
	 (realdurs (chooser
		    (filterpos (lambda (x) (not (eql x 'r))) 
			       inpits)
		    durs))
	 (cpits (flatten realpits))
	 (cdurs (repeater realdurs (take-poly realpits)))
	 (catx (repeater atx (take-poly realpits)))
	 (cvel
	  (if (and (listp invel)
		   (intersection '(p d a) (flatten invel)))
	      (eval `(map 'list 
		      (lambda (p d a) ,invel) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits invel))
	       (take-poly realpits))))
	 (cvoices
	  (if invoices
	      (if (and (listp invoices)
		       (intersection '(p d a) (flatten invoices)))
		  (eval `(map 'list 
			  (lambda (p d a) ,invoices) 
			  (quote ,cpits) 
			  (quote ,cdurs) 
			  (quote ,catx)))
		  (repeater
		   (second (plists realpits invoices))
		   (take-poly realpits)))
	      (transp
	       (flatten
		(mapcar #'indices
			(take-poly realpits)))
	       1)))
	 (copt1
	  (if (and (listp inopt1)
		   (intersection '(p d a) (flatten inopt1)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt1) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt1))
	       (take-poly realpits))))
	 (copt2
	  (if (and (listp inopt2)
		   (intersection '(p d a) (flatten inopt2)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt2) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt2))
	       (take-poly realpits))))
	 (copt3
	  (if (and (listp inopt3)
		   (intersection '(p d a) (flatten inopt3)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt1) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt3))
	       (take-poly realpits)))))
    (process for x in cpits
	     for dur in cdurs
	     for atk in catx
	     for vel in cvel
	     for voicenbr in cvoices
	     for valopt1 in copt1
	     for valopt2 in copt2
	     for valopt3 in copt3
	     output (new csnd-inst
		      :ins voicenbr
		      :opt1 valopt1
		      :opt2 valopt2
		      :opt3 valopt3
		      :freq (hertz x)
		      :time (float atk)
	 	      :amp (float vel)
		      :dur (float dur)))))

;; CSPLAY-F -- writes sco files
;; like 'csplay' but writes freqs instead of keynums
;; functions for pits, durs, amp, i-nbr, & 3 optional parameters
;; 'inpits','indurs' like 'splay'
;; optional params could be patt, val, or list
;; OR could be quoted function(s) of:
;; "p" = [cpits], "d" = [cdurs], "a" = [catx]
;; note: above variable names apply to the entire 'p', 'd', & 'a' lists 
;; invoices=nil means separate channels assigned by poly texture
(defun csplay-f (inpits indurs 
		       &optional (invel .35) (invoices 1) 
		       (inopt1 1) (inopt2 2) (inopt3 3))
  (let* ((pl (plists inpits indurs))
	 (pits (first pl))
	 (durs (second pl))
	 (atx (pitsdurs->atx pits durs))
	 (realpits (norests inpits))
	 (realdurs (chooser
		    (filterpos (lambda (x) (not (eql x 'r))) 
			       inpits)
		    durs))
	 (cpits (flatten realpits))
	 (cdurs (repeater realdurs (take-poly realpits)))
	 (catx (repeater atx (take-poly realpits)))
	 (cvel
	  (if (and (listp invel)
		   (intersection '(p d a) (flatten invel)))
	      (eval `(map 'list 
		      (lambda (p d a) ,invel) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits invel))
	       (take-poly realpits))))
	 (cvoices
	  (if invoices
	      (if (and (listp invoices)
		       (intersection '(p d a) (flatten invoices)))
		  (eval `(map 'list 
			  (lambda (p d a) ,invoices) 
			  (quote ,cpits) 
			  (quote ,cdurs) 
			  (quote ,catx)))
		  (repeater
		   (second (plists realpits invoices))
		   (take-poly realpits)))
	      (transp
	       (flatten
		(mapcar #'indices
			(take-poly realpits)))
	       1)))
	 (copt1
	  (if (and (listp inopt1)
		   (intersection '(p d a) (flatten inopt1)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt1) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt1))
	       (take-poly realpits))))
	 (copt2
	  (if (and (listp inopt2)
		   (intersection '(p d a) (flatten inopt2)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt2) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt2))
	       (take-poly realpits))))
	 (copt3
	  (if (and (listp inopt3)
		   (intersection '(p d a) (flatten inopt3)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt1) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt3))
	       (take-poly realpits)))))
    (process for x in cpits
	     for dur in cdurs
	     for atk in catx
	     for vel in cvel
	     for voicenbr in cvoices
	     for valopt1 in copt1
	     for valopt2 in copt2
	     for valopt3 in copt3
	     output (new csnd-inst
		      :ins voicenbr
		      :opt1 valopt1
		      :opt2 valopt2
		      :opt3 valopt3
		      :freq x
		      :time (float atk)
	 	      :amp (float vel)
		      :dur (float dur)))))


; SEEMS TO WORK OK
;(events
; (csplay
;  (heapvec 20 24 60)
;  (transp (heapvec 50 5 1) .125 #'*)
;  '(transp p 2)
;  '(mod12 p)
;  '(* d .2)
;  5
;  10)
; "out.sco" :header sco-header)

;; TCSPLAY -- takes pits, durs, & atx
;; writes sco files
;; functions for pits, durs, amp, i-nbr, & 3 optional parameters
;; 'inpits','indurs' like 'splay'
;; optional params could be patt, val, or list
;; OR could be quoted function(s) of:
;; "p" = [cpits], "d" = [cdurs], "a" = [catx]
;; note: above variable names apply to the entire 'p', 'd', & 'a' lists 
(defun tcsplay (inpits indurs atx
		       &optional (invel .35) (invoices 1) (inopt1 1) (inopt2 2) (inopt3 3))
  (let* ((pl (plists inpits indurs))
	 (pits (first pl))
	 (durs (second pl))
	 (realpits (norests inpits))
	 (realdurs (chooser
		    (filterpos (lambda (x) (not (eql x 'r))) 
			       inpits)
		    durs))
	 (cpits (flatten realpits))
	 (cdurs (repeater realdurs (take-poly realpits)))
	 (catx (repeater atx (take-poly realpits)))
	 (cvel
	  (if (and (listp invel)
		   (intersection '(p d a) (flatten invel)))
	      (eval `(map 'list 
		      (lambda (p d a) ,invel) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits invel))
	       (take-poly realpits))))
	 (cvoices
	  (if (and (listp invoices)
		   (intersection '(p d a) (flatten invoices)))
	      (eval `(map 'list 
		      (lambda (p d a) ,invoices) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits invoices))
	       (take-poly realpits))))
	 (copt1
	  (if (and (listp inopt1)
		   (intersection '(p d a) (flatten inopt1)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt1) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt1))
	       (take-poly realpits))))
	 (copt2
	  (if (and (listp inopt2)
		   (intersection '(p d a) (flatten inopt2)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt2) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt2))
	       (take-poly realpits))))
	 (copt3
	  (if (and (listp inopt3)
		   (intersection '(p d a) (flatten inopt3)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt1) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt3))
	       (take-poly realpits)))))
    (process for x in cpits
	     for dur in cdurs
	     for atk in catx
	     for vel in cvel
	     for voicenbr in cvoices
	     for valopt1 in copt1
	     for valopt2 in copt2
	     for valopt3 in copt3
	     output (new csnd-inst
		      :ins voicenbr
		      :opt1 valopt1
		      :opt2 valopt2
		      :opt3 valopt3
		      :freq (hertz x)
		      :time (float atk)
	 	      :amp (float vel)
		      :dur (float dur)))))

;; TCSPLAY-F -- takes freqs, durs, & atx
;; writes sco files
;; functions for pits, durs, amp, i-nbr, & 3 optional parameters
;; 'inpits','indurs' like 'splay'
;; optional params could be patt, val, or list
;; OR could be quoted function(s) of:
;; "p" = [cpits], "d" = [cdurs], "a" = [catx]
;; note: above variable names apply to the entire 'p', 'd', & 'a' lists 
(defun tcsplay-f (inpits indurs atx
		       &optional (invel .35) (invoices 1) (inopt1 1) (inopt2 2) (inopt3 3))
  (let* ((pl (plists inpits indurs))
	 (pits (first pl))
	 (durs (second pl))
	 (realpits (norests inpits))
	 (realdurs (chooser
		    (filterpos (lambda (x) (not (eql x 'r))) 
			       inpits)
		    durs))
	 (cpits (flatten realpits))
	 (cdurs (repeater realdurs (take-poly realpits)))
	 (catx (repeater atx (take-poly realpits)))
	 (cvel
	  (if (and (listp invel)
		   (intersection '(p d a) (flatten invel)))
	      (eval `(map 'list 
		      (lambda (p d a) ,invel) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits invel))
	       (take-poly realpits))))
	 (cvoices
	  (if (and (listp invoices)
		   (intersection '(p d a) (flatten invoices)))
	      (eval `(map 'list 
		      (lambda (p d a) ,invoices) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits invoices))
	       (take-poly realpits))))
	 (copt1
	  (if (and (listp inopt1)
		   (intersection '(p d a) (flatten inopt1)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt1) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt1))
	       (take-poly realpits))))
	 (copt2
	  (if (and (listp inopt2)
		   (intersection '(p d a) (flatten inopt2)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt2) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt2))
	       (take-poly realpits))))
	 (copt3
	  (if (and (listp inopt3)
		   (intersection '(p d a) (flatten inopt3)))
	      (eval `(map 'list 
		      (lambda (p d a) ,inopt1) 
		      (quote ,cpits) 
		      (quote ,cdurs) 
		      (quote ,catx)))
	      (repeater
	       (second (plists realpits inopt3))
	       (take-poly realpits)))))
    (process for x in cpits
	     for dur in cdurs
	     for atk in catx
	     for vel in cvel
	     for voicenbr in cvoices
	     for valopt1 in copt1
	     for valopt2 in copt2
	     for valopt3 in copt3
	     output (new csnd-inst
		      :ins voicenbr
		      :opt1 valopt1
		      :opt2 valopt2
		      :opt3 valopt3
		      :freq x
		      :time (float atk)
	 	      :amp (float vel)
		      :dur (float dur)))))

; FRQAMP-CSPLAY -- plays chds incl freq/amp pairs (for fm-spectrum etc)
(defun frqamp-csplay (freqamps durs)
  (let* ((inlists (transpose-matx (mapcar #'transpose-matx freqamps)))
	 (polyfrqs (first inlists))
	 (polyamps (second inlists))
	 (atx
	  (pitsdurs->atx polyfrqs durs))
	 (texture (take-poly polyfrqs))
	 (alldurs (second (plists polyfrqs durs))))
    (tcsplay-f (flatten polyfrqs) 
	       (repeater alldurs texture)
	       (repeater atx texture)
	       (flatten polyamps))))

; FRQAMP-TCSPLAY -- plays material incl 'flat' freq/amp pairs (for fm-spectrum etc)
(defun frqamp-tcsplay (freqamps durs atx)
  (let* ((inlists (transpose-matx freqamps))
	 (polyfrqs (first inlists))
	 (polyamps (second inlists))
	 (alldurs (second (plists polyfrqs durs))))
    (tcsplay-f polyfrqs
	       alldurs
	       atx
	       polyamps)))

;; CSPLAYM -- writes multi-inst sco files w/pits,durs,vels
;; 'inpits','indurs' like 'splay'
;; vel is ranvec of range [.33-.37] by default
(defun csplaym (multifile)
  (process for evnt in
	   (sort
	    (loop for n to (- (length multifile) 1) append
		  (let* ((thiseq (nth n multifile))
			 (inpits (first thiseq))
			 (indurs (second thiseq))
			 (invel (if (third thiseq) 
				    (third thiseq) 
				    (ranvec (length inpits) .32 .37)))
			 (pl (plists inpits indurs))
			 (pits (first pl))
			 (durs (second pl))
			 (atx (pitsdurs->atx pits durs))
			 (realpits (norests inpits))
			 (realdurs (chooser
				    (filterpos (lambda (x) (not (eql x 'r))) 
					       inpits)
				    durs))
			 (cpits (flatten realpits))
			 (cdurs (repeater realdurs (take-poly realpits)))
			 (catx (repeater atx (take-poly realpits)))
			 (cvel
			  (repeater
			   (second (plists realpits invel))
			   (take-poly realpits))))
		    (transpose-matx (list (copylist (list (+ n 1)) (length catx))
					  catx cdurs cvel cpits))))
	    (lambda (a b) (< (cadr a) (cadr b))))
	   output (new csnd-inst2
		    :ins (car evnt)
		    :freq (hertz (nth 4 evnt))
		    :time (float (nth 1 evnt))
		    :amp (float (nth 3 evnt))
		    :dur (float (nth 2 evnt)))))

;; CM2SCO -- designed to write sco's from multichan midi's
(defun cm2sco (multifile)
  (process for evnt in
	   (sort
	    (loop for n to (- (length multifile) 1) append
		  (let* ((thiseq (nth n multifile))
			 (inpits (first thiseq))
			 (indurs (second thiseq))
			 (atx (third thiseq))
			 (invel (if (fourth thiseq) 
				    (fourth thiseq) 
				    (ranvec (length inpits) .32 .37)))
			 (pl (plists inpits indurs))
			 (pits (first pl))
			 (durs (second pl))
			 (realpits (norests inpits))
			 (realdurs (chooser
				    (filterpos (lambda (x) (not (eql x 'r))) 
					       inpits)
				    durs))
			 (cpits (flatten realpits))
			 (cdurs (repeater realdurs (take-poly realpits)))
			 (catx (repeater atx (take-poly realpits)))
			 (cvel
			  (repeater
			   (second (plists realpits invel))
			   (take-poly realpits))))
		    (transpose-matx (list (copylist (list (+ n 1)) (length catx))
					  catx cdurs cvel cpits))))
	    (lambda (a b) (< (cadr a) (cadr b))))
	   output (new csnd-inst2
		    :ins (car evnt)
		    :freq (hertz (nth 4 evnt))
		    :time (float (nth 1 evnt))
		    :amp (float (nth 3 evnt))
		    :dur (float (nth 2 evnt)))))

;; MIDI2SCO -- wrapper for multichan midi files
(defun midi2sco (mfile)
  (cm2sco (numidi-in1 mfile)))

;; CSPLAYM-PERC -- writes multi-inst sco files for percussion
;; 'pits' varies by ratio 'varyamt'
;; 'inpits','indurs' like 'splay'
;; vel is ranvec of range [.33-.37] by default
(defun csplaym-perc (multifile &optional (varyamt .1))
  (process for evnt in
	   (sort
	    (loop for n to (- (length multifile) 1) append
		  (let* ((thiseq (nth n multifile))
			 (inpits (first thiseq))
			 (indurs (second thiseq))
			 (atx (third thiseq))
			 (invel (if (fourth thiseq) 
				    (fourth thiseq) 
				    (ranvec (length inpits) .32 .37)))
			 (pl (plists inpits indurs))
			 (pits (first pl))
			 (durs (second pl))
;			 (atx (pitsdurs->atx pits durs))
			 (realpits (norests inpits))
			 (realdurs (chooser
				    (filterpos (lambda (x) (not (eql x 'r))) 
					       inpits)
				    durs))
			 (cpits (flatten realpits))
			 (cdurs (repeater realdurs (take-poly realpits)))
			 (catx (repeater atx (take-poly realpits)))
			 (cvel
			  (repeater
			   (second (plists realpits invel))
			   (take-poly realpits))))
		    (transpose-matx (list (copylist (list (+ n 1)) (length catx))
					  catx cdurs cvel cpits))))
	    (lambda (a b) (< (cadr a) (cadr b))))
	   output (new csnd-inst2
		    :ins (car evnt)
		    :freq (vary (hertz (nth 4 evnt)) varyamt)
		    :time (float (nth 1 evnt))
		    :amp (float (nth 3 evnt))
		    :dur (float (nth 2 evnt)))))

;; MIDI2SCO-PERC -- wrapper for multichan perc midi files
(defun midi2sco-perc (mfile &optional (varyamt .1))
  (csplaym-perc (numidi-in1 mfile) varyamt))

;; --> CHECK THIS!! NOT ALWAYS ACCURATE ATX TIMES?
;; MIDI2SCO-DRUM -- wrapper for single-channel drumset midi files
(defun midi2sco-drum (mfile &optional (varyamt .1))
  (csplaym-perc (drumidi-in mfile) varyamt))

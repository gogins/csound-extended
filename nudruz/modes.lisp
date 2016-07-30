;; examples of working with modes

; (note 51 :in oct) = E5
; (defun oct-1 (transpose oct 'cs)) == same mode up a half-step
; (keynum (note 51 :in oct))
; (loop for i from 1 to 7 collect (keynum (note i :in oct)))

(defun mode-octave (modename)
  (slot-value modename 'octave))

(defun mode-size (modename)
  (length (slot-value modename 'steps)))

(defun mode-lowest (modename)
  (keynum (first (slot-value modename 'lowest))))

(defun mode-steps (modename)
  (slot-value modename 'steps))

;; TRANSP-MODE -- make a new mode as a transposition of another one (safest method)
(defun transp-mode (amode tlevel)
 (new mode :degrees (transp (append (mode-steps amode) (list (mode-octave amode))) tlevel)))

;; some basic mod12 modes

(defparameter pentatonic (new mode steps '(2 2 3 2 3))) ; 5 members
(defparameter pelog (new mode steps '(1 2 4 1 4)))   ; 5 members
(defparameter wholetone (new mode steps '(2 2 2 2 2 2))) ; 6 members
(defparameter wt-0 wholetone)
(defparameter wt-1 (transp-mode wholetone 1))
(defparameter mlt5 (new mode steps '(1 4 1 1 4 1))) ; 6 members
(defparameter symmetric6 (new mode steps '(1 3 1 3 1 3))) ; 6 members
(defparameter ionian (new mode steps '(2 2 1 2 2 2 1))) ; 7 members
(defparameter hminor (new mode steps '(2 1 2 2 1 3 1)))
(defparameter octatonic (new mode steps '(1 2 1 2 1 2 1 2))) ; 8 members
(defparameter mlt4 (new mode steps '(1 1 3 1 1 1 3 1))) ; 8 members
(defparameter mlt6 (new mode steps '(2 2 1 1 2 2 1 1))) ; 8 members
(defparameter mlt3 (new mode steps '(2 1 1 2 1 1 2 1 1))) ; 9 members
(defparameter mlt7 (new mode steps '(1 1 1 2 1 1 1 1 2 1))) ; 10 members
(defparameter chromatic (new mode steps '(1 1 1 1 1 1 1 1 1 1 1 1))) ; 12 members
(defparameter goodmode (new mode steps '(1 1 3 1 1 2 3))) ; 7 members
(defparameter chahargah (new mode steps '(1 3 1 2 1 3 1))) ; 7 members

;; some cooler (not mod 12) modes
(defparameter stravmode (new mode steps '(2 1 2))) ; also "Shur"
(defparameter hyperlydian (new mode steps '(2 2 2 1))) 
(defparameter hyperphrygian (new mode steps '(1 2 2))) ; also "Segah"
(defparameter shushtar (new mode steps '(1 2 1 3)))
(defparameter bayati (new mode steps '(2 2 1 2 1))) ; also "Bayati Shiraz"
(defparameter humayun (new mode steps '(1 3 1 2 2)))

;; PLAY-MODE -- a function to convert to keynums
; (play-mode (transp (randmel 20 5) 20) hyperlydian) = 
; (37 39 42 39 37 35 41 35 37 42 35 39 41 42 41 37 39 41 37 35)
(defun play-mode (x modename &optional (basenum 0))
  (cond 
    ((numberp x)
     (keynum (note (+ x basenum) :in modename)))
    ((listp x)
     (loop for y in x collect (play-mode y modename basenum)))
    (t 'r)))

;; MODENUMS -- reverse of 'play-mode': returns # in mode for pit or pits
(defun modenums (input modename)
  (cond
    ((numberp input) (keynum input :to modename))
    ((listp input) (mapcar (lambda (x) (modenums x modename)) input))
    (t 'r)))

;; PLAYMODE - a process
(defun playmode (a-list mode basepit durs)
  (process 
    for i in a-list 
    for thisdur in durs
        output (new midi :time (now)
                     :keynum (note (+ i basepit) :in mode)
                     :duration (float thisdur))
	wait (float thisdur)))

;; MODE-MOTIVE -- places a contour motive into a mode at starting pitch
;; example: (mode-motive 40 '(0 -1 -3) octatonic) = (60 58 55)
(defun mode-motive (startingpit motive mode)
  (loop for i in motive
	collect (keynum (note (+ startingpit i) :in mode))))

;; SOME-MODE -- collects mode members from 'bottom' to 'top' (above basepit)
(defun some-mode (a-mode basepit bottom top)
  (loop for i from bottom to top 
	collect (+ basepit (keynum (note i :in a-mode)))))

;; MODE-CHORD -- converts mode to chord
(defun mode->chord (a-mode)
  (loop for x in 
	(some-mode a-mode 0 0 12)
	if (< x 12) collect x))

;; CW->MODE -- interprets codeword as a set of modal pitches
;; example: (cw->mode '(1 3 0 0 2 1 1 2) ionian 10) = 
;; (22 48 41 31 33 46)

(defun cw->mode (codeword a-mode basepit)
  (loop for i from 0 to (- (length codeword) 1)
	if (not (eq 0 (nth i codeword))) 
	collect  (+ basepit (* 12 (nth i codeword))
		    (keynum (note i :in a-mode)))))

;; CW->CHORD -- interprets codeword as mod-transposed(!) members of a chord
;; example: (cw->chord '(1 0 2 3 1 2) '(1 3 4 6 9 10) 0 12) =
;; (13 28 42 21 34) 

(defun cw->chord (codeword a-chord basepit octavesize)
  (loop for i from 0 to (- (length codeword) 1)
	if (not (eq 0 (nth i codeword))) 
	collect  (+ basepit (* octavesize (nth i codeword))
		    (nth i a-chord))))

;; RANDOM-CHORD -- generates pc-list with 'chd-length' members
(defun random-chord (chd-length)
  (let ((rand12 (new heap of '(0 1 2 3 4 5 6 7 8 9 10 11))))
    (transp-to 0 (safesort 
		  (loop for i from 1 to chd-length collect (next rand12))))))

;; RANDOM-MODE -- makes a random mode with mode-length members
(defun random-mode (mode-length)
  (transpose (new mode steps 
       (melint (append (random-chord mode-length) (list 12))))
	     (random 12)))

;; CHORD-COMPLEMENT -- all pc's mod 12 outside of a chord
(defun chord-complement (a-chord)
  (set-difference '(0 1 2 3 4 5 6 7 8 9 10 11) a-chord))

;; TINTAB -- nearest 'modal' counterline to a melody
;; (tintab '(50 51 52 53 54 55 56 57 58) stravmode) = 
;; (50 50 52 53 53 55 55 57 58)
(defun tintab (input modename)
        (cond ((eql input 'r) 'r)
              ((numberp input) 
               (keynum (note (keynum input :to modename)
                             :in modename)))
              ((listp input) (loop for x in input collect
                                   (tintab x modename)))))

;; fun with chords
; (prime-form '(60 20 3)) = (0 3 7)
; (octave-number 70) = 4
; (pitch-class 40) = 4

;; PLAYING MODES WITH CODES
;
;(load "data/codes.lisp")
;
;(define (dochords2)
;  (let ((codeheap (new heap of code-7p3)))
;  (process repeat 200
;	   do (doeach (k (cw->mode (next codeheap) (random-mode 7) 40))
;		      (output (new midi :keynum k :time (now))))
;               wait .1)))
;
;(events (dochords2) "that.midi")


;; PLAYING MODES WITH CHORDS

;(load "data/chords.lisp")

;(define (dochords3)
;  (let ((codeheap (new heap of code-7p3)))
;  (process repeat 200
;	   do (doeach (k (cw->mode (next codeheap) (random-mode 7) 40))
;		      (output (new midi :keynum k :time (now))))
;               wait .1)))

;(events (dochords3) "that.midi")

;;; added June 2005
;; MAKE-TTMODE -- random twelve-tone row in "size" # of octaves
(defun make-ttmode (size)
  (let* ((hp (make-poly (heapvec 11 11 1) (list (/ 12 size))))
         (modepits (loop for x to (- (length hp) 1) append
                         (transp (safesort (nth x hp)) (* x 12))))
         (topoctave (list (* 12 size))))
(new mode :degrees (append (list 0) modepits topoctave))))


;; added Feb 2012

;; FIELD-MOD12P -- test for all mod12 pits included in (registral) field
(defun field-mod12p (field)
  (list-eql (indices 12)
	    (safesort (remove-duplicates
		       (mod12 field)))))

; RANGE-IN-MODE -- gives all mode-pits between [true keynums] 'minpit' and 'maxpit' 
(defun range-in-mode (minpit maxpit a-mode)
  (no-nils
   (loop for n from minpit to maxpit collect (keynum n :in? a-mode))))

; MODE-MOD12P -- mod12 test for mode from 'truepit-min' to 'truepit-max'
(defun mode-mod12p (minpit maxpit a-mode)
  (field-mod12p (range-in-mode minpit maxpit a-mode)))

;; MOD12-COLLECT -- collects 'inlist' into sublists by mod12
;; returns (list 'r) if mod is not in list
(defun mod12-collect (inlist)
  (mapcar (lambda (x) (if (eql x nil) (list 'r) x))
	  (loop for modnum in (indices 12) collect
		(filter (lambda (x) (eql (mod12 x) modnum)) inlist))))

;; PLACE-BY-MOD12
(defun place-by-mod12 (alist fieldrange)
  (let ((mod12-fields (mod12-collect fieldrange)))
    (loop for x in alist collect
	  (if (numberp x) (pickl (nth (mod12 x) mod12-fields)) (place-by-mod12 x fieldrange)))))

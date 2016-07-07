; MODES-TODO.LISP

(load "nudruz.lisp")
(load "rewrite.lisp")
(load "modes.lisp")


;; list of all pits in mode
(defun allmodenotes (modename)
  (remove-duplicates
   (loop for x to 128 collect (keynum (note x :through modename)))))

;; vector of modal indices
(defun allmodeindx (modename)
  (let ((allmdlen (length (allmodenotes modename))))
    (loop for x to (- allmdlen 1) collect 
          (mod x (slot-value modename 'divisions)))))

; e.g.,
; (= (length (allmodenotes octatonic)) (length (allmodeindx octatonic))) = T

;; MODENUM -- converts keynums to (overall) mode indices
;; all non-mode pits are converted to 'R
(defun modenum (melody modename)
  (let ((modevec (allmodenotes modename)))
    (if (listp melody)
      (loop for x in melody collect
            (if (note x :in? modename) 
              (position x modevec)
              'R))
      (if (note melody :in? modename) (position melody modevec) 'R))))

; e.g.
; (allmodenotes pentatonic) = (0 2 4 7 9 12 14 ....
; (allmodenotes stravmode) = (0 2 3 5 7 8 10 12 13 15 ...




(modereg 55 octatonic)

(mode-lowest pentatonic)
(mode-octave pentatonic)

(floor 

;; converts mode indices by mod modesize
(defun modemodnum (modenums modename)
  (let ((msize (mode-size modename)))
    (mapcar (lambda (x) (mod x msize)) modenums)))





(mixo '(7 9 11 9 9 11 9 7 9)) ;  = (7 9 10 9 9 10 9 7 9)


(modeheight (heapvec 30 80) pentatonic)

(modeheight 54 pentatonic)
















;;;;;;;; FILTERING ON MODES

(define theselens '(4.5 5.25 2.0 3.0 4.75))

(define modelist '(pelog pentatonic wholetone stravmode))

(define modecyc (new cycle of modelist))

(define rawcyc (new cycle of (transp (heapvec 2000 48) 50)))

(define (modey modename len)
  (process
    repeat (/ len .125)
        output (multievent 'midi :keynum
                             :keynum (keynum 
                                      (note (next rawcyc) :through (eval modename)))
                             :time (now)
                             :duration .125)
        wait .125))

(define mmmode
  (process for x in theselens
           sprout (modey (next modecyc) x) at (now)
           wait x))

(events mmmode "mmmode.midi")


;; NEEDS WORK!!
(defun modefilt (pitlist durlist modelist regions)
  (let ((atx (melint->line 0 durlist))
        (modecyc (new cycle of modelist))
        (atkcyc (new cycle of atklist)))
    (loop until (eop? pitcyc)
      append (keynum (note (next pitcyc (next atkcyc)) 
                           :through (eval (next modecyc)))))))


(define myx '())







;; 


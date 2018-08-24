(in-package :cm)

;; mathieu etc.

;(load "nudruz.lisp")
;(load "beats.lisp")
;(load "inflect.lisp")
;(load "designs.lisp")
;(load "modes.lisp")
;(load "diffs.lisp")
;(load "rewrite.lisp")
;(load "graphs.lisp")
;(load "phrasing.lisp")
;(load "tonnetz.lisp")
;(load "selfsim.lisp")
;(load "reger.lisp")
;(load "scanons.lisp")
;(load "nondet.lisp")
;(load "motive.lisp")
;(load "lewin.lisp")
;(load "slonimsky.lisp")

;(load "mathieu11alt.lisp")



;; moving 'w11alt' to 0..11 domain
(defparameter w11alt0 (mapcar (lambda (x) (transp x -1)) w11alt))

; & shuffled
(defparameter shfw (shuffle w11alt0))

;; shorter 'm11alt' for prototyping
(defparameter shortm11 (subseq m11alt 0 20))

;; NOTE: "chds->lines" will give smoothness, instead of 'smoothlist'

;; smoothlist with ttmode
; MKG: Disabling example.
#|
(events
 (splay
  (play-mode
   (transp
    (smoothlist
     (loop for x in shortm11 append
	   (mapcar (lambda (y) (listsub x (indices 12) y)) shfw)))
    11)
   (make-ttmode 4)
   )
  .125)
  "out.midi" :play 'nil)
|#
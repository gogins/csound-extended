;;;; N U D R U Z   C S O U N D   F F I   E X A M P L E S
;;;; Michael Gogins
;;;; August 2018

#|
Contrary to CM documentation, the events function does not return a usable seq object.
The generated score is placed into the seq that is passed to events.
|#
(require :asdf)
(require :sb-posix)
(require :cm2)
(require :fomus)
(require :nudruz)
(in-package :cm)
(use-package :screamer-user)

(defparameter csound-seq (new seq :name "csound-test"))

(events 
   ; (selfsim cyclops2p5x9 '(0 2 3) '(2 5 9) '(68 80 76) .25)
    (selfsim cyclops6x7 (shuffle (transp '(60 62 64 65) 12)) '(6 8 13 15) '(0 -7 -14 -21) .25)
    csound-seq
)

(defparameter *piano-part* 
  (new fomus:part
   :name "Piano"
   :partid 0 
   :instr '(:piano :staves 2)))
(defparameter partids (make-hash-table))
(setf (gethash 0 partids) 0)
(defparameter voices (make-hash-table))
(defparameter voicelist '(1 2 3))
(setf (gethash 0 voices) voicelist)
(seq-to-lilypond csound-seq "selfsim.ly" *piano-part* partids voices :title "selfsim" :composer "Drew Krause")
(defparameter arrangement (make-hash-table))
(setf (gethash 0 arrangement) (list 8 3 .5))
(render-with-csd csound-seq csd-text :channel-offset 0 :velocity-scale 120 :csd-filename "selfsim.csd" :arrangement arrangement)
(quit)


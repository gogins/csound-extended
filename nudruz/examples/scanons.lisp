;;;; N U D R U Z   C S O U N D   F F I   E X A M P L E S
;;;; Michael Gogins
;;;; August 2018

#|
Contrary to CM documentation, the events function does not return a usable seq object.
The generated score is placed into the seq that is passed to events.
|#
(require :asdf)
(require :cm2)
(require :fomus)
(require :nudruz)
(in-package :cm)
(use-package :screamer-user)

(defparameter csound-seq (new seq :name "csound-test"))

(events
 (splay
  (loop for scan in 
        '((0 4 7 11) (0 3 6 10) (0 3 7 10) (0 4 7 10) 
          (0 3 6 9) (0 3 7 5) (0 4 7 6)
          (0 -1 2 6) (0 3 6 5) (0 4 2 6) (0 -2 2 5) 
          (0 -1 2 5) (0 3 2 5) (0 -2 2 0)
          (0 -1 2 1))
        append (stack-can (transp '(0 3 6 5) (+ 50 (random 12))) 7 3))
  .75) csound-seq)

(defparameter *piano-part* 
  (new fomus:part
   :name "Piano"
   :partid 0 
   :instr '(:piano :staves 2)))
(defparameter partids (make-hash-table))
(setf (gethash 0 partids) 0)
(defparameter voices (make-hash-table))
(defparameter voicelist 1)
(setf (gethash 0 voices) voicelist)
(seq-to-lilypond csound-seq "scanons.ly" *piano-part* partids voices :title "Stacked Canons" :composer "Drew Krause")
(render-with-csd csound-seq csd-text :channel-offset 21 :velocity-scale 150 :csd-filename "scanons.csd")
(quit)


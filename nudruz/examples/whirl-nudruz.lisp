;;;; N U D R U Z   C S O U N D   F F I   E X A M P L E S
;;;; Michael Gogins
;;;; August 2018

#|
Contrary to CM documentation, the events function does not return a usable seq object.
The generated score is placed into the seq that is passed to events.
|#
(require :asdf)
(require :cm2)
(require :nudruz)
(require :fomus)
(in-package :cm)

(defparameter csound-seq (new seq :name "csound-test"))
(events (whirl 10 .1 .5 20 10 50 harms) csound-seq)
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
(seq-to-lilypond csound-seq "whirl.ly" *piano-part* partids voices)
(render-with-csd csound-seq csd-text :channel-offset 9 :velocity-scale 100)
(quit)


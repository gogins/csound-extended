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
(events (tzplay) csound-seq)
(defparameter *piano-part* 
  (new fomus:part
   :name "Piano"
   :partid 0 
   :instr '(:piano :staves 2)))
(defparameter partids (make-hash-table))
(setf (gethash 0 partids) 0)
(defparameter voices (make-hash-table))
(setf (gethash 0 voices) (list 1 2 3))
(seq-to-lilypond csound-seq "tzplay.ly" *piano-part* partids voices)
;(render-with-orc csound-seq orc-text :channel-offset 25 :velocity-scale 100)
    



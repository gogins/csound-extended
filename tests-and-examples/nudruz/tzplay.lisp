;;;; N U D R U Z   C S O U N D   F F I   E X A M P L E S
;;;; Michael Gogins
;;;; August 2018

#|
Contrary to CM documentation, the events function does not return a usable seq 
object. The generated score is placed into the seq that is passed to events.
|#
(load "~/quicklisp/setup.lisp")
;; There's a bug in Fomus. The first time you run this program, uncomment the 
;; following line, then run it again with the line commented out again.
;;(require :fomus)
(ql:quickload :nudruz)
(in-package :cm)

(defparameter csound-seq (new seq :name "csound-test"))
(events (tzplay) csound-seq)
(defparameter *piano-part* 
  (new fomus:part
   :name "B3"
   :partid 0 
   :instr '(:piano :staves 2)))
(defparameter partids (make-hash-table))
(setf (gethash 0 partids) 0)
(defparameter voices (make-hash-table))
(setf (gethash 0 voices) (list 1 2))
(seq-to-lilypond csound-seq "tzplay.ly" *piano-part* partids voices :title "tzplay" :composer "Drew Krause")
(render-with-csd csound-seq csd-text :channel-offset 25 :velocity-scale 120 :csd-filename "tzplay.csd")

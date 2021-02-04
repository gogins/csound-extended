;;;; N U D R U Z   C S O U N D   F F I   E X A M P L E S
;;;; Michael Gogins
;;;; August 2018

#|
Contrary to CM documentation, the events function does not return a usable seq object.
The generated score is placed into the seq that is passed to events.
|#
(require :asdf)
(require :nudruz)
(load "example-csd.lisp")
(in-package :cm)
(use-package :screamer-user)

(defparameter csound-seq (new seq :name "csound-test"))

(events
 (splay
  (transp
   (mapcar (lambda (x) (car (stack-by x 7))) 
	   (flatter
	    (mapcar (lambda (x) (stravrot x 'n))
		    (subsequences (randhexrow) 6))))
   45)
  (makecyc (transp (code->durs (resclassvec 3 5 7)) 2)))
csound-seq)

(defparameter *piano-part* 
  (new fomus:part
   :name "Filtered Sines"
   :partid 0 
   :instr '(:piano :staves 2)))
(defparameter partids (make-hash-table))
(setf (gethash 0 partids) 0)
(defparameter voices (make-hash-table))
(defparameter voicelist '(1 2 3 4))
(setf (gethash 0 voices) voicelist)
(seq-to-lilypond csound-seq "stack-can.ly" *piano-part* partids voices :title "stack-can" :composer "Drew Krause")
(render-with-csd csound-seq csd-text :channel-offset 9 :velocity-scale 100 :csd-filename "stack-can.csd")
(quit)


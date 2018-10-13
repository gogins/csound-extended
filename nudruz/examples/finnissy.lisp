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

(defparameter csound-seq (new seq :name "csound-test"))
(events
 (let ((mypits (heapvec 100 10 50))
       (durs (strums 20 2 6 4 6)))
   (list
    (splay mypits (ferney '(1) '(4) durs))
    (splay (transp mypits 3) (ferney '(1) '(5) durs))
    (splay (transp mypits 21) (ferney '(1) '(3) durs)))) csound-seq)
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
(seq-to-lilypond csound-seq "finnissy.ly" *piano-part* partids voices :title "Finnissy Effect" :composer "Drew Krause")
(render-with-csd csound-seq csd-text :channel-offset 18 :velocity-scale 100 :csd-filename "finnissy.csd")
(quit)


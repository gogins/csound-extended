;;;; N U D R U Z   C S O U N D   F F I   E X A M P L E S
;;;; Michael Gogins
;;;; August 2018

#|
Contrary to CM documentation, the events function does not return a usable seq object.
The generated score is placed into the seq that is passed to events.
|#
(require :asdf)
(require :nudruz)
(in-package :cm)

(defun sierpinski (tone melody levels dur amp)
  (let ((len (length melody)))
    (process for i in melody
             for k = (transpose tone i)
             ;; play current tone in melody
             output (new midi :time (now) :duration dur 
                         :amplitude amp :keynum k
                         :channel 0)
             when (> levels 1)
             ;; sprout melody on tone at next level
             sprout (sierpinski (transpose k 12)
                                melody
                                (- levels 1)
                                (/ dur len) 
                                amp)
             wait dur)))

(defparameter csound-seq (new seq :name "csound-test"))
(events (sierpinski 'a0 '(0 7 5 1) 4 3 .5) csound-seq)
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
(seq-to-lilypond csound-seq "sierpinski.ly" *piano-part* partids voices :title "Sierpinski" :composer "Drew Krause")
(render-with-csd csound-seq csd-text :channel-offset 18 :velocity-scale 100 :csd-filename "sierpinski.csd")
(quit)


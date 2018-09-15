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

(let ((csound-seq (new seq :name "csound-test")))
(events (whirl 10 .1 .5 20 10 50 harms) csound-seq)
(render-with-csd csound-seq csd-text :channel-offset 9 :velocity-scale 100))
(quit)


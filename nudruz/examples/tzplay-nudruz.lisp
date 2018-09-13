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
(progn 
    (package-name (symbol-package 'seq))
    (let ((csound-seq (new seq :name "csound-test")))
    (events (tzplay) csound-seq)
    (render-with-orc csound-seq orc-text :channel-offset 25 :velocity-scale 100))
)



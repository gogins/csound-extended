;;;; N U D R U Z   C S O U N D   F F I   E X A M P L E S
;;;; Michael Gogins
;;;; August 2018

#|
This file contains some examples pulled from the nudruz codebase
and rendered with Csound via the csound FFI wrapper.

Contrary to CM documentation, the events function does not return a usable seq object.
The generated score is placed into the seq that is passed to events.
|#

(require "asdf")
(asdf:make :nudruz)

(in-package :cm)

(let ((csound-seq (new seq :name "csound-test")))
(events (tzplay) csound-seq)
(render-with-csound csound-seq csd-text 5 100))

(let ((csound-seq (new seq :name "csound-test")))
(events (whirl 10 .1 .5 20 10 50 harms) csound-seq)
(render-with-csound csound-seq csd-text 18 100))


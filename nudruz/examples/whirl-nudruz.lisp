;;;; N U D R U Z   C S O U N D   F F I   E X A M P L E S
;;;; Michael Gogins
;;;; August 2018

#|
Contrary to CM documentation, the events function does not return a usable seq object.
The generated score is placed into the seq that is passed to events.
|#

(require "asdf")
(print (format t "ASDF version: ~S~%" (asdf:asdf-version)))
#+lispworks
(push "~/.local/share/common-lisp/source/" asdf:*central-registry*)
(print (format t "ASDF central registry: ~D~%" asdf:*central-registry*))

(asdf:make :nudruz)

(in-package :cm)

(let ((csound-seq (new seq :name "csound-test")))
(events (whirl 10 .1 .5 20 10 50 harms) csound-seq)
(render-with-csound csound-seq csd-text 9 100))


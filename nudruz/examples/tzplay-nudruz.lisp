;;;; N U D R U Z   C S O U N D   F F I   E X A M P L E S
;;;; Michael Gogins
;;;; August 2018

#|
Contrary to CM documentation, the events function does not return a usable seq object.
The generated score is placed into the seq that is passed to events.
|#

(require "asdf")
(print (format t "ASDF version: ~S~%" (asdf:asdf-version)))
(push "~/.local/share/common-lisp/source/" asdf:*central-registry*)
(print (format t "ASDF central registry: ~D~%" asdf:*central-registry*))

(asdf:load-system :nudruz)

(in-package :cm)

(let ((csound-seq (new seq :name "csound-test")))
(events (tzplay) csound-seq)
(render-with-csound csound-seq csd-text 53 100))

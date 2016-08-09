;; An example demonstrating how to render tzplay using Csound via FFI.

(in-package :cm)

;;; MKG replacing this with Csound: (events tzplay "tzplay.midi")
;;; Contrary to CM documentation, events does not return a usable seq object.
;;; The generated score is placed into the seq that is passed to events.
(let ((csound-seq (new seq :name "csound-test")))
(events (tzplay) csound-seq)
(render-with-csound csound-seq csd-text 18 100))

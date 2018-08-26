(in-package :cm)
(use-package :csound)

(defun event-to-istatement (event channel-offset velocity-scale)
"
Translates a Common Music MIDI event to a Csound score event
(i-statement), which is terminated with a newline. An offset, which may
be any number, is added to the MIDI channel number.
"
    (format nil "i ~,6f ~,6f ~,6f ~,6f ~,6f 0 0 0 0 0 0~%" (+ channel-offset (midi-channel event)) (object-time event)(midi-duration event)(keynum (midi-keynum event))(* velocity-scale (midi-amplitude event)))
)
(export 'event-to-istatement)

(defun replace-all (string part replacement &key (test #'char=))
"
Replaces all occurences of the string 'part' in 'string' with 'replacement',
using 'test' for character equality.
"
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun render-with-csound (sequence csd-text &optional (channel-offset 1) (velocity-scale 127) (csound nil))
"
Given a Common Music seq 'sequence', translates each of its events into a
Csound 'i' statement, optionally offsetting the channel number and/or
rescaling MIDI velocity, then renders the resulting score using 'csd-text'.
A CSD is used because it can contain any textual Csound input in one block of
raw text. The score generated from 'sequence' is appended to any <CsScore>
lines found in 'csd-text'. This is done so that Csound will quit performing
at the end of the score. It is possible to call csoundReadScore during the
performance. This function returns the Csound object that it uses.

The optional 'csound' parameter is used to call Csound if passed. This enables
'render-with-csound' to be run in a separate thread of execution, and for
the caller to control Csound instrument parameters during real time
performance, e.g.

(setq csound (sb-csound:csoundCreate 0))
(setq my-thread (bt:make-thread (lambda () (render-with-csound cs csd 1 127 csound))))
(sb-csound:csoundSetControlChannel csound 'mychannel' myvalue)
(bt:join-thread my-thread)

"

    (let
        ((score-list (list))
        (cs)
        (sco-text)
        (result)
        (new-csd-text))
        (progn
            (format t "Building Csound score...~%")
            (defun curried-event-to-istatement (event)
                (event-to-istatement event channel-offset velocity-scale))
            (setq score-list (mapcar 'curried-event-to-istatement (subobjects sequence)))
            (setq sco-text (format nil "~{~A~^ ~}" score-list))
            (print sco-text)
            (if csound
                (setq cs csound)
                (progn
                    (setq cs (sb-csound:csoundCreate 0))
                    (format t "csoundCreate returned: ~S.~%" cs)
                )
            )
            (setq new-csd-text (replace-all csd-text "</CsScore>" (concatenate 'string sco-text "</CsScore>")))
            (format t "new-csd-text: ~A~%" new-csd-text)
            (setq result (sb-csound:csoundCompileCsdText cs new-csd-text))
            (format t "csoundCompileCsdText returned: ~D.~%" result)
            (setq result (sb-csound:csoundStart cs))
            (format t "csoundStart returned: ~D.~%" result)
            (loop
                (setq result (sb-csound:csoundPerformKsmps cs))
                (when (not (equal result 0))(return))
            )
            (setf result(sb-csound:csoundCleanup cs))
            (format t "csoundCleanup returned: ~D.~%" result)
            (sleep 5)
            (if (not csound)
                (sb-csound::csoundDestroy cs)
                (format t "csoundDestroy was called.~%")
            )
            (format t "The Csound performance has ended: ~D.~%" result)
        )
    )
)



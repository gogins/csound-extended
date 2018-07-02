(in-package :cm)

;; MIDI-IN -- creates (pits durs) two-element list of lists from midifile
;; note: gives same duration to simultaneous atx (legato to next atk)
(defun midi-in (midifile)
  (let ((mytracks (import-events midifile :meta-exclude t))
        (pits '())
        (durs '())
        (starts '()))
    (map-subobjects #'(lambda (x) (push x pits)) mytracks :key #'midi-keynum :type 'midi)
    (map-subobjects #'(lambda (x) (push x durs)) mytracks :key #'midi-duration :type 'midi)
    (map-subobjects #'(lambda (x) (push x starts)) mytracks :key #'object-time :type 'midi)
    (list 
     (first (combine-pits (nreverse pits) (nreverse starts)))
     (append (melint (remove-duplicates starts)) (list 1.0)))))


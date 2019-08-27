(in-package :cm)

;; MIDI-IN -- creates (pits durs) two-element list of lists from midifile
;; note: gives same duration to simultaneous atx (legato to next atk)
(defun midi-in (midifile)
  (let ((mytracks (import-events midifile :meta-exclude t))
        (pits '())
        (durs '())
        (starts '()))
    ; MKG changed to handle the list of one seq per track now returned from 
    ; import-events in a backwardly-compatible way.
    (if (listp mytracks)        
        (loop for mytrack in mytracks do (progn
            (map-subobjects #'(lambda (x) (push x pits)) mytrack :key #'midi-keynum :type 'midi)
            (map-subobjects #'(lambda (x) (push x durs)) mytrack :key #'midi-duration :type 'midi)
            (map-subobjects #'(lambda (x) (push x starts)) mytrack :key #'object-time :type 'midi)
        ))
        (progn
            (map-subobjects #'(lambda (x) (push x pits)) mytrack :key #'midi-keynum :type 'midi)
            (map-subobjects #'(lambda (x) (push x durs)) mytrack :key #'midi-duration :type 'midi)
            (map-subobjects #'(lambda (x) (push x starts)) mytrack :key #'object-time :type 'midi)
        ))
    (list 
        (first (combine-pits (nreverse pits) (nreverse starts)))
        (append (melint (remove-duplicates starts)) (list 1.0)))))


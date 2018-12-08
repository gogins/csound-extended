(print "Testing 'Computer Models of Musical Creativity' ASDF and examples.")
(require :asdf)
(asdf:load-system "cmmc")
(asdf:load-system "nudruz")
(load "/home/mkg/csound-extended/nudruz/sources/all-in-one-orc.lisp")

(in-package :cl-user)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)
(defparameter output "dac")

;(print forgray)
;(defparameter improvise-events (improvise forgray))
;(print improvise-events)

(defparameter poet-generated (poet '(life love angel mercy patience) *choices*))
(print "post-generated:" poet-generated)

(setq *illegal-verticals* 
'(0 1 2 5 6 7 10 11 13 14 17 18 19 22 23 25 26 29 30 34 35 -1 -2 -3 -4 -5 -6 -7 -8))

(setq *cantus-firmus* '(69 71 72 76 74 72 71))
;(defparameter gradus-production (gradus))
;(print gradus-production)
;(defparameter (make-events (gradus-production)))
;(print gradus-events)
;(quit)
(defun CREATE-CANON ()
  "Creates a simple canon in two voices using gradus."
  (setq *seed-note* (- (my-last *cantus-firmus*) 12))
  (gradus)
  (setq *save-voices* (evaluate-pitch-names *save-voices*))
  (let* ((theme (append *cantus-firmus* (mapcar #'(lambda (x)(+ x 12))(second *save-voices*))))
         (lower-voice (mapcar #'(lambda (x)(- x 12)) theme)))
    (make-events 
     (pair (list (append theme theme theme (make-list (length  *cantus-firmus*) :initial-element 0))
                 (append (make-list (length  *cantus-firmus*) :initial-element 0) lower-voice lower-voice lower-voice))))))

(defparameter gradus-events (create-canon))
(print gradus-events)
(defparameter gradus-seq (cm::cope-events-to-seq gradus-events))
(cm::render-with-orc gradus-seq cm::all-in-one-orc :output output :channel-offset 28 :velocity-scale 90 :csd-filename "gradus.csd")
(quit)

(defparameter cosine-events (make-events (cosine 20 24 108)))
(print cosine-events)
(defparameter cosine-seq (cm::cope-events-to-seq cosine-events))
(cm::render-with-orc cosine-seq cm::all-in-one-orc :output output :channel-offset 63 :velocity-scale 90 :csd-filename "cosine.csd")

(defparameter sonified-events (make-events (sonify cassiopeia-a)))
(defparameter sonified-seq (cm::cope-events-to-seq sonified-events))
(cm::render-with-orc sonified-seq cm::all-in-one-orc :output output :channel-offset 63 :velocity-scale 90 :csd-filename "sonified.csd")

(defparameter markovian-events_(compose-new-music-based-on-markovian-probabilities 60 50 '((0 60 1000 1 127) (0 60 1000 1 127) (1000 62 1000 1 127) (2000 64 1000 1 127) (3000 65 1000 1 127) (4000 67 1000 1 127) (5000 65 1000 1 127)(6000 69 1000 1 127) (7000 71 1000 1 127) (8000 60 1000 1 127))))
(defparameter markovian-seq (cm::cope-events-to-seq markovian-events_))
(cm::render-with-orc markovian-seq cm::all-in-one-orc :output output :channel-offset 1 :velocity-scale 100 :csd-filename "markov.csd")


(quit)


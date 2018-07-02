(in-package :cm)

; METHOD 1: EXPORT

(defparameter up
  (process for key from 60 to 72 
        for beg from 0 by 1
        collect (new midi :time beg
                     :keynum key
                     :duration 1)))

(events (splay (indices 10 60) .5) "my-score.eps" :title "up")

(events up "my-score.eps" :title "up!")

; ; then ">gs my-score.eps" to see it!



;; METHOD 2: NATIVE

(in-package :cmn)

(cmn (size 24) staff alto (key e-minor) (meter 3 4) 
     b4 q     g4 q     e4 q
     a4 q     f4 q  ds4 q
     b3 q  cs4 e ds4 e e4 e f4 e
     g4 h e4 q
     e5 q  c5 q a4 q
     b4 q g4 q e4 q
     a4 e f4 e ds4 e b3 e cs4 e d4 e
     e4 h.
     double-bar)


(cmn (size 24) staff treble (key b-minor) (meter 2 4) 
     b4 e f4 e d5 e b4 e
     as4 e c5 e f4 q
     as4 e c5 e f4 e e5 e
     d5 q b4 q
     g4 e b4 e e4 e g4 e
     f4 e b4 e d4 e b4 e
     as4 e f4 e c5 e a4 e
     e5 e d5 s c5 s b4 q
     double-bar)
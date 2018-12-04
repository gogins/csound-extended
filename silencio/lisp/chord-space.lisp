(require :asdf)
(asdf:load-system :nudruz)
;(asdf:load-system :cm2)
;(in-package :silencio)
(in-package :cm)

(defparameter major (new mode :degrees '(c  d  e f  g  a  b c)))
(defparameter M (new mode :degrees     '(c     e    g        )))
(print major)
(print M)
(defparameter CM (transpose M 0))
(print CM)
(defparameter CSM (transpose M 1))
(print CSM)
(print (keynum 'a7 :through CSM))
(defparameter M9 (new mode :degrees    '(c  d  e    g     b  c)))
(defparameter m9 (new mode :degrees    '(c  d  ef    g     bf  c)))



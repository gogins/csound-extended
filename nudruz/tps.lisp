
;; TONAL PITCH SPACE
(load "/home/drew/Lisp/cm-2.4.0/Druzaks/nudruz.lisp")

;; four levels (with all belonging to lower levels as well)
;; 'tonic' = 1 member
;; 'triad' = 2 more members: "basechd"
;; 'scale' = 4 more members (vary?)
;; 'chromatic space' = 5 (remaining) members

;; ---> build from modes?!?

;; note that :scale slot defaults to the chromatic scale 

(defclass person ()
  ((name :accessor person-name
	 :initform 'bill
	 :initarg :name)
   (age :accessor person-age
	:initform 10
	:initarg :age)))

(defclass tps ()
  ((tonic :accessor tps-tonic 
	  :initform 0
	  :initarg :tonic)
  (triad :accessor tps-triad 
	 :initform '(0 4 7)
	 :initarg :triad)
  (scale :initform '(2 5 9 11))))

(define mytps (make-instance 'tps))




(define myspace (make-tps))

(tps-scale myspace)


(define mymode (new mode :tonic 'd :steps '(c d e fs g a bf)))
(loop for x to 10 collect (note x :in mymode))

(define thatmode (make-instance 'mode :tonic 'f))

(define mymode (new mode :tonic 'd))

(define mytps (new tpspace :tonic 'd :basechd '(d f a)))

; (slot-value mymode 'scale) = #<tuning "chromatic-scale">
; (slot-value mymode 'octave) = 12

(slot-value mymode 'keynum-offset) = steps above C for tonic

;Mode slots:
;   STEPS          =  (110592)
;   LOWEST         =  (D-1 2 0 2 D NIL)
;   SCALE          =  #<tuning "chromatic-scale">
;   OCTAVE         =  12
;   DIVISIONS      =  1
;   INTO           =  (0 NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;   KEYNUM-OFFSET  =  2
;   NAME           =  NIL


(defclass tpspace ()
  ((tptonic

(

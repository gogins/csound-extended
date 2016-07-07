
;(defclass class-name (superclass-name*)
;  (slot-description*)
;  class-option*)

; :ACCESSOR function-name
; :INITFORM expression
; :INITARG symbol


(defclass 2d-object () ())

(defclass 2d-centered-object (2d-object)
  ((x :accessor x)
   (y :accessor y)
   (orientation :accessor orientation)))

(defclass oval (2d-centered-object)
  ((axis-1 :accessor axis-1)
   (axis-2 :accessor axis-2)))

(defclass regular-polygon (2d-centered-object)
  ((n-sides :accessor number-of-sides)
   (size :accessor size)))








(defclass 3d-point ()
  ((x :accessor point-x)
   (y :accessor point-y)
   (z :accessor point-z)))

(let ((a-point (make-instance '3d-point)))
  (setf (point-x a-point) 0)
  (point-x a-point))


(defclass 3d-point ()
  ((x :reader get-x :writer set-x)
   (y :reader get-y :writer set-y)
   (z :reader get-z :writer set-z)))

(define mypoint (make-instance '3d-point))

(set-x 4 mypoint)
(set-y 3 mypoint)
(set-z 2 mypoint)

(defclass sphere ()
  ((x :accessor x)
   (y :accessor y)
   (z :accessor z)
   (radius :accessor radius)
   (volume :reader volume)
   (translate :writer translate)))

;; huh?
(defclass melody ()
  ((pits :reader get-pits :writer set-pits)
   (ivec :reader ivec)
   (most-related-transp :reader most-related-transp)
   (octmod :accessor octmod :initval??? 12)
   (inversion :reader inversion)

((def


(defmethod volume ((obj sphere))
  (* 4/3 pi (expt (radius obj) 3)))


(define mysphere (make-instance 'sphere))

(setf (x mysphere) (get-x mypoint))
(setf (y mysphere) (get-y mypoint))
(setf (z mysphere) (get-z mypoint))

(setf (radius mysphere) 3.1)

(volume mysphere)

(setf (radius mysphere) 4.5)



(defclass person ()
  ((name :accessor person-name
        :initform 'bill
        :initarg :name)
   (age :accessor person-age
        :initform 10
        :initarg :age)))

(make-instance 'person :age 100)

(defun make-person (name age)
  (make-instance 'person :name name :age age))

(setq p1 (make-instance 'person :name 'jill :age 100))

(* 2 (person-age p1))


(defclass teacher (person)
  ((subject :accessor teacher-subject
            :initarg :subject)))

(defclass maths-teacher (teacher)
  ((subject :initform "Mathematics")))


(setq p2 (make-instance 'maths-teacher
                        :name 'john
                        :age 34))

(define p2b (make-instance 'maths-teacher
                           :name 'joan
                           :age 43))

(defmethod change-subject ((teach teacher) (new-subject string))
  (setf (teacher-subject teach) new-subject))

(change-subject p2b "biology")

(slot-value p2 'subject)


(load "nudruz.lisp")
(load "modes.lisp")

(find-class 'mode)

(defstruct ship
  x-position
  y-position
  x-velocity
  y-velocity
  mass)

(setq ship2 (make-ship))


; (typep pentatonic 'mode) = T
; (type-of pentatonic) = mode
; 

(defclass C1 ()
  ((S1 :initform 5.4 :type number)
   (S2 :allocation :class)))

(defclass C2 (C1)
  ((S1 :initform 5 :type integer)
   (S2 :allocation :instance)
   (S3 :accessor C2-S3)))

;;;

(defclass position () ())

(defclass x-y-position (position)
  ((x :initform 0 :initarg :x)
   (y :initform 0 :initarg :y)))

(defclass rho-theta-position (position)
  ((rho :initform 0)
   (theta :initfor 0)))

(class-of 'a)



;; how about...

(defclass figure ()
((prime-form :initarg)
 (contour :initarg)
 (tp-vector :initarg (tpoint (slots->durs) fastest-tact etc))
 (

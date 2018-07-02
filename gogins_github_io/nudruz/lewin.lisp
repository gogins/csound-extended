(in-package :cm)

; (load "nudruz.lisp")
; (load "designs.lisp")
; (load "modes.lisp")
; (load "nondet.lisp")
; (load "graphs.lisp")

; Lewin PNM article on designs

(defparameter lewin-ionian
  (sublis '((1 . 0) (2 . 4) (3 . 7) (4 . 11) (5 . 5) (6 . 9) (7 . 2))
          pplane3))

(defparameter lew-i-heap
  (loop for x in (heapvec 100 7)
        collect (nth x lewin-ionian)))

(defparameter lewin-dorian
  (sublis '((1 . 2) (2 . 5) (3 . 9) (4 . 4) (5 . 7) (6 . 11) (7 . 0))
          pplane3))

(defparameter lew-d-heap
  (loop for x in (heapvec 100 7)
        collect (nth x lewin-dorian)))

(defparameter lewin-mixolydian
  (sublis '((1 . 2) (2 . 11) (3 . 7) (4 . 0) (5 . 9) (6 . 5) (7 . 4))
          pplane3))

(defparameter lew-m-heap
  (loop for x in (heapvec 100 7)
        collect (nth x lewin-mixolydian)))

; goodmode (0 1 2 5 6 7 9)
; triads (2 6 9) (2 5 9) (1 6 9) (0 5 9)

(defparameter lewin-goodmode
  (sublis '((1 . 9) (2 . 6) (3 . 2) (4 . 7) (5 . 5) (6 . 0) (7 . 1))
          pplane3))

(defparameter lew-g-heap
  (loop for x in (heapvec 100 7)
        collect (nth x lewin-goodmode)))

(defparameter lewchords 
  (matchreg-chds (append '((60 72 96)) lew-m-heap)))

(defparameter contralew (au-contraire (topline lewchords) 40))

;; !!
;(events 
; (let ((lewpits (make-poly (flatten lewchords) '(2 1 3 4 1 3))))
;   (trope lewpits (durweight lewpits .25)
;          `(55 ,(heapvec 2 12 80) 34) '(.25 .25 .25)
;          (lambda (x) (eql 3 (length x))))) "out.midi")

;; BUT WOULD LIKE TO USE TROPITS ON THE FLY 


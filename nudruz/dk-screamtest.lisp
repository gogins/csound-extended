(in-package :cm)
; (load (translate-logical-pathname "clocc:src;cllib;base"))
; (load (translate-logical-pathname "clocc:src;cllib;matrix"))
; (load (translate-logical-pathname "clocc:src;cllib;gnuplot"))
; (load (translate-logical-pathname "clocc:src;cllib;octave"))
; (load (translate-logical-pathname "clocc:src;cllib;iter"))
;  (load (translate-logical-pathname "clocc:src;cllib;stat"))

;;(use-package :screamer)

;; FLATTEN -- removes all nesting in list
;; "thank you Paul Graham!"
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc)) 
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))


;; WIGGLE-UTIL: utility for wiggle-to
(defun wiggle-util (wigpair)
   (flatten
    (loop for x from 0 to (- (length (first wigpair)) 1)
          collect (loop repeat (nth x (second wigpair)) 
                    collect (nth x (first wigpair))))))

;;; LIST-EQL: utility for wiggle-to
(defun list-eql (list1 list2)
  (and (subsetp list1 list2) 
       (subsetp list2 list1)
       (= (length list1) (length list2))))

(defun wiggle-to2 (startnum endnum maxlen allowed-ints)
   (remove-duplicates
     (all-values
      (let* ((len (screamer::an-integer-between 0 maxlen))
             (intslen (length allowed-ints))
             (multvars (make-array intslen)))
        (dotimes (j intslen) (setf (aref multvars j) (screamer::an-integer-between 0 len)))
        (assert! (< (apply-nondeterministic #'+ (coerce multvars 'list))
                    (+ maxlen 1)))
        (assert! (= (- endnum startnum)
                  (apply-nondeterministic #'+
                   (map 'list #'* allowed-ints (coerce multvars 'list)))))
        (wiggle-util (list allowed-ints (coerce multvars 'list)))))
    :test #'list-eql))

; (wiggle-to2 45 48 8 '(-1 2)) = ((-1 2 2) (-1 -1 -1 2 2 2))
; (wiggle-to2 45 48 8 '(2 -1)) = ((2 2 2 -1 -1 -1))

(wiggle-to2 45 48 8 '(-1 2))
(wiggle-to2 45 48 8 '(2 -1))



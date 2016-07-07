
(load "rewrite.lisp")
;;;
;;; Temperly-inspired phrasing (hierarchies)
;;; 

;; FRASETREE -- general recursive function (makes midi)
;; pitvec = base melody
;; list-of-levels = figures from slow to fastest [list of lists]
;; basedur = duration of slowest line
;; e.g. (frasetree (indices 12 40) 
;;             (list (transp '(3 2 0) 12) (transp '(0 2 4 6) 24)) 3)
(defun frasetree (pitvec list-of-levels basedur)
  (let ((pitlen (length pitvec)))
    (process for x in pitvec
             output (multievent 'midi :keynum
                                :keynum x
                                :time (now)
                                :duration basedur)
             when list-of-levels
             sprout (frasetree
                     (flatten (transp (car list-of-levels) x))
                     (cdr list-of-levels)
                     (/ basedur (length (car list-of-levels))))
             wait basedur)))
              
;; FRASETREE-PITS
;; general recursive function (builds pitch-slot vector)
;; pitvec = base melody
;; list-of-levels = figures from slow to fastest [list of lists]
;; e.g.  (frasetree-pits (randvec 26 8 20)
;                        (list (transp (randvec 5) 14)
;                              (transp (randvec 4) 23)))))
(defun frasetree-pits (pitvec list-of-levels)
  (let ((pitlen (length pitvec))
        (bigdur (apply #'* (mapcar #'length list-of-levels))))
    (merge-slots
     (list (menses pitvec bigdur)
            (loop for x in pitvec
                  when list-of-levels
                  append (frasetree-pits
                           (transp (car list-of-levels) x)
                           (cdr list-of-levels)))))))




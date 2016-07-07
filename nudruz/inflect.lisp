; INFLECT.LISP
; Drew Krause (2005)
; www.wordecho.org

;(load "rewrite.lisp")
;(load "modes.lisp")

; some keystroke-saving utilities
 

;; MODEHEIGHT -- determines height of pitch in mode
(defun modeheight (pits modename)
  (if (listp pits)
    (mapcar (lambda (x) 
              (mod (- x (mode-lowest modename)) (mode-octave modename)))
            pits)
    (mod (- pits (mode-lowest modename)) (mode-octave modename))))

;; MODEREG -- determines 'octave' of pitch in mode
(defun modereg (pits modename)
  (if (listp pits)
    (mapcar (lambda (x) 
              (floor (/ (- x (mode-lowest modename)) (mode-octave modename))))
            pits)
    (floor (/ (- pits (mode-lowest modename)) (mode-octave modename)))))

;; rewrite rules for inflections, e.g.,
; (un-modal 0 11 wholetone) ==> (10 (0) 10 -> 10 11 10)
; (ln-modal 5 6 ionian) ==> (7 (5) 7 -> 7 6 7)
; (pt-up-modal 4 5 wholetone) ==> (2 (4) 6 -> 2 5 6)
; (pt-down-modal 2 1 stravmode) ==>  (3 (2) 0 -> 3 1 0)

(defun un-modal (anote altnote modename)
  (let ((lowernote (nth (mod (- (position anote (mode-steps modename)) 1)
                             (mode-size modename))
                        (mode-steps modename))))
    `( ,lowernote (,anote) ,lowernote -> ,altnote)))

(defun ln-modal (anote altnote modename)
  (let ((uppernote (nth (mod (+ (position anote (mode-steps modename)) 1)
                             (mode-size modename))
                        (mode-steps modename))))
    `( ,uppernote (,anote) ,uppernote -> ,altnote)))

(defun pt-up-modal (anote altnote modename)
  (let ((uppernote (nth (mod (+ (position anote (mode-steps modename)) 1)
                             (mode-size modename))
                        (mode-steps modename)))
        (lowernote (nth (mod (- (position anote (mode-steps modename)) 1)
                             (mode-size modename))
                        (mode-steps modename))))
    `( ,lowernote (,anote) ,uppernote -> ,altnote)))

(defun pt-down-modal (anote altnote modename)
  (let ((uppernote (nth (mod (+ (position anote (mode-steps modename)) 1)
                             (mode-size modename))
                        (mode-steps modename)))
        (lowernote (nth (mod (- (position anote (mode-steps modename)) 1)
                             (mode-size modename))
                        (mode-steps modename))))
    `( ,uppernote (,anote) ,lowernote -> ,altnote)))

; utility to parse inflection rules
; rule vector = (type anote altnote) 
; types = un, ln, up, dn
; (parse-irule '(un 4 3) wholetone) = (2 (4) 2 -> 2 3 2)
(defun parse-irule (rulevec modename)
  (let* ((itype (first rulevec))
         (anote (second rulevec))
         (altnote (third rulevec))
         (ifunc (case itype
                  ('un #'un-modal)
                  ('ln #'ln-modal)
                  ('up #'pt-up-modal)
                  ('dn #'pt-down-modal))))
    (funcall ifunc anote altnote modename)))

; utility for inflect's id list
(defun mode-rw-idlist (modename)
  (loop for x to (- (mode-octave modename) 1) collect
        `(,x :id ,x)))

; utility to make id nodes for any "altnote" that are lists
(defun mode-ids-for-lists (moderules)
  (no-nils 
   (loop for mr in moderules collect
         (if (listp (third mr)) `(,(third mr) :id ,(third mr))))))

; utility for one-to-one rules
(defun mode-rw-1to1 (modename)
  (loop for x to (- (mode-octave modename) 1) collect
        `(,x -> ,x)))

;;; MODE-INFLECT -- the main function
; - assumes conjunct monody; apply utilities before if necessary
; - assumes that you'll never change mode 0 to mode (n - 1) & vice-versa
;    [otherwise registers won't compute correctly]
; - all ornaments (replacements as lists) return as lists
; (mode-inflect '(19 17 16 14 16) ionian '(ln 2 (3 2 3)) '(dn 5 6)) 
; = (19 18 16 (15 14 15) 16)

(defun mode-inflect (pitlist modename &rest moderules)
  (let* ((pmheight (modeheight pitlist modename))
         (pmreg (modereg pitlist modename))
         (mdoct (mode-octave modename))
         (mdlow (mode-lowest modename))
         (inflect-rw
          (new rewrite
            :of (append (mode-rw-idlist modename) 
                        (mode-ids-for-lists moderules))
            :initially pmheight
            :rules (append 
                    (loop for mrule in moderules collect
                          (parse-irule mrule modename))
                    (mode-rw-1to1 modename))
            :for (length pmheight)))
         (infl-heights (second (rw-gens inflect-rw 2))))
    (loop for x to (- (length infl-heights) 1) collect
          (transp (transp (nth x infl-heights) mdlow) (* mdoct (nth x pmreg))))))



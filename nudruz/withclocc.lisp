;; WITHCLOCC.LISP -- some stuff that requires clocc libraries


;; PROPER-SUBSETS
;; power set of a list, minus trivial (empty & complete) sets
(defun proper-subsets (a-list)
    (butlast 
     (set-difference (cllib:subsets a-list) (list a-list 'nil))))

;; SUBSETS-LEN
;; all subsets of specified size
;; (subsets-len '(1 2 3 4) 2) = ((3 4) (2 4) (2 3) (1 4) (1 3) (1 2))
(defun subsets-len (a-list len)
  (set-difference
   (mapcar (lambda (x) (if (= (length x) len) x)) 
           (cllib:subsets (remove-duplicates a-list)))
   (list 'nil)))

;; PERMUTATIONS --  returns all permutations of a list
(defun permutations (a-list)
  (loop for x in 
        (cllib:permutations-list (make-array (length a-list) 
                                             :initial-contents a-list))
        collect (coerce x 'list)))

;; REORDER-BY-MELINT -- returns a melody permuted with the 
;; most frequent instance of a melint (as list of instances)
;; example: (reorder-by-melint '(1 2 3 4) 2) = 
;;  ((1 3 2 4) (1 3 4 2) (2 4 1 3) (2 4 3 1) (3 1 2 4) 
;;   (3 1 4 2) (4 2 1 3) (4 2 3 1))
(defun reorder-by-melint (a-list mel-intv)
  (let* ((aperms (permutations a-list))
         (maxmel (loop for x in aperms
                       maximize (melint-count x mel-intv))))
    (no-nils (loop for p in aperms collect 
                   (if (= (melint-count p mel-intv) maxmel) p)))))

;; TRICHORD-P -- generalized tricp
;; (trichord-p '(0 7 10) 4 '(0 4 7)) = T
(defun trichord-p (a-list pit trich)
  (cond ((or (not (listp a-list)) (< (length a-list) 2)) 'nil)
        ((= (length a-list) 2) (tricp a-list pit trich))
        ((> (length a-list) 2) 
         (car (member 't (mapcar (lambda (x) (tricp x pit trich))
                            (subsets-len a-list 2)))))))

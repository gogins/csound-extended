
;; from "ampsums" files

;; SPEAR->SLOTS -- interface with imported SPEAR files
;; gives "raw" melodies (rpted notes)
(defun spear->slots (svec)  
  (loop for part in svec collect
        (let* ((frqs (first part))
               (slots (second part))
               (last-slot (car (last slots))))
          (loop for n to last-slot collect
                (if (member n slots) 
                    (round 
                     (keynum (nth (position n slots) frqs) :hz 't)) 'r)))))

;; SPEARMELS -- gives 'tied' melodies
(defun spearmels (svec)
  (let ((spslots (spear->slots svec)))
    (loop for x in spslots collect
          (list (norpt x) (tievec x)))))

;; PLAYSPEARS -- plays all melodies in an imported spears
(defun playspears (svec &optional (basedur .25))
  (let ((smel (spearmels svec)))
    (loop for x in smel collect
          (splay (first x) 
                 (sum-across basedur (second x))))))



;; from "amps" files

(defun blips->slots (avec)
  (let* ((keyslots 
          (sort
           (remove-duplicates
            (loop for blip in avec collect
                  (list (round (keynum (first blip) :hz 't)) (second blip)))
            :test #'list-eql)
           (lambda (x y) (< (second x) (second y)))))
         (slotlen (second (car (last keyslots))))
         (outvec (loop repeat slotlen collect (list 'r))))
    (loop for ks in keyslots do
          (push (first ks) (nth (- (second ks) 1) outvec))
          finally (return outvec))))

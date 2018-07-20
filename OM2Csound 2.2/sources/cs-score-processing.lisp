;;;===================================================
;;; OM2Csound
;;; Control of Csound sound synthesis from OpenMusic
;;;
;;; Utilities for file access (e.g. Csound scores) and automatic local modifications
;;; By Laurent Pottier for PatchWork, 1993
;;;===================================================

(in-package :om)

(defun codes->strg (liste-de-codes)
  (coerce 
   (let ((Lint()))
     (dolist (n liste-de-codes (reverse Lint))
       (push (code-char (+ n 48)) Lint)))
   'string))

(defun regroupe-elts (result)
  (let ((L ()) (Ltemp ()))
    (dotimes (x (length result) (remove nil (reverse L)))
      (cond
       ((= (nth x result) -16)
        (push (reverse Ltemp) L)
        (setf Ltemp ()))
       ((= (nth x result) -39)
        (push (reverse Ltemp) L)
        (setf Ltemp ()))
       ((= (nth x result) -8)
        ())
       ((= (nth x result) -7)
        (push (reverse Ltemp) L)
        (setf Ltemp ()))
       ((= x (1- (length result)))
        (push (nth x result) Ltemp)
        (push (reverse Ltemp) L))
       ((< (nth x result) 10)
        (push (nth x result) Ltemp))
       (t
        (push (nth x result) Ltemp))))))


(defun numberp-liste (liste)
"donne t si la liste ne comporte que des valeurs entre -3 et 9, nil sinon"
  (eval  (cons 'and (mapcar #'(lambda (elt) (and (not (= -1 elt)) (< -4 elt 10))) liste))))

;*********************************************************
(defun base10andtext (result)
  (let ((Li (regroupe-elts result))(Lf ()))
    (dolist (x Li (reverse Lf))
      (push (if (numberp-liste x) (base10 x)
                (codes->strg x)) Lf))))

(defun base10 (liste)
"transforme une liste de codes de char en un nombre; a une soustraction -48 pres  "
  (let ((number 0)(signe 1)(exptmx (expt-max  liste)))
    (if (= -3 (car liste))
      (setf signe -1)())
    (setf liste (remove -3 (remove -2 liste)))
    (dotimes (n (length liste) (if (ratiop (* signe number))
                                 (float (* signe number))
                                 (* signe number)))
      (setf number (+ number (* (expt 10 (- exptmx n)) (nth n liste)))))))

(defun expt-max (liste)
  (let (n1 n2 n3)
    (dotimes (x (length liste)(- n2 n1))
      (cond
       ((and (not n1)(< -1 (nth x liste) 10))
        (setf n1 x n2 x))
       ((= -2 (nth x liste))
        (setf n3 0))
       ((and n1 (not n3) (< -1 (nth x liste) 10))
        (setf n2 x))
       (t 
        ())))))

;*********************************************************
(defun string-to-liste (string)
  "transforme un string de chiffres en un nombre correspondant"
  (let ((result ()))
    (dolist (char (coerce string 'list)(base10andtext (reverse result)))
      (push  (- (char-code char) 48) result))))


;*********************************************************
(defun lire-line-n (nom-fichier n)
  (with-open-file (test nom-fichier
                        :direction :input
                        :if-does-not-exist nil)
    (let (ligne)
      (if (zerop n)
        ()
        (repeat (1- n) 
          (read-line test nil nil)))
        (setf ligne (read-line test nil nil)) ligne)))


(defun write-ds-file (LdeL-de-texte nom-fichier)
  (with-open-file (stream nom-fichier
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    
    (cond ((endp LdeL-de-texte)
           ())
          ((atom LdeL-de-texte)
           (format stream "~a" LdeL-de-texte))
          (t
           (dolist (mot LdeL-de-texte)
             (format stream "~A~A" mot #\tab))))
          (format stream "~%")) LdeL-de-texte)

(defun write-ds-file2 (LdeL-de-texte nom-fichier)
  (with-open-file (stream nom-fichier
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    
    (format stream "~a~%" LdeL-de-texte)))


;*************************************************************************
(defun scal-spec (occurences liste scaler)
"multiplie les elts de liste - indiquees par les indices donnes par la liste 
occurence - par scaler"
  (dolist (n occurences liste)
    (setf (nth n liste)(* scaler (nth n liste)))))


;*************************************************************************
(defun rescal (nom-fich1 nom-fich2 ligne1 ligneend scaler occurences)
  "multiplie les elts de nom-fich1 - dont les indices de colonnes sont indiques 
par la liste occurence - par scaler"
  (if (zerop ligne1) () (resum-file nom-fich1 nom-fich2 0 (1- ligne1)))
  (setf ligne1 (1+ ligne1) ligneend (1+ ligneend))
  (dotimes (x (- (1+ ligneend) ligne1) )
    (write-ds-file 
     (scal-spec scaler occurences
                (string-to-liste 
                 (lire-line-n nom-fich1 (+ x ligne1))) )
     nom-fich2))
  (if (equal ligneend 'EOF) () (resum-file nom-fich1 nom-fich2 (1+ ligneend) 'EOF)))

;*************************************************************************
(defun resum-file (nom-fich1 nom-fich2 ligne1 ligneend)
"pour la reecriture d'une partie d'un fichier,
de la ligne ligne1 a la ligne ligneend"
  (let ((ligne t))
    (setf ligne1 (1+ ligne1) ligneend (1+ ligneend))
    (dotimes (x   (- (1+ ligneend) ligne1))
      (write-ds-file  
       (if ligne 
         (string-to-liste 
          (setf ligne (lire-line-n nom-fich1 (+ x ligne1))
                )))
       nom-fich2)) ligne))


;************************ chge colonne ****************************
(defun chge-val (op scal col L)
(if (and L (numberp (nth col L)))
    (setf (nth col L)(funcall op scal (nth col L)))) L)

(defun chge-col (scorefile outfile op scal col lig-i lig-f)
  (let ((wnf (concatenate 'string (namestring scorefile) "temp")) (c 0))
    (om-copy-file scorefile wnf :if-exists :rename-and-delete)
    (resum-file wnf outfile 0 (1- lig-i))
    (while (and (<= c (- lig-f lig-i))
                (write-ds-file (chge-val op scal col (string-to-liste (lire-line-n wnf (+ 1 c lig-i)))) outfile))
      (incf c))
    (while (resum-file wnf outfile (+ c lig-i)(+ c lig-i))
      (incf c)) 
    (delete-file wnf)
    outfile))


(om::defmethod! change-col ((op symbol) (val number) col (lb number) (le number))  
  :icon 149
  (change-col-in-file nil nil op val col lb le))


;===============================
; New box OM2Csound 2.0 (2010)
;===============================

(om::defmethod! change-col-in-file (file newfile (op  symbol) (val number) col (lb number) (le number))  
  :icon 149
  :indoc '("a file pathname" "output file pathname" "a function or function name" "value to apply with <op>" "column number" "begin line number" "ending line number")
  :initvals '(nil nil * 1 1 1 1)
  :doc  "Apply <op> with <val> on every <col>th column between lines <lb> and <le> in <file>."
  (if (and file (not (probe-file file)))
      (om-beep-msg (string+ "ERROR!! File not found : " (namestring filename)))
    (let* ((inf (or file (om-choose-file-dialog :button-string "Choose a file to process")))
           (outf (and inf (or newfile (om-choose-new-file-dialog :button-string "Choose name and destination for the new file")))))
      (when (and inf outf)
        (chge-col inf outf op val col lb le)))))



;;;================================================
;;; EXIST IN OM
#|
(defun string-to-number (string)
  (base10 (om- (mapcar 'char-code (coerce string 'list)) 48)))

(defun base10 (liste)
  (let ((number 0)
        (signe 1)
        (exptmx (expt-max liste)))
    (if (= -3 (car liste))
      (setf signe -1)())
    (setf liste (remove -3 (remove -2 liste)))
    (dotimes (n (length liste) (if (ratiop (* signe number))
                                 (float (* signe number))
                                 (* signe number)))
      (setf number (+ number (* (expt 10 (- exptmx n)) (nth n liste)))))))

(defun expt-max (liste)
  (let (n1 n2 n3)
    (dotimes (x (length liste)(- n2 n1))
      (cond
       ((and (not n1)(< -1 (nth x liste) 10))
        (setf n1 x n2 x))
       ((= -2 (nth x liste))
        (setf n3 0))
       ((and n1 (not n3) (< -1 (nth x liste) 10))
        (setf n2 x))
       (t 
        ())))))
|#

;;;===================================================
;;; OM2Csound
;;; Control of Csound sound synthesis from OpenMusic
;;;
;;; ORCHESTRA EDITOR WINDOW
;;;===================================================

(in-package :om)

(defclass csound-ins (OMPatchAbs) ()
   (:metaclass omstandardclass))
(defclass instr (csound-ins) ())
(defclass instrEditor (patchEditor)  ())
(defmethod get-editor-panel-class ((self instrEditor))  'instrPanel)
(defclass instrPanel (patchPanel) ())



;=====SAVE

(defun get-csound-lib ()
   (find-library "OM2Csound"))


(defmethod om-save ((self instr) &optional (values? nil))
   "Generation of code to save 'self'."
   (let ((boxes (mapcar #'(lambda (box) (omNG-save box values?)) (boxes self)))
         (connectiones (mk-connection-list (boxes self))))
     `(om-load-patch-instr ,(name self) ',boxes ',connectiones ,*om-version*)))

(defun om-load-patch-instr (name boxes connections &optional (version nil))
   "This function is called when you load a saved Abstraction patch."
   (let ((newpatch (make-instance 'instr :name name :icon (list 606 (get-csound-lib))))) 
     (setf (boxes newpatch) (mapcar #'(lambda (box) (eval box)) boxes))
     (setf (saved? newpatch) connections)
     (remk-connections (boxes newpatch) connections)
     (compile-patch newpatch)
     (when version
       (setf (omversion newpatch) version))
     newpatch))



;================================================================
;Pour afficher les sorties par defaut dans le patch

(defmethod initialize-instance :after ((self instr) &key) nil)
(defmethod get-editor-class ((self instr)) 'instrEditor)


(defmethod OpenEditorframe ((self instr))
  (or (editorframe self)
      (panel (open-new-RelationFrame self (name self) (get-elements self)))))


;POur mettre des modules et i/o ds un patch

(defmethod add-window-buttons  ((self instrPanel))
   (call-next-method)
   (om-add-subviews self 
    (om-make-view 'button-icon
       :iconID (list 156 (get-csound-lib))
       :position (om-make-point 60 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'assign-val)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "assign-val"))))))
     (om-make-view 'button-icon
       :iconID (list 152 (get-csound-lib))
       :position (om-make-point 90 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'out)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "out"))))))
     
     
   
     (om-make-view 'button-icon
       :iconID (list 154 (get-csound-lib))
       :position (om-make-point 120 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'inst)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "inst"))))))
     (om-make-view 'button-icon
       :iconID (list 148 (get-csound-lib))
       :position (om-make-point 150 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'plus)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "plus"))))))
     (om-make-view 'button-icon
       :iconID (list 146 (get-csound-lib))
       :position (om-make-point 180 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'minus)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "minus"))))))
     (om-make-view 'button-icon
       :iconID (list 147 (get-csound-lib))
       :position (om-make-point 210 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'multiply)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "multiply"))))))
     (om-make-view 'button-icon
       :iconID (list 145 (get-csound-lib))
       :position (om-make-point 240 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'divide)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "divide"))))))

     (om-make-view 'button-icon
       :iconID (list 142 (get-csound-lib))
       :position (om-make-point 270 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'oscil)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "oscil"))))))
     
     (om-make-view 'button-icon
       :iconID (list 143 (get-csound-lib))
       :position (om-make-point 300 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'line-seg)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "line-seg"))))))
     
     
     (om-make-view 'button-icon
       :iconID (list 155 (get-csound-lib))
       :position (om-make-point 330 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'linen)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "linen"))))))
     
     
     (om-make-view 'button-icon
       :iconID (list 164 (get-csound-lib))
       :position (om-make-point 360 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'phasor)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "phasor"))))))
     
     
     (om-make-view 'button-icon
       :iconID (list 144 (get-csound-lib))
       :position (om-make-point 390 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'tablei)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "tablei"))))))
     
     
     (om-make-view 'button-icon
       :iconID (list 159 (get-csound-lib))
       :position (om-make-point 420 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'rand)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "rand"))))))
     
     
     (om-make-view 'button-icon
       :iconID (list 161 (get-csound-lib))
       :position (om-make-point 450 5)
       :size (om-make-point 24 24)
       :action
       #'(lambda(item) (declare (ignore item)) 
          (omG-add-element self
                           (make-frame-from-callobj 
                            (omNG-make-new-boxcall (fdefinition 'statement)
                                                   (om-make-point 50 80)
                                                   (mk-unique-name self "statement"))))))
     
     ))





(pushr 'instr *spec-new-boxes-types*)

;;; appele dans exit-from-dialog ((self new-fun-enter-view) newtext)
(defmethod get-new-box-from-type ((type (eql 'instr)) position container)
  (let ((newinstr (make-instance 'instr 
                    :name "myinstr" :icon (list 606 (get-csound-lib)))))
    (add-default-boxes newinstr)
    (omNG-make-new-boxcall 
     newinstr
     position 
     (mk-unique-name container "myinstr"))))

(defmethod add-default-boxes ((self instr))
  (let ((output (make-new-output "output" 0 (om-make-point 234 325)))
        (instr-output (omNG-make-new-boxcall (fdefinition 'inst)
                                             (om-make-point 236 250)
                                             "inst"))
        (out-output (omNG-make-new-boxcall (fdefinition 'out)
                                           (om-make-point 238 175)
                                           "out")))
    (omNG-add-element self output)
    (omNG-add-element self instr-output)
    (omNG-add-element self out-output)
    (omng-connect  instr-output 0 output 0 nil)
    (omng-connect  out-output 0 instr-output 2 nil)))
  







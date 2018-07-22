;;;===================================================
;;; OM2Csound
;;; Control of Csound sound synthesis from OpenMusic
;;;
;;; Preferences
;;; J. Bresson, IRCAM 2010
;;;
;;; Modified by Michael Gogins in July 2018 to enable
;;; Csound rendering to real-time audio.
;;;===================================================

(in-package :om)

(defvar *CSOUND-PATH* "path to Csound folder")

(pushr 'csound *external-prefs*)

(defvar *csound-defflags* "-f -m7 -N -g -b8192 -B8192")  ;; "-m7 -A -e -b8192 -B8192 -V50 -P128"
;; Can be set to any standard operating system output audio device name.
(defvar *csound-dac* "dac")

(defmethod get-external-name ((module (eql 'csound))) "Csound")
(defmethod get-external-icon ((module (eql 'csound))) (and (find-library "OM2Csound") (list 606 (find-library "OM2Csound"))))

(defmethod get-external-module-vals ((module (eql 'csound)) modulepref) (get-pref modulepref :csound-options))
(defmethod get-external-module-path ((module (eql 'csound)) modulepref) (get-pref modulepref :csound-path))
(defmethod set-external-module-vals ((module (eql 'csound)) modulepref vals) (set-pref modulepref :csound-options vals))
(defmethod set-external-module-path ((module (eql 'csound)) modulepref path) 
(set-pref modulepref :csound-path path))

(defun def-csound-options () (list "-f -m7 -A -N -g -b8192 -B8192" "dac"))

(defmethod get-external-def-vals ((module (eql 'csound))) 
  `(:csound-path ,(pathname "/usr/local/bin/csound")
    :csound-options ,(def-csound-options)))

(defmethod save-external-prefs ((module (eql 'csound))) 
  `(:csound-path ,(om-save-pathname *CSOUND-PATH*) 
    :csound-options (list ,*csound-defflags* ,*csound-dac*)))

(defmethod put-external-preferences ((module (eql 'csound)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :csound-options)))
  (print list-prefs)
    (when list-prefs 
      (setf *csound-defflags* (nth 0 list-prefs)
            *csound-dac* (nth 1 list-prefs)))
    (when (get-pref moduleprefs :csound-path)
      (setf *CSOUND-PATH* (find-true-external (get-pref moduleprefs :csound-path))))
    t))
      
(put-external-preferences 'csound (find-pref-module :externals))

;;;===========================
;;; INTERFACE

(defmethod show-external-prefs-dialog ((module (eql 'csound)) prefvals)
  (let* ((rep-list (copy-list prefvals))
         (dialog (om-make-window 'om-dialog
                                 :window-title "Csound Options"
                                 :size (om-make-point 300 200)
                                 :position :centered
                                 :resizable nil :maximize nil :close nil))
         (pos 20)
         (flaglabel (om-make-dialog-item 'om-static-text  
                                         (om-make-point 20 pos)  (om-make-point 147 17) "Csound default Flags"
                                         :font *om-default-font2*))
         (pos (+ pos 30))
         (defflagsline (om-make-dialog-item 'om-editable-text
                                             (om-make-point 24 pos)
                                             (om-make-point 240 20) (nth 0 prefvals)  
                                         :font *om-default-font2*))
         (pos (+ pos 40))
         (daclabel (om-make-dialog-item 'om-static-text  
                                         (om-make-point 20 pos)  (om-make-point 147 17) "Audio output device for \"dac\""
                                         :font *om-default-font2*))
         (pos (+ pos 30))
         (dacline (om-make-dialog-item 'om-editable-text
                                             (om-make-point 24 pos)
                                             (om-make-point 240 20) (nth 1 prefvals)  
                                         :font *om-default-font2*))
          (pos (+ pos 40))
         )
    (om-add-subviews dialog
                     flaglabel defflagsline 
                     daclabel dacline
      (om-make-dialog-item 'om-button (om-make-point 20 pos) (om-make-point 80 20) "Restore"
                           :di-action (om-dialog-item-act item
                                        (om-set-dialog-item-text defflagsline (nth 0 (def-csound-options)))
                                        (om-set-dialog-item-text dacline (nth 1 (def-csound-options)))
                                        ))
      
      (om-make-dialog-item 'om-button (om-make-point 130 pos) (om-make-point 80 20) "Cancel"
                           :di-action (om-dialog-item-act item
                                        (om-return-from-modal-dialog dialog nil)))
      
      (om-make-dialog-item 'om-button (om-make-point 210 pos) (om-make-point 80 20) "OK"
                           :di-action (om-dialog-item-act item
                                        (setf (nth 0 rep-list) (om-dialog-item-text defflagsline))
                                        (setf (nth 1 rep-list) (om-dialog-item-text dacline))
                                        (om-return-from-modal-dialog dialog rep-list))
                           :default-button t :focus t)
    )
    (om-modal-dialog dialog)))



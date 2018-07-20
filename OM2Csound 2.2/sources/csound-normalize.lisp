;;;===================================================
;;; OM2Csound
;;; Control of Csound sound synthesis from OpenMusic
;;; www.csounds.com
;;;
;;; CSOUND Normalize module
;;; J. Bresson, K. Haddad, M. Stroppa IRCAM 2010
;;;===================================================

(in-package :om)

 
(defmethod! csound-normalize ((file pathname) &optional outfile gain resolution)
 :icon '(287)
 :indoc '("input file" "ouput file pathname" "level (dB))" "resolution (n bits)")
 :outdoc '("normalized sound pathname")
 :doc "Normalizes an input sound file."
 (when (probe-file file)
   (let ((snd (load-sound-file file)))
     (when snd 
       (csound-normalize snd outfile gain resolution)))))
            
(defmethod! csound-normalize ((file pathname) &optional outfile gain resolution)
 (when (probe-file file)
   (let ((snd (load-sound-file (pathname file))))
     (when snd 
       (csound-normalize snd outfile gain resolution)))))


(defmethod! csound-normalize ((snd sound) &optional outfile gain resolution)
   (let ((out (cond ((pathnamep outfile) outfile)
                    ((stringp outfile) (outfile outfile))
                    (t (om-choose-new-file-dialog :directory (outfile nil)))))
         (res (if (and resolution (numberp resolution)) resolution))
         (valdb (or gain (if (numberp *normalize-level*) *normalize-level* 0.0))))
     
     ;;; RESOLUTION PRIORITY
     ;;; 1 - RES supplied in normalize args
     ;;; 2 - INPUT SOUND RESOLUTION
     ;;; 3 - PREF RESOLUTION
     ;;; 4 - 16 bits
     
     (unless res
       (setf res (or *audio-res*
                     (om-sound-sample-size snd)
                     16)))
 
     (when (and res (not (member res '(16 24 32))))
       (print "WARNING: Csound normalize can only output 16, 24 or 32 bits/sample audio")
       (setf res 16))

     (when out
       (setf out (handle-new-file-exists out))
       (let ((nchannels (om-sound-n-channels snd))
             (sr (om-sound-sample-rate snd))
             (format (om-sound-format snd)))
         
         (om-print "======================================")
         (om-print " CSOUND NORMALIZATION...")
         (om-print "======================================")
         
         (cond ((and (numberp nchannels) (member nchannels '(1 2 4 6 8 16 32)))
                (print (format nil "Normalize ~D to ~D dB and converting to ~D bits" (om-path2cmdpath (om-sound-file-name snd)) valdb res))
                (let ((csdfile (om-make-pathname :directory (append (pathname-directory (lib-resources-folder (find-library "OM2Csound"))) '("cs")) 
                                                 :host (pathname-host (lib-resources-folder (find-library "OM2Csound")))
                                                 :device (pathname-device (lib-resources-folder (find-library "OM2Csound")))
                                                 :name "normalize"
                                                 :type "csd")))
                  (om-cmd-line 
                    (format nil "~s -~A -~A -g --omacro:GAIN=~D --omacro:RES=~D --omacro:NCH=~D -r~D -k~D -i ~s -o ~s ~s"
                                  (om-path2cmdpath *CSOUND-PATH*)
                                  (case res (16 "s") (24 "3") (32 "l"))
                                  (if (string-equal "AIFF" (subseq format 0 4)) "A" "W")
                                  valdb
                                  (case res (16 32767) (24 8388607) (32 2147483647))
                                  nchannels sr sr
                                  (om-path2cmdpath (om-sound-file-name snd))
                                  (om-path2cmdpath out)
                                  (om-path2cmdpath csdfile)
                                  )
                   *sys-console*)
                  (setf out (probe-file out))
                  ))
                (t (om-beep-msg "ERROR : Csound normalize only supports 1, 2, 4, 8, 16 or 32 channels files.")
                   (setf out nil)))
               )
         out)))



(defmethod general-normalize ((norm (eql :csound)) tmppath outpath val &optional resolution)
  (csound-normalize tmppath outpath val resolution))

(unless (find :csound *loaded-normalizers*)
  (pushr :csound *loaded-normalizers*))

(defmethod get-def-normalize-value ((self (eql :csound))) 0.0)
(defmethod get-module-name ((self (eql :csound))) "Csound")



;;;===================================================
;;;
;;; OM2Csound
;;; Control of Csound sound synthesis from OpenMusic
;;;
;;; see www.csounds.com
;;;
;;; UTILITIES
;;; Authors/Contributors: Laurent Pottier, Karim Haddad, Jean Bresson 
;;; (c) IRCAM 1993-2010
;;;===================================================

(in-package :om)

;;; Compat OM2CSound 1

(defmethod! bpf-xy-intpol (bpf1 bpf2 n curve)
  :numouts 2
  :icon '(213)
  (multiple-value-bind (lbpf lx ly)
      (bpf-interpol bpf1 bpf2 n curve nil 'sample)
    (values (car lx) ly)))

(defmethod! bpf-intpol (bpf1 bpf2 n curve)
  (bpf-interpol bpf1 bpf2 n curve nil 'sample))

(defmethod! list-interpol (l1 l2 n curve)
  (interpolation l1 l2 n curve nil))

(defmethod! lin-list (init end steps)
  (interpolation init end steps 0))        

(defun rang? (liste elem &optional (pred 'eq )) 
  (rang-p list elem pred))




;;; BPF SAMPLER

(defmethod! sampler ((bpf bpf)
                          (mode symbol)
                          (nsamp number)
                          xinit
                          xend
                          (oper number)
                          (ndec number))
  :icon 139
  :indoc '("bpf" "mode" "nsamp" "xmin" "xmax" "oper" "ndec")
  :initvals (list (make-instance 'bpf) 'fact 1 nil nil 1 0)
  :menuins '((1 (("fact" 'fact) 
                 ("max" 'max) 
                 ("sum" 'sum))))
  :doc "A bpf sampler

       <bpf>   input from a table BPF
       <mode>  xfact, max, sum
       <nsamp> number of samples ( echantillons) to be obtained
       <xmin> minimum value of x in the table (specified by x-off)
       <xmax>  maximum value of x (specified by xzoom)
       <oper>  factor multiplying the ordinates
       <ndec>  number of decimals in the samples

      xfact mode = samples a bpf with a scaling factor <oper> multiplying the ordinates
      max   mode = samples a bpf between O and  maximum value <oper> of the ordinates
      sum   mode = samples a bpf with a fixed sum of values,
                   <oper> being the maximum value for the sum of the ordinates
" 
  (let* ((sampled-values (third (multiple-value-list (om-sample bpf nsamp xinit xend ndec)))))
    (cond 
     ((eq mode 'xfact) (om::om-round (om::om* sampled-values oper) ndec))
     ((eq mode 'max) (om::om-round (om-scale/max sampled-values oper) ndec))
     ((eq mode 'sum) (om::om-round (om::om-scale/sum sampled-values oper) ndec)))))



;===========================================================================================
;;; SUPPRIMER ?

(defmethod get-bpf-sample-output ((self null) echan1 xinit2 xend3 fact4 nbdec5)
  (declare (ignore echan1 xinit2 xend3 fact4 nbdec5)) self)


(defmethod get-bpf-sample-output ((self bpf) echan1 xinit2 xend3 fact4 nbdec5)
  (om::om-round 
   (om::om* (transfer self (om::arithm-ser xinit2 xend3 (/ (- xend3 xinit2) (- echan1 '1)))) fact4) nbdec5))

(defmethod get-bpf-sample-output ((self cons) echan1 xinit2 xend3 fact4 nbdec5)
  (mapcar #'(lambda (bpf) (get-bpf-sample-output bpf echan1 xinit2 xend3 fact4 nbdec5))
          self))

(defmethod get-bpf-sample-output ((self number) echan1 xinit2 xend3 fact4 nbdec5)
  (declare (ignore echan1 xinit2 xend3 fact4 nbdec5)) self)


(defun bpf-out (bpf time times &optional float-fl)
  (let ((last-time) (last-value)
        (values (y-points bpf))
        res)
    (unless times (setq times (om::x-points bpf)))
    (if (not (>= time (car times)))
       (car times)
       (progn 
           (while (and times (>= time (car times))) 
           (setq last-time (pop times)) (setq last-value (pop values)))
           (setq res
             (if (not times)
               last-value
               (linear-interpol last-time (car times) last-value (car values) time)))
          (if (not float-fl) (round res) res)))))



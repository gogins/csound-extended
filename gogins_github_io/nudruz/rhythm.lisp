(in-package :cm)

;; CYC-RHYTHMS -- combines measures and subdivisions cyclically
(defun cyc-rhythms (mlens subdivs)
  (let* ((subdivcyc (new cycle of subdivs))
	 (mlencyc (new cycle of mlens)))
    (loop until (and (eop? subdivcyc)
		     (eop? mlencyc))
      for this-subdiv = (next subdivcyc)
      for this-mlen = (next mlencyc)
      append (loop repeat this-subdiv
	       collect (float (/ this-mlen this-subdiv))))))

;; (cyc-rhythms '(2 3 1.5) '(1 2))
;; = (2.0 1.5 1.5 1.5 1.0 1.0 3.0 0.75 0.75)

;; COMBINE-ATKS -- 'fuses' attacks within a rhythm 
;; 'combiner' = number of attacks to combine (cycle)

(defun combine-atks (rhythms atklist)
  (let ((rcyc (new cycle of rhythms))
	(atkcyc (new cycle of atklist)))
    (loop until (and (eop? rcyc) (eop? atkcyc))
      collect (apply #'+ (next rcyc (next atkcyc))))))

;; (combine-atks '(1 2) '(1 1 3))
;; = (1 2 4 2 1 5)

;; UPBEATCYC -- returns upbeat cycle 
;; between 'minups' and 'maxups' upbeats of 1
;; between 'mindown' and 'maxdown' length of downbeat

(defun upbeatcyc (minups maxups mindown maxdown)
  (new cm::random :of `((1 :min ,minups :max ,maxups) 
		    (,(pval (between mindown maxdown)) :max 1))))

(next (upbeatcyc 2 5 10 20) 20)


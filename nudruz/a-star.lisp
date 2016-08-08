;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-

; (in-package :User)
(in-package :cm)

;;;==========================================================================
;;;==========================================================================
;;;==========================================================================
;;; 11/90 Marty Hall [Many missing optimizations -- MRH '94]
;;; Defines a generic version of the A* search routines.
;;; It is generic in the sense that it should work for any application domain where
;;; you have separately defined
;;;    A) The Start position
;;;    B) The Goal position (or NIL if you define an appropriate Goal-Function)
;;;    C) A Goal-Function that tests if a position is a goal. This defaults to
;;;       testing if the position is EQUAL to the Goal position.
;;;    D) A Child-Generator-Function that, given a position, generates all the
;;;       positions that can be reached in one step.
;;;    E) An H-Function that gives an estimate of the distance to the goal.
;;;       This implementation assumes that the H-Function is also a legitimate
;;;       distance function -- ie for moves of one step it correctly measures the
;;;       distance.  Note the requirement that the H-Function be
;;;       non-overestimating, otherwise A* might find a non-optimal solution.
;
;;; Note that the application-domain routines that use A* do NOT need to know anything
;;; about how nodes are represented or even how A* works at all.
;
;;; Eg, assume that you have a function 'Generate-Child-Words' that, given a
;;; string such as "love" finds all legal English words one letter different
;;; (lone, live, dove, etc). Also assume that you have a function called
;;; 'Word-Difference' that, for two words of the same length, tells how many
;;; letters fail to match exactly (eg 2 for "love" and "dive", 1 for "love" and "dove").
;;; Now (A* "love" "hate"
;;;         :Child-Generator-Function 'Generate-Child-Words
;;;         :H-Function 'Word-Difference)
;
;;; will use A* to find a word ladder between "love" and "hate". Using the
;;; Symbolics dictionary, it gives (love cove cave have hate).
;;;==========================================================================
;;;==========================================================================
;;;==========================================================================

;;;==========================================================================

(defvar *Open*)
(defvar *Closed*)

;;;==========================================================================
;;; Top-Level A* routine. Marty Hall.


(defun A* (Start Goal &key (Goal-Function #'equal)
	                   (Child-Generator-Function #'Generate-Children)
		           (H-Function #'Heuristic-Estimate)
		      )
  (let ((Current-Node))
    (setq *Open* (list (Make-Initial-Node Start)))
    (setq *Closed* '() )
    (loop
      (if
	(endp *Open*)
	(return (Failure)) )
      (setq Current-Node (First-Open-Node))
      (push Current-Node *Closed*)
      (when
	(Goal? Goal-Function Current-Node Goal)
	(return (Show-Solution Current-Node)))
      (Add-Children-to-Open Current-Node
			    Child-Generator-Function
			    H-Function
			    Goal)
      (Sort-Open-List)
    )
))

;;;==========================================================================

(defun Make-Initial-Node (Entry)
  (list (list Entry nil) '(0 nil))
)

;;;==========================================================================

(defun Make-Node (Entry G-Value Parent H-Function Goal)
  (list (list Entry Parent)
	(list (+ (funcall H-Function Parent Entry) G-Value)
	      (funcall H-Function Entry Goal)) )
)

;;;==========================================================================

(defun Goal? (Test Node Goal-Entry)
  (funcall Test (Entry Node) Goal-Entry)
)

;;;==========================================================================

(defun Failure ()
;  (format t "~%No solution exists!~%")
  (values)
)

;;;==========================================================================
;;; Takes the first node (ie one with the best f value) off the OPEN list, and then
;;; removes all other nodes that get to that same position (entry) by a different route.
;;; Marty Hall

(defun First-Open-Node ()
  (let ((Node (pop *Open*)))
    (setq *Open* (delete (Entry Node) *Open* :key #'Entry :test #'equal))
    Node
))

;;;==========================================================================

(defun Sort-Open-List ()
  (setq *Open* (sort *Open* #'Node-Less-Than?))		      
)

;;;==========================================================================
;;; Should really break ties by choosing the larger G.

(defun Node-Less-Than? (Node1 Node2)
  (< (Node-F-Value Node1) (Node-F-Value Node2))
)

;;;==========================================================================
;;; F value is G+H.  Marty Hall.

(defun Node-F-Value (Node)
  (+ (G-Value Node) (H-Value Node))
)

;;;==========================================================================
;;; A node is represented as a list: ((Current-Entry Parent-Entry) (G-Value H-Value).
;;; Marty Hall

(defun G-Value (Node)
  (first (second Node))
)

;;;==========================================================================
;;; A node is represented as a list: ((Current-Entry Parent-Entry) (G-Value H-Value).
;;; Marty Hall

(defun H-Value (Node)
  (second (second Node))
)

;;;==========================================================================
;;; A node is represented as a list: ((Current-Entry Parent-Entry) (G-Value H-Value).
;;; Marty Hall

(defun Entry (Node)
  (first (first Node))
)

;;;==========================================================================
;;; A node is represented as a list: ((Current-Entry Parent-Entry) (G-Value H-Value)).
;;; Marty Hall

(defun Parent (Node)
  (second (first Node))
)

;;;==========================================================================
;;; Generates children of a node, and puts all that are NOT on ClOSED list onto OPEN list.
;;; Marty Hall.
;;; Obviously, a hash table for Closed would be a LOT faster.

(defun Add-Children-to-Open (Node Child-Generator-Function H-Function Goal)
  (let ((G (G-Value Node))
	(Entry (Entry Node))
	(Child-Nodes '())    )
    (dolist (Child (funcall Child-Generator-Function Entry))
      (unless (member Child *Closed* :key #'Entry :test #'equal)
	(push (Make-Node Child G Entry H-Function Goal) Child-Nodes) )  )
    (setq *Open* (nconc Child-Nodes *Open*))
))

;;;==========================================================================
;;; Recursive routine to trace back through CLOSED list for entire solution path ending
;;; at Node. Ie finds the parent of Node, its parent, and so on back to the starting
;;; node, for which there is no parent.
;;; Marty Hall.

(defun Show-Solution (Node)
  (let ((Entry (Entry Node))
	(Parent (Parent Node))  )
    (if
      Parent
      (append (Show-Solution (first (member Parent *Closed* :key 'Entry)))
	      (list Entry))
      (list Entry))
))

;;;==========================================================================
;;; Debugging purposes only.
;;; Marty Hall

(defun Describe-Solution (Node)
  (format t "~%Solution node=~S, with a G value of ~D." Node (Value Node))
)

;;;==========================================================================

(defun A*-Nodes-Stored ()
  (+ (length *Open*) (length *Closed*))
)

;;;==========================================================================

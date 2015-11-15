(defun qusort (L)
	(cond
		((null L) nil)
		(t 
			(append
				(qusort (slist (car L) (cdr L)))
				(cons (car L) nil)
				(qusort (llist (car L) (cdr L)))))))
	
(defun llist (a b)
	(cond
		((or (null a)(null b)) nil)
		((= 1 (compare a (car b)))(llist a (cdr b)))
		(t(cons (car b)(llist a (cdr b))))))
	
(defun slist (a b)
	(cond
		((or (null a)(null b)) nil)
		((>= 0 (compare a (car b)))(slist a (cdr b)))
		(t(cons (car b)(slist a (cdr b))))))
(defun quicksort (L)
	(cond
		((null L) nil)
		(t 
			(append
				(quicksort (leftlist (car L) (cdr L)))
				(cons (car L) nil)
				(quicksort (rightlist (car L) (cdr L)))))))
	
(defun rightlist (a b)
	(cond
		((or (null a)(null b)) nil)
		((= 1 (compare a (car b)))(rightlist a (cdr b)))
		(t(cons (car b)(rightlist a (cdr b))))))
	
(defun leftlist (a b)
	(cond
		((or (null a)(null b)) nil)
		((>= 0 (compare a (car b)))(leftlist a (cdr b)))
		(t(cons (car b)(leftlist a (cdr b))))))
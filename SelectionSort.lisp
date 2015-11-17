(defun selectionsort (L)
	(if (null L)
		(let (a (findmin a L))
		(cons a
			(selectionsort (remove a L))))))
			
(defun findmin (m L)
	(cond ((null (car L)) m)
        ((= 1 (compare (car L) m)) (findmin (car L) (cdr L)))
        (t (findmin m (cdr L)))))
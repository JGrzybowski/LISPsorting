(defun selectionsort (L)
	(if (null L) 
		L
		(let ((a (findmin L)))
			(cons a (selectionsort (remove a L)))
		)
	)
)
			
(defun findmin (L &aux mini)
	(setq mini (car L))
	(loop 
		for x in (cdr L)
		do (
			if (= -1 (compare x mini)) (setq mini x)
		)
	)
	mini
)

(defmacro selectionsort-macro (L)
	(let ((L-name (gensym)) )
		`(let ((,L-name ,L))
			(if (null ,L-name) 
				,L-name
				(let ((a (findmin ,L-name)))
					(cons a (selectionsort-macro (remove a ,L-name))))))))
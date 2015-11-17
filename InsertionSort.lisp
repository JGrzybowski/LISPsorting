(defun insert (a L)
	(if (null L)
		(list a)
		(if (>= 0 (compare a (car L)))
			(cons a L)
			(cons (car L) (insert a (cdr L))))))
			

(defun insertionSort (L)
	(if (null L)
		nil
		(insert (car L) (insertionSort (cdr L)))))
	
	
(defmacro insert-macro (a L)
	(let ((a-name (gensym)) (L-name (gensym)))
		`(let ((,a-name ,a) (,L-name ,L))
			(if (null ,L-name)
				(list ,a-name)
				(if (>= 0 (compare ,a-name (car ,L-name)))
					(cons ,a-name ,L-name)
					(cons (car ,L-name) (insert ,a-name (cdr ,L-name)))))
		)))

(defmacro insertionSort-macro (L)
	(let ((L-name (gensym)))
		`(let ((,L-name ,L))
			(if (null ,L-name)
				nil
				(insert-macro (car ,L-name) (insertionSort-macro (cdr ,L-name))))
		)))
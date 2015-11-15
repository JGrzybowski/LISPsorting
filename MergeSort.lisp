(defun mergesort (sequence)
	(let ((len (length sequence)))
		;(format T "Sorting ~a[~a].~%" sequence len)
		(if (= len 1) 
			sequence
			(own-merge 
				(mergesort (subseq sequence 0 (truncate len 2)) )
				(mergesort (subseq sequence (truncate len 2)) ) 
			)
		)
	)	
)

(defmacro mergesort-macro (sequence)
	(let ((sequence-var-name (gensym)) (length-var-name (gensym)))
		`(let*  ( (,sequence-var-name ,sequence) (,length-var-name (length ,sequence-var-name)))
			(if (= ,length-var-name 1)
				,sequence-var-name
				(own-merge 
					(mergesort-macro (subseq ,sequence-var-name 0 (truncate ,length-var-name 2)) )
					(mergesort-macro (subseq ,sequence-var-name (truncate ,length-var-name 2)) ) 					
				)		
			)
		)		
	)
)

(defun own-merge (listA listB &aux result)
	(loop
		while (and listA listB)
		do	(let ((compareResult (compare (car listA) (car listB))))
				;(format T "Merging ~a ~a (comparison=~a) ~%" listA listB compareResult)
				
				(when (< -1 compareResult) 
					(setq result (append result (list (car listB))))
					(setq listB (cdr listB))
				)	
				;(if (< -1 compareResult) (format T "After B ~a ~a (comparison=~a) ~%" listA listB compareResult))
				
				(when (> 1 compareResult) 
					(setq result (append result (list (car listA))))
					(setq listA (cdr listA))
				)	
				;(if (> 1 compareResult) (format T "After A ~a ~a (comparison=~a) ~%" listA listB compareResult))
			)
	)
	(loop until (null listA) 
		do
		(setq result (append result (list (car listA))))
		(setq listA (cdr listA))
	)
	(loop until (null listB)
		do
		(setq result (append result (list (car listB))))
		(setq listB (cdr listB))		
	)
	;(format T " -> ~a.~%" result)
	result	
)


(defun append-and-cut (sequence appended)
	(values-list (list (append sequence(list (car appended))) (cdr appended) ) )
)
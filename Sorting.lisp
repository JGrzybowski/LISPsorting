(defun subCompare (x y typePredicate areEqual isLessThan)
	"Sum any two numbers after printing a message."
	;(format t "Comparing ~d and ~d. ~%" x y)
	(if (apply typePredicate (list x))
		(if (apply typePredicate (list y))
			(cond 
				((apply areEqual (list x y)) 0)
				((apply isLessThan (list x y)) -1)
				(T 1)
			)
			-1
		)
		(if (apply typePredicate (list y)) 1 NIL)
	)
)

(defun listCompare (listA listB)
	(case (compare (car listA) (car listB))
		( 0 (compare (cdr listA) (cdr listB)) )	
		( 1 1)
		( -1 -1)
		(otherwise NIL)
	)
)

(defun compare (x y)
	"Universal comaparer. Returns -1 if x is smaller than y, 0 if they are equal and 1 if y is smaller."
	(cond
		((and (null x) (null y)) 0)
		((null x) -1)
		((null y) 1)
		((subCompare x y #'numberp #'eql #'<))
		((subCompare x y #'characterp #'CHAR= #'CHAR<))
		((subCompare x y #'stringp #'STRING= #'STRING<))
		((listCompare x y))
		(T (format t "Can't compare ~d and ~d.~%" x y))
	)
)

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

(defun own-merge (listA listB &aux result)
	(loop
		while (and listA listB)
		do	(let ((compareResult (compare (car listA) (car listB))))
				;(format T "Merging ~a ~a (comparison=~a) ~%" listA listB compareResult)
				
				(if (< -1 compareResult) (setq result (append result (list (car listB)))))
				(if (< -1 compareResult) (setq listB (cdr listB)))	
				;(if (< -1 compareResult) (format T "After B ~a ~a (comparison=~a) ~%" listA listB compareResult))
				
				(if (> 1 compareResult) (setq result (append result (list (car listA)))))
				(if (> 1 compareResult) (setq listA (cdr listA)))	
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
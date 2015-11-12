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

(defun compare (x y)
	"Universal comaparer. Returns -1 if x is smaller than y, 0 if they are equal and 1 if y is smaller."
	(cond
		((null x) -1)
		((null y) 1)
		((subCompare x y #'numberp #'eql #'<))
		((subCompare x y #'characterp #'CHAR= #'CHAR<))
		((subCompare x y #'stringp #'STRING= #'STRING<))
		
		(T (format t "Can't compare ~d and ~d.~%" x y))
	)
)
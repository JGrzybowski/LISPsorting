(defun bubble(sequence)
	(if (= 1 (length sequence)) 
		sequence
		(progn 
			(if (eql 1 (compare (car sequence) (cadr sequence)))
				(rotatef (car sequence) (cadr sequence))
			)
			(bubble (cdr sequence))
		)
	)
)

(defun bubblesort (sequence &aux sorted)
	(loop for i from 1 to (length sequence)
	do ( 
	progn
		(format T "Sorted: ~a ToSort:~a ~%" sorted sequence)
		(setq sorted (append (bubble sequence) sorted))
		(setq sequence (butlast sequence))
		;(format T "Sorted: ~a ToSort:~a ~%" sorted sequence)
	)
	)
	sorted
)
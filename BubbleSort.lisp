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

(defmacro bubble-macro (sequence swapValue)
	(let ((seq-name (gensym)) (swap-val-name (gensym)))
		`(let ((,seq-name ,sequence) (,swap-val-name ,swapValue))
			(if (= 1 (length ,seq-name))
				,seq-name
				(progn 
					(if (= ,swap-val-name (compare (car ,seq-name) (cadr ,seq-name)))
						(rotatef (car ,seq-name) (cadr ,seq-name))
					)
					(bubble (cdr ,seq-name))
				)
			)
		)
	)	
)

(defun bubblesort (sequence &aux sorted)
	(loop for i from 1 to (length sequence)
	do ( 
	progn
		;(format T "Sorted: ~a ToSort:~a ~%" sorted sequence)
		(setq sorted (append (bubble sequence) sorted))
		(setq sequence (butlast sequence))
		;(format T "Sorted: ~a ToSort:~a ~%" sorted sequence)
	)
	)
	sorted
)

(defun bubblesort-with-macro (sequence &aux sorted)
	(loop for i from 1 to (length sequence)
	do ( 
	progn
		;(format T "Sorted: ~a ToSort:~a ~%" sorted sequence)
		(setq sorted (append (bubble-macro sequence 1) sorted))
		(setq sequence (butlast sequence))
		;(format T "Sorted: ~a ToSort:~a ~%" sorted sequence)
	)
	)
	sorted
)
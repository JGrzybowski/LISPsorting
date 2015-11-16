(defun heapsort (sequence)
	(setq sequence (buildheap sequence))
	(shift-heap sequence)
)


(defun LIndex (n) (+ (* n 2) 1) )
(defun RIndex (n) (+ (* n 2) 2) )
(defun PIndex (n) (truncate (- n 1) 2))

(defun buildheap (sequence)
	(let ((len (length sequence)))
		(loop for i from (- (truncate len 2) 1) downto 0
		do
			(setq sequence (shift-down sequence i))			
		)
	)
	sequence
)

(defun shift-down (sequence i)
	(let ((maxIdx (- (length sequence) 1)) (LIdx (LIndex i)) (RIdx (RIndex i)))
		(cond 
		((>= maxIdx RIdx)
			(let ((P (nth i sequence)) (L (nth (LIndex i) sequence)) (R (nth (RIndex i) sequence))  )
				;(format T "P:~a L:~a R:~a ~%" P L R)
				(cond
					((when (and (eql -1 (compare R P)) (>= 0 (compare R L))) 
						(rotatef (nth  i sequence) (nth  RIdx sequence))
						(setq sequence (shift-down  sequence RIdx))
					)) 
					((when (and (eql -1 (compare L P)) (>= 0 (compare L R)))
						(rotatef (nth  i sequence) (nth  LIdx sequence))
						(setq sequence (shift-down sequence LIdx ))
					))
				)
			)
		)
		((>= maxIdx LIdx)
			(let ((P (nth i sequence)) (L (nth (LIndex i) sequence)))
				;(format T "P:~a L:~a ~%" P L)
				(when (eql -1 (compare L P)) 
						(rotatef (nth  i sequence) (nth  Lidx sequence))
						(setq sequence (shift-down  sequence LIdx))
				) 
			)
		)
		)
	)
	sequence
)


(defun shift-heap (heap)
	(when (< 1 (length heap))
		(rotatef (car heap) (car (last heap)))
		(setq heap 
			(append  
				(last heap) 
				(shift-heap (shift-down (butlast heap) 0)) 
			)
		)
	)
	heap
)

(defmacro shift-heap-macro (heap)
	(let ((heap-name (gensym)))
		`(let ((,heap-name ,heap))
			(when (< 1 (length ,heap-name))
				(rotatef (car ,heap-name) (car (last ,heap-name)))
				(setq ,heap-name
					(append  
						(last ,heap-name) 
						(shift-heap (shift-down (butlast ,heap-name) 0)) 
					)
				)
			)
			,heap-name
		)
	)
)

(defun heapsort-with-macro (sequence)
	(setq sequence (buildheap sequence))
	(shift-heap-macro sequence)
)

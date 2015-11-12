(defun testCompare()
	"Unit Test of compare function."
	(load "Sorting.lisp")
	
	(singleTest 1 0 1 -1)
	(singleTest 2 0 0 0)
	(singleTest 3 1 0 1)
	
	
)

(defun singleTest (nTest x y result)
	"Single compare function Unit test."
	(if (eq result (compare x y))
		(format t "Unit test #~d [~d ~d => ~d] PASSED. ~%" nTest x y result)
		(format t "Unit test #~d [~d ~d => ~d] FAILED. ~%" nTest x y result)
	)
) 
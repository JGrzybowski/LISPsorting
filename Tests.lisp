(defun testAll()
	"Runs all unit tests."
	(load "Sorting.lisp")
	(test-compare)
	(test-own-merge 1 (list 1) (list 11) (list 1 11))
	(test-own-merge 2 (list 11) (list 1) (list 1 11))
	(test-own-merge 3 (list 1 10) (list 5 2) (list 1 5 2 10))
	(test-own-merge 4 (list 3 4) (list 1 2) (list 1 2 3 4))
	(test-sorting #'mergesort)
	(test-sorting #'bubblesort)
)

(defun test-compare()
	"Unit Test of compare function."
	(singleCompareTest 1 0 1 -1)
	(singleCompareTest 2 0 0 0)
	(singleCompareTest 3 1 0 1)
	
	(singleCompareTest 4 #\a #\b -1)
	(singleCompareTest 5 #\b #\b 0)
	(singleCompareTest 6 #\c #\b 1)
	
	(singleCompareTest 7 "abc" "bca" -1)
	(singleCompareTest 8 "abc" "abc" 0)
	(singleCompareTest 9 "abc" "aab" 1)
	(singleCompareTest 10 "ab" "abc" -1)
	(singleCompareTest 11 "abc" "ab " 1)
	
	(singleCompareTest 12 "" "a" -1)
	(singleCompareTest 13 "" "" 0)
	(singleCompareTest 14 "a" "" 1)
	
	(singleCompareTest 15 NIL 0 -1)
	(singleCompareTest 16 0 NIL 1)
	(singleCompareTest 17 NIL #\c -1)
	(singleCompareTest 18 #\c NIL 1)
	(singleCompareTest 19 NIL "abc" -1)
	(singleCompareTest 20 "abc" NIL 1)
	
	(singleCompareTest 21 (list 2 3) (list 2 4) -1)
	(singleCompareTest 22 (list 2 3) (list 2 3) 0)
	(singleCompareTest 23 (list 2 3) (list 1 4) 1)
	
	(singleCompareTest 24 (list (list 1 5)) (list (list 2 3) (list 2 4)) -1)
	(singleCompareTest 25 (list (list 1 2) (list 2 3)) (list (list 1 2)(list 2 3)) 0)
	(singleCompareTest 26 (list (list 1 2) (list 2 3)) (list (list 1 2)(list 2 1)) 1)

	(singleCompareTest 27 3 (list 2 4) -1)
	(singleCompareTest 28 #\a (list 2 4) -1)
	(singleCompareTest 29 "Abc" (list 2 3) -1)
	(singleCompareTest 30 (list 2 3) 1 1)
	(singleCompareTest 31 (list 2 3) #\c 1)
	(singleCompareTest 32 (list 2 3) "abc" 1)
)

(defun singleCompareTest (nTest x y result)
	"Single compare function Unit test."
	(if (eq result (compare x y))
		(format t "Unit test #~d PASSED. [~d ~d => ~d] ~%" nTest x y result)
		(format t "Unit test #~d FAILED. [~d ~d => ~d] ~%" nTest x y result)
	)
) 

(defun test-own-merge(nTest x y result)
	"Single own-merge function Unit test."
	(if (equal 0 (compare result (own-merge x y)))
		(format t "Unit test #~d PASSED. [~d ~d => ~d] ~%" nTest x y result)
		(format t "Unit test #~d FAILED. [~d ~d => ~d] ~%" nTest x y result)
	)
)

(defun test-sorting(sortingFunction)
	"Runs mergesort unit tests."
	;(format T "Testing ~a.~%" sortingFunction)
	(sortingTest 1 sortingFunction (list 4 3 2 1) (list 1 2 3 4) )
	(sortingTest 2 sortingFunction (list (list 4 2 1) (list 1 2 3) (list 2 1 3)) (list (list 1 2 3) (list 2 1 3) (list 4 2 1)))
)

(defun sortingTest (nTest sortingFunction argument expectedResult)
	"Single generic sorting unit test."
	;(format T "Single ~a test ~a => ~a).~%" sortingFunction argument expectedResult)
	(let* 
		(
			(sortingResult (apply sortingFunction (list argument)))
			(result (compare sortingResult expectedResult))
		)
		;(format T "Single test ~a => ~a [~a]).~%" argument expectedResult result)
		(if (equal result 0)
			(format t "Unit test #~a PASSED. ~% in:~a ~%out:~a ~%exp:~a  ~%~%" nTest argument sortingResult expectedResult)
			(format t "Unit test #~a FAILED. ~% in:~a ~%out:~a ~%exp:~a  ~%~%" nTest argument sortingResult expectedResult)
		)
	)
)
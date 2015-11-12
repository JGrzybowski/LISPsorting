(defun testAll()
	"Runs all unit tests."
	(load "Sorting.lisp")
	(testCompare)
	
)

(defun testCompare()
	"Unit Test of compare function."
	(singleTest 1 0 1 -1)
	(singleTest 2 0 0 0)
	(singleTest 3 1 0 1)
	
	(singleTest 4 #\a #\b -1)
	(singleTest 5 #\b #\b 0)
	(singleTest 6 #\c #\b 1)
	
	(singleTest 7 "abc" "bca" -1)
	(singleTest 8 "abc" "abc" 0)
	(singleTest 9 "abc" "aab" 1)
	(singleTest 10 "ab" "abc" -1)
	(singleTest 11 "abc" "ab " 1)
	
	(singleTest 12 "" "a" -1)
	(singleTest 13 "" "" 0)
	(singleTest 14 "a" "" 1)
	
	(singleTest 15 NIL 0 -1)
	(singleTest 16 0 NIL 1)
	(singleTest 17 NIL #\c -1)
	(singleTest 18 #\c NIL 1)
	(singleTest 19 NIL "abc" -1)
	(singleTest 20 "abc" NIL 1)
	
	(singleTest 21 (list 2 3) (list 2 4) -1)
	(singleTest 22 (list 2 3) (list 2 3) 0)
	(singleTest 23 (list 2 3) (list 1 4) 1)
	
	(singleTest 24 (list (list 1 5)) (list (list 2 3) (list 2 4)) -1)
	(singleTest 25 (list (list 1 2) (list 2 3)) (list (list 1 2)(list 2 3)) 0)
	(singleTest 26 (list (list 1 2) (list 2 3)) (list (list 1 2)(list 2 1)) 1)

	(singleTest 27 3 (list 2 4) -1)
	(singleTest 28 #\a (list 2 4) -1)
	(singleTest 29 "Abc" (list 2 3) -1)
	(singleTest 30 (list 2 3) 1 1)
	(singleTest 31 (list 2 3) #\c 1)
	(singleTest 32 (list 2 3) "abc" 1)
)

(defun singleTest (nTest x y result)
	"Single compare function Unit test."
	(if (eq result (compare x y))
		(format t "Unit test #~d [~d ~d => ~d] PASSED. ~%" nTest x y result)
		(format t "Unit test #~d [~d ~d => ~d] FAILED. ~%" nTest x y result)
	)
) 
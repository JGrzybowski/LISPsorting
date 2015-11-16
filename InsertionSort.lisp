(defun insert (a L)
  (if (null L)
    (cons a nil)
    (if (>= 0 (compare a (car L)))
      (cons a L)
      (cons (car L) (insert a (cdr L))))))

(defun insertionSort (L)
  (if (null L)
    nil
    (insert (car L) (insertionSort (cdr L)))))
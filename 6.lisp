(ql:quickload :str)

(defparameter *fishes* (uiop:read-file-line "input/day6.txt"))

(defun fish-array (fishes)
  (let ((fish-arr (make-array 9)))
    (mapcar (lambda (n)
              (incf (aref fish-arr n)))
            (mapcar #'parse-integer (str:split "," fishes)))
    fish-arr))

(defun after-days (days a b c d e f g h i)
  (dotimes (_ days)
    (rotatef a b c d e f g h i)
    (incf g i))
  (+ a b c d e f g h i))

(defun solve-problems ()
  (let ((fish-list (coerce (fish-array *fishes*) 'list)))
    (values (apply #'after-days 80 fish-list)
            (apply #'after-days 256 fish-list))))

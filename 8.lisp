(ql:quickload '("split-sequence" "str"))

(defparameter *signals* (uiop:read-file-lines "input/day8.txt"))

(defun split-signals (signals)
  (mapcar #'(lambda (s)
              (str:split-omit-nulls " | " s))
          signals))

(defun four-digit-outputs (splited-signals)
  (mapcar #'second splited-signals))

(defun unique-sig (sig)
  (let ((len (length sig)))
    (or (= 2 len)
        (= 3 len)
        (= 4 len)
        (= 7 len))))

(defun solve-first-part (signals)
  (loop for s in (four-digit-outputs (split-signals signals))
        sum (count t (mapcar #'unique-sig (str:split " " s)))))

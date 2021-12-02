(ql:quickload '("split-sequence" "alexandria"))

(defparameter *courses* (uiop:read-file-lines "input/day2.txt"))

(defparameter *dist* 0)
(defparameter *depth* 0)
(defparameter *aim* 0)

(defun reset ()
  (setf *dist* 0
        *depth* 0
        *aim* 0))

(defun parse-single-course (course)
  (let ((result (split-sequence:split-sequence #\Space course)))
    (cons (read-from-string (first result))
          (parse-integer (second result)))))

(defun parse-courses (courses)
  (mapcar #'parse-single-course courses))

(defun instr-to-pos (instr)
  (let ((command (car instr))
        (n (cdr instr)))
    (alexandria:eswitch (command)
      ('forward (incf *dist* n))
      ('down (incf *depth* n))
      ('up (decf *depth* n)))))

(defun run-courses (fn courses)
  (mapcar fn (parse-courses courses)))

(defun run (fn courses)
  (progn
    (run-courses fn courses)
    (print (* *dist* *depth*))
    (reset)))

;; Part I

(run #'instr-to-pos *courses*)

;; Part II

(defun instr-to-pos-ver-2 (instr)
  (let ((command (car instr))
        (n (cdr instr)))
    (alexandria:eswitch (command)
      ('forward (progn (incf *dist* n)
                       (incf *depth* (* n *aim*))))
      ('down (incf *aim* n))
      ('up (decf *aim* n)))))

(run #'instr-to-pos-ver-2 *courses*)

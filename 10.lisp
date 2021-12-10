(defparameter *nav-sys* (uiop:read-file-lines "input/day10.txt"))

(defparameter *corresp-list*
  '((#\( #\) 3)
    (#\[ #\] 57)
    (#\{ #\} 1197)
    (#\< #\> 25137)))

(defun get-right-paren (c)
  (second (find c *corresp-list* :key #'first)))

(defun get-point (c)
  (third (find c *corresp-list* :key #'second)))

(defun left-paren-p (c)
  (some (lambda (n)
          (char= c n))
        '(#\( #\[ #\{ #\<)))

(defun corrupted-p (line)
  "If corrupted, return CHAR, otherwise NIL"
  (let ((stack nil))
    (loop for c across line
          if (left-paren-p c)
            do (push c stack)
          else do (let ((corresp-pop (pop stack)))
                    (if (not (char= c (get-right-paren corresp-pop)))
                        (return c))))))

(defun solve-first-part (sys)
  (let ((clean-res (remove nil (mapcar #'corrupted-p sys))))
    (reduce #'+ (mapcar #'get-point clean-res))))

;; Second part

(defun auto-complete (line)
  (let ((stack nil))
    (loop for c across line
          if (left-paren-p c)
            do (push c stack)
          else do (pop stack)
          finally (return (mapcar #'get-right-paren stack)))))

(defun right-paren-completion-score (c)
  (1+ (position c *corresp-list* :key #'second)))

(defun completion-score (right-parens)
  (let ((score 0))
    (loop for scr in (mapcar #'right-paren-completion-score right-parens)
          do (setf score (+ (* 5 score)
                            scr))
          finally (return score))))

(defun take-middle (seq)
  (let ((len (length seq)))
    (elt (stable-sort seq #'<) (truncate len 2))))

(defun incomplete-lines (sys)
  (loop for s in sys
        if (not (characterp (corrupted-p s)))
          collect s))

(defun solve-second-part (sys)
  (take-middle
   (loop for line in (incomplete-lines sys)
         collect (completion-score (auto-complete line)))))

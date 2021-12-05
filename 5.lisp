(ql:quickload '("str" "fset"))

(defparameter *vents* (uiop:read-file-lines "input/day5.txt"))

(defun vent-to-cons (vent)
  "For example: \"941,230 -> 322,849\" would be converted
to ((941 . 230) (322 . 849))"
  (mapcar #'(lambda (v)
              (mapcar #'parse-integer (str:split "," v)))
          (str:split " -> " vent)))

(defun vent-src (v-cons)
  (first v-cons))

(defun vent-dest (v-cons)
  (second v-cons))

(defun required-line-p (vent)
  "vents are of the form ((x1 y1) (x2 y2)) only horizontal and vertical lines
diagonal lines for second part"
  (let* ((vent-src (vent-src vent))
         (vent-dest (vent-dest vent))
         (src-x (first vent-src))
         (src-y (second vent-src))
         (dest-x (first vent-dest))
         (dest-y (second vent-dest)))
    (or (= src-x dest-x)
        (= src-y dest-y)
        ;; diagonal-p
        (= (abs (- src-x dest-x))
           (abs (- src-y dest-y))))))

(defun keep-required-lines (vents)
  (remove-if-not #'required-line-p (mapcar #'vent-to-cons vents)))

(defun tuple-compare (comparison-functions)
  (lambda (left right)
    (loop for fn in comparison-functions
          for x in left
          for y in right
            thereis (funcall fn x y)
          until (funcall fn y x))))

(defun gen-diagonal-line (vent)
  (let* ((sorted-vent (stable-sort (copy-list vent)
                                   (tuple-compare (list #'< #'<))))
         (vent-src (vent-src sorted-vent))
         (vent-dest (vent-dest sorted-vent))
         (dist (- (first vent-dest) (first vent-src))))
    (if (= (/ (- (second vent-dest)
                 (second vent-src))
              dist)
           1)
        ;; slope = 1
        (loop for i from 0 to dist
              collect (mapcar #'(lambda (n)
                                  (+ n i))
                              vent-src))
        ;; slope = -1
        (loop for i from 0 to dist
              collect (funcall #'(lambda (n)
                                   (list (+ (first n) i)
                                         (- (second n) i)))
                               vent-src)))))

(defun gen-line (vent)
  (let* ((sorted-vent (stable-sort (copy-list vent)
                                   (tuple-compare (list #'< #'<))))
         (vent-src (vent-src sorted-vent))
         (vent-dest (vent-dest sorted-vent)))
    (cond ((diagonal-p sorted-vent) (gen-diagonal-line sorted-vent))
          ;; ((x1 y) (x2 y))
          ((= (second vent-src) (second vent-dest))
           (loop for i from (first vent-src) to (first vent-dest)
                 collect (list i (second vent-src))))
          ;; ((x y1) (x y2))
          (t (loop for i from (second vent-src) to (second vent-dest)
                   collect (list (first vent-src) i))))))

(defun count-overlaps (bag)
  (fset:set-size
   (fset:filter #'(lambda (i)
                    (> (fset:multiplicity bag i) 1))
                bag)))

(defun solve-problem (vents)
  (let ((req-vents (keep-required-lines vents))
        (bag (fset:empty-bag)))
    (loop for vent in req-vents
          do (loop for dot in (gen-line vent)
                   do (fset:adjoinf bag dot))
          finally (return (count-overlaps bag)))))

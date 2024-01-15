(deftype sprite () '(member :X :O :0))

(defvar *move-number* 0)
(defparameter *game-board* (make-array '(6 7) :initial-element 0))
(defparameter *col-count* (array-dimension *game-board* 0))
(defparameter *row-count* (array-dimension *game-board* 1))
(defvar *win-threshold* 4)
(ql:quickload :array-operations)

;;Thanks grolter
(defun can-drop-column (col-num)
  (loop for i below *col-count*
        for element = (aref *game-board* i col-num)
        thereis (zerop element)))

(defun set-column (array col-index new-column)
  (dotimes (i (array-dimension array 0))
    (setf (aref array i col-index) (elt new-column i))))

(defun check-maxnum-in-row (elements curr-sym maxnum)
  (let ((max-streak 0))
         (let ((current-streak 0))
         (loop for i from 0 to (1- (length elements)) do
             (let ((current (nth i elements))
                   (next (nth (1+ i) elements)))
             (if (eq current curr-sym)
                  (progn
                   (incf current-streak)
                   (setf max-streak (max current-streak max-streak)))
                  (setf current-streak 0)))))
        (eq max-streak maxnum)))
  
(defun check-win-vert-horiz ()
  (let* ((winner nil))
    (loop for col-num below *row-count* do
          (let ((col-elements (loop for i below *col-count* collect (aref *game-board* i col-num))))
            (when (check-maxnum-in-row col-elements 'X *win-threshold*)
              (setf 'X winner))
            (when (check-maxnum-in-row col-elements 'O *win-threshold*)
              (setf 'O winner)))))
    (loop for col-num below *col-count* do
          (let ((row-elements (loop for i below *row-count* collect (aref *game-board* i col-num))))
            (when (check-maxnum-in-row row-elements 'X *win-threshold*)
              (setf 'X winner))
            (when (check-maxnum-in-row row-elements 'O *win-threshold*)
              (setf 'O winner)))))

(defun drop-in-col (col-num player-symbol)
  (let* ((col-elements (loop for i below *col-count* collect (aref *game-board* i col-num)))
         (zero-elem (position 0 col-elements :from-end t)))
    (progn
      (setf (elt col-elements zero-elem) player-symbol)
      (set-column *game-board* col-num col-elements)
      (princ *game-board*))))

(defun random-move ()
  (incf *move-number*))

(defun init-game-board ()

  )

(defun game-loop ()
  (princ "Which row to drop into? ")
  (princ "Valid ones are ")
  (princ (remove-if-not #'can-drop-column (loop for n below *row-count* collect n))) 
  (drop-in-col 1 :X)
  (incf *move-number*)
  (game-loop))

;; https://www.geeksforgeeks.org/zigzag-or-diagonal-traversal-of-matrix/
(defun diagonal-order-pos (matrix row col)
  (loop for line from 1 below (+ row col)
        collect
        (let* ((start-col (max 0 (- line row)))
               (element-count (min line (- col start-col) row)))
          (loop for j from 0 below element-count
                collect (aref matrix (- (- (min row line) j) 1) (+ start-col j))))))

;;this is fairly complicated, will need to be tested well
(defun diagonal-order-neg (matrix row col)
  (loop for line from 1 below (+ row col)
        collect
        (let* ((start-col (max 0 (- line row)))
               (values-to-check (list (- (+ row col) line)
                                      (- row (- line 1))
                                      (- col start-col)
                                      row))
               (non-negative-values (remove-if-not #'plusp values-to-check))
               (element-count (apply #'min non-negative-values))
               (updated-row (if (> start-col 0) 1 row)))
          (loop for j from 0 below element-count
                collect (aref matrix (- (+ (min updated-row line) j) 1) (+ start-col j))))))

;; This will be put in a separate testing file
(defun generate-matrix (rows cols)
  (make-array (list rows cols)
              :initial-contents
              (loop for i below rows
                    collect (loop for j below cols
                                  collect (+ (* i cols) j)))))

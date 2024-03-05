(defparameter *move-number* 0)
(defparameter *piece-names* (list "X" "O"))
(defparameter *game-board* (make-array '(6 7) :initial-element "-"))
(defparameter *row-count* (array-dimension *game-board* 0))
(defparameter *col-count* (array-dimension *game-board* 1))
(defparameter *win-threshold* 4)
(defpackage :connect-n (:use :cl))

;;Thanks grolter
(defun can-drop-column (col-num)
  (loop for i below *row-count*
        for element = (aref *game-board* i col-num)
        thereis (equal element "-")))

;set column and return true if no errors
(defun set-column (array col-index new-column)
  (dotimes (i (array-dimension array 0))
    (setf (aref array i col-index) (elt new-column i)))
   t)

(defun check-maxnum-in-row (elements curr-sym maxnum)
  (unless (< (list-length elements) maxnum)
  (let ((max-streak 0))
         (let ((current-streak 0))
         (loop for i from 0 to (1- (length elements)) do
               (let ((current (nth i elements)))
               (if (eql current curr-sym)
                 (progn
                   (incf current-streak)
                   (setf max-streak (max current-streak max-streak)))
                 (setf current-streak 0)))))
    (when (>= max-streak maxnum)
      curr-sym))))

(defun flatten (lists)
  "Flatten a list like ((O nil) (nil nil) (nil X)) only one layer of nesting"
  (loop for sublist in lists
        append sublist))

(defun populate-winner-list (winner-list symbol-list element-list)
  (loop for value in element-list
        do (push (loop for checker in symbol-list
                       collect (check-maxnum-in-row value checker *win-threshold*))
                 winner-list)))

(defun check-win-vert-horiz ()
     (let* ((winner nil)
            (pos-diagonal (diagonal-order-pos *game-board* *row-count* *col-count*))
            (neg-diagonal (diagonal-order-neg *game-board* *row-count* *col-count*)))
       (populate-winner-list winner *piece-names* pos-diagonal)
       (populate-winner-list winner *piece-names* neg-diagonal) 
       (loop for col-num below *col-count* do
             (let ((col-elements (loop for i below *row-count* collect (aref *game-board* i col-num))))
               (push (loop for checker in *piece-names*
                           collect (check-maxnum-in-row col-elements checker *win-threshold*))
                     winner)))
       (loop for row-num below *row-count* do
             (let ((row-elements (loop for i below *row-count* collect (aref *game-board* row-num i))))
               (push (loop for checker in *piece-names*
                           collect (check-maxnum-in-row row-elements checker *win-threshold*))
                     winner)))
       winner))


(defun drop-in-col (col-num player-symbol)
  (let* ((col-elements (loop for i below *row-count* collect (aref *game-board* i col-num)))
         (zero-elem (position "-" col-elements :test #'equal :from-end t)))
    (if zero-elem (progn
                    (setf (elt col-elements zero-elem) player-symbol)
                    (set-column *game-board* col-num col-elements))
       nil)))

(defun input (prompt)
  (princ prompt)
  (terpri)
  (let ((curr-input (read)))
    (if (numberp curr-input)
        curr-input
        (if (or (string= "QUIT" curr-input) (string= "EXIT" curr-input))
            (progn (princ "Bye") (cl-user::quit))
            (progn (princ "Enter a number, 'quit', or 'exit' please") (input ""))))))

(defun random-move ()
  (incf *move-number*)
  ())

(defun game-loop ()
     (princ "Valid ones are ")
     (terpri)
     (princ (remove-if-not #'can-drop-column (loop for n below *col-count* collect n)))
     (let ((current-column (input "Which column to drop into? ")))
       (terpri)
       (if (evenp *move-number*)
           (drop-in-col current-column "X")
           (drop-in-col current-column  "O"))
       (incf *move-number*)
       (princ *game-board*)
       (when (find "X" (flatten (check-win-vert-horiz)))
         (format t "Player one has won! ~%"))
       (when (find "O" (flatten (check-win-vert-horiz)))
         (format t "Player two has won! ~%"))
       (game-loop)))

(defun init-game-board ()
  (let* ((rows (input "How many rows do you desire?"))
         (cols (input "How many columns do you desire?"))
         (max-win-input (1- (min rows cols)))
         (win-threshold (input (format nil "What is the win threshold (must be at least 3 and at most ~a in a row)?" max-win-input))))
    (if (and (>= max-win-input 3) (<= win-threshold max-win-input))
        (progn
          (defparameter *game-board* (make-array (list rows cols) :initial-element :-))
          (defparameter *row-count* (array-dimension *game-board* 0))
          (defparameter *col-count* (array-dimension *game-board* 1))
          (defparameter *win-threshold* win-threshold)
          (game-loop))
        (progn
          (princ "Incorrect number entered for the win threshold.")
          (init-game-board)))))


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
               (values-to-check (list (- (+ row col) line) (- row (- line 1)) (- col start-col) row))
               (non-negative-values (remove-if-not #'plusp values-to-check))
               (element-count (apply #'min non-negative-values))
               (updated-row (if (> start-col 0) 1 row)))
          (loop for j from 0 below element-count
                collect (aref matrix (- (+ (min updated-row line) j) 1) (+ start-col j))))))

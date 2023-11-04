(deftype sprite () '(member :X :O :0))
(defvar *move-number* 0)
(defvar *game-board* (make-array '(6 7) :initial-elements 0))

(defun can-drop-column (col-num)
  (let* ((col-count (array-dimension *game-board* 0))
        (col-elements (loop for i below col-count collect (aref *game-board* i col-num))))
    (> (loop :for element :in col-elements :counting 0) 0)))

(defun set-column (array col-index new-column)
  (dotimes (i (array-dimension array 0))
    (setf (aref array i col-index) (elt new-column i))))

(defun drop-in-col (col-num player-symbol)
  (let* ((col-count (array-dimension *game-board* 0))
         (col-elements (loop for i below col-count collect (aref *game-board* i col-num)))
         (zero-elem (position 0 col-elements :from-end t)))
    (progn
      (setf (elt col-elements zero-elem) player-symbol)
      (set-column *game-board* col-num  ))
      (princ *game-board*)))

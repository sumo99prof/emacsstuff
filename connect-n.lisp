(deftype sprite () '(member :X :O :0))
(defvar *move-number* 0)
(defvar *game-board* (make-array '(6 7) :initial-element 0))
(defvar *col-count* (array-dimension *game-board* 0))
(defvar *row-count* (array-dimension *game-board* 1))
(defvar *win-threshold* 4)

(defun can-drop-column (col-num)
  (let* ((col-elements (loop for i below *col-count* collect (aref *game-board* i col-num))))
    (> (loop :for element :in col-elements :counting 0) 0)))

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
  (let* ((winner-list '(nil)))
    (loop for col-num below *row-count* do
          (let ((col-elements (loop for i below *col-count* collect (aref *game-board* i col-num))))
            (princ col-elements)
            (when (= *win-threshold* (count 'X col-elements))
              (push 'X (car winner-list)))
            (when (= *win-threshold* (count 'O col-elements))
              (push 'O (car winner-list)))
            ))
    (loop for col-num below *row-count* do
          (let ((col-elements (loop for i below *col-count* collect (aref *game-board* i col-num))))
            (when (= *win-threshold* (count 'X col-elements))
              (push 'X (car winner-list)))
            (when (= *win-threshold* (count 'O col-elements))
              (push 'O (car winner-list)))))))

(defun drop-in-col (col-num player-symbol)
  (let* ((col-elements (loop for i below *col-count* collect (aref *game-board* i col-num)))
         (zero-elem (position 0 col-elements :from-end t)))
    (progn
      (setf (elt col-elements zero-elem) player-symbol)
      (set-column *game-board* col-num col-elements)
      (princ *game-board*))))

(defun random-move ()
  (incf *move-number*))

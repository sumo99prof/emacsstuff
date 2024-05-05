(defpackage :connect-n (:use :cl))
(defparameter *move-number* 0)
(defparameter *piece-names* (list "X" "O"))
(defparameter *game-board* (make-array '(6 7) :initial-element "-"))
(defparameter *row-count* (array-dimension *game-board* 0))
(defparameter *col-count* (array-dimension *game-board* 1))
(defparameter *game-history* '())
(defparameter *win-threshold* 4)
(defparameter *computer-first* t)
(defparameter *has-human* t)
(defparameter *undo-done* nil)

(defun get-row (row-num)
  (loop for i from (* *col-count* row-num) to (+ (* *col-count* row-num) (- *col-count* 1))
        collect (row-major-aref *game-board* i)))

(defun get-column (array col-index)
  "Get the column specified by col-index from the 2D array."
  (loop for i below *row-count* collect (aref array i col-index)))

(defun reverse-move (col-num)
  "Reverse the last move in the specified column. Column must have at least one X or O"
  (let* ((col-elements (get-column *game-board* col-num))
         (last-dropped-elem (position "-" col-elements :test #'equal :from-end t)))
    (if last-dropped-elem
        (setf (nth (+ last-dropped-elem 1) col-elements) "-")
        (setf (car col-elements) "-"))
    (set-column *game-board* col-num col-elements)))

(defun takeback (human-accepted)
  (unless (< (length *game-history*) 2)
    (let* ((first-two (subseq *game-history* 0 2))
           (rest-history (subseq *game-history* 2)))
      (mapcar #'reverse-move first-two)
      (setf *game-history* rest-history))))


;;Thanks grolter
(defun can-drop-axis (is-col num)
  (loop for i below (if is-col *row-count* *col-count*)
        for element = (if is-col (aref *game-board* i num) (aref *game-board* num i))
        thereis (equal element "-")))

(defun pop-checker (col-num move-number)
  "drop the very last element if it matches the players turn. Shift everything down by one"
  (let ((pop-symbol (if (evenp move-number) "X" "O"))
        (current-col (get-column *game-board* col-num)))
    (if (equal pop-symbol (car (last current-col)))
        (list (append '("-") (butlast current-col)) t)
        (list current-col nil))))

(defun repeat-element (element count)
  "Repeats an element a specified number of times."
  (loop for i below count collect element))

(defun random-list-picker (possible-cols)
  (when (null possible-cols) (error "Cannot pick from an empty list"))
  (let ((random-index (random (length possible-cols))))
    (nth random-index possible-cols)))

(defun positions (item sequence &key (test #'equal))
  "Returns a list of all positions of ITEM in SEQUENCE, using TEST for comparison."
  (loop for element in sequence
        for index from 0
        when (funcall test element item)
        collect index))

(defun search-all (subseq seq curr-seq-pos matches)
  "This function returns a list of all positions where subseq appears in seq."
  (unless (and (>= (length seq) (length subseq)) (search subseq seq :test #'equal))  ;base case
    (return-from search-all matches))
  (let ((subseq-length (length subseq))   
        (subseq-position (search subseq seq :test #'equal)))
    (when subseq-position
      (if matches ;when match is null, we have the first match, else accumulate
          (progn 
            (setf curr-seq-pos (+ subseq-position subseq-length curr-seq-pos))
            (push curr-seq-pos matches))
          (push subseq-position matches)))
    (search-all subseq                    
                (subseq seq (+ subseq-position subseq-length)) curr-seq-pos
                matches)))

(defun random-move-easy ()
  "The goal of this AI is to randomly drop a piece in a column, avoiding immediate vertical or horizontal threats of four in a row."
  (let* ((win-minus-one (- *win-threshold* 1))
         (human-opp-sym (if (evenp *move-number*) "O" "X"))
         (computer-sym (if (evenp *move-number*) "X" "O"))
         (n-minus-one (repeat-element human-opp-sym win-minus-one))
         (n-row-column (positions human-opp-sym (flatten (cdr (assoc :columns (check-win-vert-horiz win-minus-one)))) :test #'equal))
         (n-row-row (positions human-opp-sym (flatten (cdr (assoc :rows (check-win-vert-horiz win-minus-one)))) :test #'equal))
         (n-row-column (mapcar (lambda (x) (floor (/ x 2))) n-row-column))
         (n-row-row (mapcar (lambda (x) (floor (/ x 2))) n-row-row))
         (valid-col-threats (remove-if-not (lambda (col) (can-drop-axis t col)) n-row-column))
         (valid-row-threats (remove-if-not (lambda (row) (can-drop-axis nil row)) n-row-row))
         (forced-move '()))
     (loop for column in valid-col-threats
          do (let* ((col-elements (get-column *game-board* column))
                    (leftmost-empty (+ (position "-" col-elements :from-end t :test #'equal) 1))
                    (rightmost-threat (+ leftmost-empty win-minus-one)))
               (when (equal n-minus-one (subseq col-elements leftmost-empty rightmost-threat))
                 (push column forced-move))))
    (loop for row in valid-row-threats
          do (setf forced-move (append forced-move (translate-to-col (get-row row) human-opp-sym))))
    (if forced-move
        (drop-in-col (random-list-picker forced-move) computer-sym)
        (drop-in-col (random-list-picker (remove-if-not (lambda (col) (can-drop-axis t col)) (loop for i below *col-count* collect i))) computer-sym))
    (incf *move-number*)))

(defun translate-to-col (row-slice sym)
  "This function translates a row slice to a column based on search patterns and transformations."
  (let* ((core-list (repeat-element sym (- *win-threshold* 1)))
         (leftmost-edge (mapcar #'string (append core-list '(-))))
         (two-threats (mapcar #'string (append '(-) core-list '(-))))
         (rightmost-edge (mapcar #'string (append '(-) core-list)))
         (two-threat-row-search (search-all two-threats row-slice 0 '()))
         (leftmost-edge-search (search-all leftmost-edge row-slice 0 '()))
         (rightmost-edge-search (search-all rightmost-edge row-slice 0 '()))
         (leftmost-edge-search (remove-if (lambda (x) (member (- x 1) two-threat-row-search)) leftmost-edge-search))
         (rightmost-edge-search (remove-if (lambda (x) (member x two-threat-row-search)) rightmost-edge-search))
         (all-threats '()))
    (when two-threat-row-search
      (setq all-threats
            (append all-threats
                    (apply #'append
                           (mapcar (lambda (x) (list x (+ x (- *win-threshold* 1)))) two-threat-row-search)))))
    (when leftmost-edge-search
      (setq all-threats (append all-threats (mapcar (lambda (x) (+ x (- *win-threshold* 1))) leftmost-edge-search))))
    (when rightmost-edge-search
      (setq all-threats (append all-threats rightmost-edge-search)))
    all-threats))

(defun set-column (array col-index new-column)
  (dotimes (i (array-dimension array 0))
    (setf (aref array i col-index) (elt new-column i)))
  t)

(defun print-message-clear-screen (message)
  (format t message)
  (defun print-board ()
    (dotimes (i (array-dimension *game-board* 0))
      (dotimes (j (array-dimension *game-board* 1))
        (format t "~a " (aref *game-board* i j)))
      (terpri))
    (terpri))
  (print-board)
  (clear-game))

(defun play-screensaver ()
  "Loop this screen saver until user quits, when there is a winner clear the board and start over"
     (loop
       (when (eql (* *row-count* *col-count*) *move-number*)
         (print-message-clear-screen "No one has won ~%"))
       (random-move-easy)
       (when (find "X" (flatten (check-win-vert-horiz 4)) :test #'equal)
         (print-message-clear-screen "Player 1 has won ~%"))
       (when (find "O" (flatten (check-win-vert-horiz 4)) :test #'equal)
         (print-message-clear-screen "Player 2 has won ~%"))
       (fresh-line)
       (print-board)
       (sleep 2)))

(defun check-maxnum-in-row (elements curr-sym maxnum)
  (unless (< (list-length elements) maxnum)
  (let ((max-streak 0))
    (let ((current-streak 0))
         (loop for i from 0 to (1- (length elements)) do
               (let ((current (nth i elements)))
               (if (equal current curr-sym)
                 (progn
                   (incf current-streak)
                   (setf max-streak (max current-streak max-streak)))
                 (setf current-streak 0)))))
    (when (>= max-streak maxnum)
      curr-sym))))

(defun flatten (alist)
  "Flattens a nested association list (alist) to a single level."
  (cond ((null alist) nil)
        ((atom (car alist)) (cons (car alist) (flatten (cdr alist))))  
        (t (append (flatten (car alist)) (flatten (cdr alist)))))) 


(defun populate-winner-list (winner-list symbol-list element-list w-thresh)
  (loop for element in element-list
        do (push (loop for checker in symbol-list
                       collect (check-maxnum-in-row element checker w-thresh))
                 winner-list))
  winner-list)

(defun check-win-vert-horiz (w-thresh)
    "Go through the entire game board with upper, lower, columns, rows etc and make an alist to return"
    (let* ((winner nil)
           (col-vals (loop for col-num below *col-count* collect
                           (let ((col-elements (get-column *game-board* col-num)))
                             (loop for checker in *piece-names* collect (check-maxnum-in-row col-elements checker w-thresh)))))
           (princ col-vals)
           (row-vals (loop for row-num below *row-count* collect
                           (let ((row-elements (get-row row-num)))
                             (loop for checker in *piece-names* collect (check-maxnum-in-row row-elements checker w-thresh)))))
           (pos-diagonal (diagonal-order-pos *game-board* *row-count* *col-count*))
           (neg-diagonal (diagonal-order-neg *game-board* *row-count* *col-count*))
           (winner (acons :diag-neg (populate-winner-list nil *piece-names* neg-diagonal w-thresh) winner))
           (winner (acons :diag-pos (populate-winner-list nil *piece-names* pos-diagonal w-thresh) winner))
           (winner (acons :columns col-vals winner))
           (winner (acons :rows row-vals winner)))
      winner))

(defun drop-in-col (col-num player-symbol)
  (let* ((col-elements (get-column *game-board* col-num))
         (zero-elem (position "-" col-elements :test #'equal :from-end t)))
    (if zero-elem (progn
                    (setf (elt col-elements zero-elem) player-symbol)
                    (set-column *game-board* col-num col-elements)
                    (setf *game-history* (cons col-num *game-history*))
                    (incf *move-number*))
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

(defun clear-game ()
  (setf *move-number* 0)
  (setf *piece-names* (list "X" "O"))
  (setf *game-board* (make-array '(6 7) :initial-element "-"))
  (setf *row-count* (array-dimension *game-board* 0))
  (setf *col-count* (array-dimension *game-board* 1))
  (setf *game-history* '())
  (setf *win-threshold* 4)
  (setf *computer-first* t)
  (setf *has-human* t)
  (setf *undo-done* nil))

(defun win-dialog ()

  )

(defun game-loop ()
  (princ "Valid ones are ")
  (terpri)
  (princ (remove-if-not (lambda (x) (can-drop-axis t x)) (loop for n below *col-count* collect n)))
  (let ((current-column (input "Which column to drop into? ")))
    (terpri)
    (if (evenp *move-number*)
        (drop-in-col current-column "X")
        (drop-in-col current-column "O"))
    (princ *game-board*)
    (when (find "X" (flatten (check-win-vert-horiz 4)) :test #'equal)
      (format t "Player one has won! ~%"))
    (when (find "O" (flatten (check-win-vert-horiz 4)) :test #'equal)
      (format t "Player two has won! ~%"))
    (game-loop)))

(defun init-game-board ()
  (let* ((rows (input "How many rows do you desire?"))
         (cols (input "How many columns do you desire?"))
         (max-win-input (1- (min rows cols)))
         (win-threshold (input (format nil "What is the win threshold (must be at least 3 and at most ~a in a row)?" max-win-input))))
    (if (and (>= max-win-input 3) (<= win-threshold max-win-input))
        (progn
          (setf *game-board* (make-array (list rows cols) :initial-element :-)
                (*row-count* (array-dimension *game-board* 0))
                (*col-count* (array-dimension *game-board* 1))
                (*win-threshold* win-threshold))
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

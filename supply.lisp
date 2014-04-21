(in-package supply)


(let ((supply nil))

  (defun supply-init (plnum)
    (setf supply nil)
    (push-cardset 'copper 60)
    (push-cardset 'silver 40)
    (push-cardset 'gold 30)
    (push-cardset 'estates 24)
    (push-cardset 'duchies (if (<= plnum 2) 8 12))
    (push-cardset 'province (case plnum
			       (2 8) (5 15) (6 18) (t 12)))
    (push-cardset 'curse (* 10 (1- plnum))))

  (defun random-10card-choice (card-list)
    (do ((len (length card-list))
	 (result nil))
	((>= (length result) 10) 
	 (mapcar #'(lambda (x) (nth x card-list)) result))
      (pushnew (random len) result)))

  (defun supply-set (card-list)
    (mapc #'push-cardset
	  (mapcar #'card-name
		  (sort 
		   (mapcar #'(lambda (name) (gethash name *card-table*))
			   (random-10card-choice card-list))
		   #'< :key #'card-cost))))

  (defun push-cardset (card-name &optional (number-of-cards 10))
    (push (cons card-name number-of-cards) supply))

  (defun show-supply ()
   (do* ((i 1 (1+ i))
	 (sup (reverse supply) (cdr sup))
	 (card (gethash (caar sup) *card-table*) 
	       (gethash (caar sup) *card-table*)))
	((null sup))
      (format t "~a : ~a - ~a、コスト ~a、残り ~a枚~%" 
	      i 
	      (card-jname card) 
	      (card-jtype card)
	      (card-cost card)
	      (cdar sup))))

  (defun take-card-from-supply (card-name player &optional (number-of-cards 1) (place :discard-pile))
    (cond ((null (assoc card-name supply))
	   (error "This card doesn't exist in the supply. Please do supply-init"))
	  ((< (cdr (assoc card-name supply)) number-of-cards)
	   (format t  "サプライの枚数が足りません。~%"))
	  (t (decf (cdr (assoc card-name supply)) number-of-cards)
	     (dotimes (i number-of-cards)
	       (case place
		 (:discard-pile (push (gethash card-name *card-table*)				      (discard-pile player)))
		 (:hand-cards (push (gethash card-name *card-table*)
				    (hand-cards player)))
		 (:deck (push (gethash card-name *card-table*)
			      (deck player)))
		 (otherwise (error "look take-card-from-supply place")))))))
  
  (defun choice-supply-card ()
    (princ "カードを選んでください。")
    (fresh-line)
    (show-supply)
    (read-loop num (and (integerp num) (list-indexp supply (1- num))
			(plusp (cdr (nth (- (length supply) num) supply))))
	       (gethash (car (nth (- (length supply) num) supply)) 
		   *card-table*)
	       (format t "有効な範囲を選択してください。~%")))

  (defun end-gamep ()
    (or (<= (cdr (assoc 'province supply)) 0)
	(<= 3 (count-if #'(lambda (x) (<= (cdr x) 0)) supply)))))

(defun list-indexp (lst i)
  (and (integerp i)
       (<= 0 i (1- (length lst)))))

(defun have-actionp (player)
  (have-typep player :action))

(defun have-treasurep (player)
  (have-typep player :treasure))

(defun have-victoryp (player)
  (have-typep player :victory))

(defun have-reactionp (player)
  (have-typep player :reaction))

(defun have-typep (player type)
  (some #'(lambda (card) (card-typep card type)) (hand-cards player)))


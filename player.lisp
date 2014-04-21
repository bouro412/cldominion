(in-package player)

(defparameter *players-list* nil)


(def-exporting-class player ()
  ((ID
    :initarg :ID
    :initform (error "Must supply :ID")
    :accessor ID
    :export t)
   (hand-cards
    :initarg :hand-cards
    :initform nil
    :accessor hand-cards
    :export t)
   (deck
    :initarg :deck
    :initform nil
    :accessor deck
    :export t)
   (using-place
    :initarg :using-place
    :initform nil
    :accessor using-place
    :export t)
   (discard-pile
    :initarg :discard-pile
    :initform nil
    :accessor discard-pile
    :export t)
   (action
    :initarg :action
    :initform 1
    :accessor action
    :export t)
   (buy
    :initarg :buy
    :initform 1
    :accessor buy
    :export t)
   (coin
    :initarg :coin
    :initform 0
    :accessor coin
    :export t)
   (guard
    :initarg :guard
    :initform nil
    :accessor guard
    :export t)))

(define-condition optional-input ()
  ((act :initarg :act
	  :reader act)))

  
(defmethod show-player ((p player))
  (with-accessors ((id id) (hand-cards hand-cards) (deck deck) 
		   (using-place using-place) (discard-pile discard-pile)
		   (action action) (buy buy) (coin coin)) p
    (format t "Player ~a~%" ID)
    (format t "手札 : ~%")
    (print-cards hand-cards)
    (format t "デッキ : ~%")
    (print-cards deck)
    (format t "実行中 : ~%")
    (print-cards using-place)
    (format t "捨て札 : ~%")
    (print-cards discard-pile)
    (format t "残りアクション : ~a~%" action)
    (format t "残り購入権 : ~a~%" buy)
    (format t "コイン : ~a~%" coin)))

(defun make-deck (player)
  (with-slots (deck discard-pile) player
    (setf deck (nconc deck (shuffle-list discard-pile)) 
	  discard-pile nil)))

(defun nshuffle-list (lst)
  (loop for idx downfrom (1- (length lst)) to 1
     for other = (random (1+ idx))
     do (unless (= idx other)
	  (rotatef (elt lst idx) (elt lst other))))
  lst)

(defun shuffle-list (lst)
  (nshuffle-list (copy-list lst)))

(defun draw (player &optional (card-num 1))
  (with-slots (deck hand-cards) player
    (unless (num<lstlen card-num deck)
      (make-deck player))
    (dotimes (i card-num)
      (let ((card (pop deck)))
	(push card hand-cards)
	(format t "~aを引きました。~%" (card-jname card))))))

(defun num<lstlen (num lst)
  (cond ((< num 1) lst)
	(t (num<lstlen (1- num) (cdr lst)))))

(defun choice-card (player)
  (print-cards (hand-cards player))
  (princ "カードを選んでください。")
  (fresh-line)
  (let ((num (read)))
    (if (and (integerp num)
	     (<= 1 num (length (hand-cards player))))
	(1- num)
	(format t "有効な数字を入れてください。"))))

(defmacro game-read-loop (var test then else 
			  &key (initfn '(game-read-number)) (updatefn initfn))
  `(read-loop ,var ,test ,then ,else :initfn ,initfn :updatefn ,updatefn))

(defun game-read-number ()
  (let* ((input (read-line))
	 (num (parse-integer input :junk-allowed t)))
    (cond (num num)
	  ((string= input "") 
	   (error 'optional-input :act :next ))
	  ((string= input "q")
	   (error 'optional-input :act :cancel))
	  (t nil))))

(defun other-players (player)
  (remove-if #'(lambda (x) (equal x player)) *players-list*))

(defun have-typep (player type)
  (some #'(lambda (xs) (find type xs)) 
	(mapcar #'card-type (hand-cards player))))
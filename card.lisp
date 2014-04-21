(in-package card)

(defvar *card-table* (make-hash-table :test #'equal))
(defvar *trush-pile* nil)

(defun get-card (name)
  (gethash name *card-table*))

(defstruct card name jname type cost coin point document ability)

(defmacro exports (&rest symbs)
  `(progn
    ,@(mapcar #'(lambda(x) (list 'export x)) symbs)))

(exports 'card-name 'card-jname 'card-type 'card-cost 'card-coin
	 'card-point 'card-document 'card-ability)

(defun card-jtype (card)
  (labels ((jtype (type)
	     (ecase type
	       (:treasure "財宝")
	       (:victory "勝利点")
	       (:curse "呪い")
	       (:action "アクション")
	       (:reaction "リアクション")
	       (:attack "アタック")))
	   (types-string (types)
	     (if (cdr types)
		 (concatenate 'string (jtype (car types))
			      "・" (types-string (cdr types)))
		 (jtype (car types)))))
    (types-string (card-type card))))

(defun card-typep (card type)
  (find type (card-type card)))

(defun print-card (card)
  (let ((coin (card-coin card))
	(point (card-point card))
	(doc (card-document card)))
    (format t "名前 : ~a~%種類 : ~a~%コスト : ~a~%" 
	    (card-jname card)
	    (card-jtype card)
	    (card-cost card))
    (unless (zerop coin)
      (format t "コイン : ~a~%" coin))
    (unless (zerop point)
      (format t "勝利点 : ~a~%" point))
    (when doc
      (format t "-詳細-~%")
      (format t doc))))

(defun show-card (card)
  "全てのプレイヤーに公開する。"
  (print-card card))

(defun print-cards (card-list)
  (dotimes (i (length card-list))
    (format t "~a : ~a - ~a~%" (1+ i) (card-jname (elt card-list i)) 
	    (card-jtype (elt card-list i)))))

(defun show-cards (card-list)
  "全てのプレイヤーに公開する。"
  (print-cards card-list))

(defmacro defcard (raw-name (jname type cost &key (coin 0) (point 0)(document nil)) 
		   (&rest vars)
		   &rest ability)
  (let ((name `',raw-name))
    `(progn
       (export ,name)
       (setf (gethash ,name *card-table*)
	     (make-card :name ,name
			:jname ,jname
			:type ',(mapcar #'(lambda (x) 
					    (read-from-string (concatenate
							       'string
							       ":" (write-to-string x))))
					type)
			:cost ,cost
			:coin ,coin
			:point ,point
			:document ,document
			:ability #'(lambda (,@vars &key (reac nil))
				     ,@ability))))))
(export 'reac)

(defcard copper ("銅貨" (treasure) 0 :coin 1)())
(defcard silver ("銀貨" (treasure) 3 :coin 2)())
(defcard gold ("金貨" (treasure) 6 :coin 3)())
(defcard estates ("屋敷" (victory) 2 :point 1)(_) 1)
(defcard duchies ("公領" (victory) 5 :point 3)(_) 3)
(defcard province ("属州" (victory) 8 :point 6)(_) 6)
(defcard curse ("呪い" (curse) 0 :point -1)(_) -1)

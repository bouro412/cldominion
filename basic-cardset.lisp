(in-package basic-cardset)
(defparameter *basic-cardlist* nil)

(defmacro def-basic-card (name &body body)
  `(progn
     (pushnew ',name *basic-cardlist*)
     (defcard ,name ,@body)))

(defun ability (player card &key (reac nil))
  (funcall (card-ability card) player :reac reac))

(defun +x-action (player &optional(x 1))
  (incf (action player) x))

(defun +x-coin (player &optional (x 1))
  (incf (coin player) x))

(defun +x-buy (player &optional (x 1))
  (incf (buy player) x))

(defun x-discard (player &optional (x 1))
  (dotimes (var x)
    (format t "プレイヤー~a カードをあと~a枚捨てます。~%" (id player) (- x var))
    (format t "プレイヤー~a 手札 : ~%" (id player))
    (read-loop card-index
	       (and (integerp card-index) 
		    (<= 0 card-index (length (hand-cards player))))
	       (move-card (elt (hand-cards player) card-index)
			  (hand-cards player) (discard-pile player))
	       (format t "有効な数字を入力してください。~%")
	       :initfn (choice-card player))))

(defun x-trushcard (player &optional (x 1))
  (dotimes (var x)
    (format t "カードをあと~a枚廃棄します。~%" (- x var))
    (format t "プレイヤー~a 手札 : ~%" (id player))
    (read-loop card-index
	       (and (integerp card-index) 
		    (<= 0 card-index (length (hand-cards player))))
	       (move-card (elt (hand-cards player) card-index)
			  (hand-cards player) *trush-pile*)
	       (format t "有効な数字を入力してください。~%")
	       :initfn (choice-card player))))

(defun discard (player card)
  (setf (hand-cards player) 
	(delete card (hand-cards player) :test #'equal :count 1))
  (push card (discard-pile player)))

(defmacro move-card (card from to)
  (let ((gcard (gensym))
	(gfrom (gensym)))
    `(let ((,gcard ,card)
	   (,gfrom ,from))
       (if (find ,gcard ,gfrom :test #'equal)
	   (progn (deletef ,from ,gcard :test #'equal :count 1)
		  (push ,gcard ,to))
	   (format t "カードが見つかりませんでした。~%")))))


(define-modify-macro deletef (obj &rest args)
  (lambda (place obj &rest args)
    (apply #'delete obj place args)))

(defun remove-index (lst i)
  (nconc (subseq lst 0 i) (subseq lst (1+ i))))

(defmacro bind-if ((var value) test then else)
  `(let ((,var ,value))
     (if ,test ,then ,else)))


(defmacro awhen (test &body body)
  `(let ((it ,test))
     (if it
	 (progn ,@body))))

(defmacro attack-players ((var players) &body body)
  `(dolist (,var ,players)
     (if (guard ,var)
	 (setf (guard ,var) nil)
	 (progn ,@body))))

(def-basic-card village ("村" (action) 3 
			       :document "+2アクション~%1ドロー~%") (player)
			       (+x-action player 2)
			       (draw player 1))

(def-basic-card smithy ("鍛冶屋" (action) 4
				  :document "3ドロー~%") (player)
				  (draw player 3))

(def-basic-card militia ("民兵" (action attack) 4
				 :document "+2コイン~%他のプレイヤーは全員、自分の手札が3枚になるまで捨て札をする。~%")
  (player)
  (+x-coin player 2)
  (attack-players (p (other-players player))
    (let ((hcnum (length (hand-cards p))))
      (when (< 3 hcnum)
	(x-discard p (- hcnum 3))))))

(def-basic-card cellar ("地下貯蔵庫" (action) 2
				      :document "+1アクション~%手札から好きな枚数のカードを捨て札にする。~%捨て札1枚につき、カードを1枚引く。~%") 
  (player)
  (+x-action player)
  (print-cards (hand-cards player))
  (format t  "捨てる枚数を決めてください。~%")
  (let ((num (read-loop x 
			 (and (integerp x) 
			      (<= 0 x (length (hand-cards player))))
			 x 
			 (format t "捨てる枚数を決めてください。~%"))))
    (x-discard player num)
    (draw player num)))

(def-basic-card moat ("堀" (action reaction) 2
			    :document "+2ドロー~%他のプレイヤーがアタックカードを使用した時、手札からこのカードを公開できる。~%そうした場合、あなたはそのアタックカードの影響を受けない。~%")
  (player)
  (if reac
      (setf (guard player) t)
      (draw player 2)))

(def-basic-card workshop ("工房" (action) 3
				  :document "コスト最大4までのカードを獲得する。~%")
		(player)
		(format t "コスト4以下のカードを選んでください。~%")
		(read-loop card (<= (card-cost card) 4) 
			   (take-card-from-supply (card-name card) player)
			   (format t "コスト4以下のカードを選んでください。~%")
			   :initfn (choice-supply-card)))

(def-basic-card remodel ("改築" (action) 4
				 :document "あなたの手札のカード1枚を廃棄する。廃棄したカードよりコストが最大2多いカード1枚を獲得する。~%")
		(player)
		(format t "カードを1枚廃棄します。~%")
		(x-trushcard player)
		(let ((limit (+ 2 (card-cost (car *trush-pile*)))))
		  (format t "コスト~a以下のカードを選んでください。~%" limit)
		  (read-loop card (<= (card-cost card) limit)
			     (take-card-from-supply (card-name card) player)
			     (format t "コスト~a以下のカードを選んでください。~%" limit)
			     :initfn (choice-supply-card))))

(def-basic-card laboratory ("研究所" (action) 5
				      :document "+2ドロー~%+1アクション~%")
		(player)
		(draw player 2)
		(+x-action player))

(def-basic-card throne-room ("玉座の間" (action) 4
					:document "あなたの手札のアクションカード1枚を選ぶ。~%そのカードを2回使用する。")
		(player)
		(when (some #'(lambda (xs) (find :action xs)) 
			    (mapcar #'card-type (hand-cards player)))
		  (read-loop 
		   card
		   (find :action (card-type card))
		   (with-accessors ((hc hand-cards) (up using-place))
		       player
		     (format t "プレイヤー~aが~aを2回使いました。~%"
			     (id player) (card-jname card))
		     (print-card card)
		     (setf hc (delete card hc :test #'equal :count 1))
		     (dotimes (nouse 2)
		       (when (find :attack (card-type card))
			 (dolist (p (remove-if #'(lambda (x) (equal x player))
					       *players-list*))
			   (when-bind (rcard (game::use-reactionp p))
			     (ability p rcard :reac t))))
		       (ability player card)))
		   (format t "アクションカードを選んでください。~%")
		   :initfn (elt (hand-cards player) 
				(choice-card player)))))

(def-basic-card spy ("密偵" (action attack) 4
			     :document "+1ドロー~%+1アクション~%各プレイヤー(あなたも含む)は、自分の山札の一番上のカードを公開し、そのカードを捨て札にするかそのまま戻すかをあなたが選ぶ。~%") 
		(player)
		(draw player)
		(+x-action player)
		(format t "各プレイヤーの山札の一番上のカードを公開します。~%")
		(attack-players (p *players-list*)
		  (format t "プレイヤー~a :" (id p))
		  (show-card (car (deck p)))
		  (if (y-or-n-p "このカードを捨て札にしますか?")
		      (progn
			(format t "捨て札になりました。~%")
			(push (pop (deck p)) (discard-pile p)))
		      (format t "山札に戻されました。~%"))))

(def-basic-card council-room ("議事堂" (action) 5
					:document "+4ドロー~%+1カード購入~%他のプレイヤー全員は、カードを1枚引く。~%")
		(player)
		(draw player 4)
		(+x-buy player)
		(mapc #'draw (other-players player)))

(def-basic-card library ("書庫" (action) 5
				 :document
				 "あなたの手札が7枚になるまでカードを引く。~%この方法で引いたアクションカードを脇に置いてもよい。(7枚には数えない。)脇に置いたカードは、このアクションの後、捨て札にする。~%")
		(player)
		(with-accessors ((hc hand-cards) (dp discard-pile) (deck deck)
				 (up using-place))
		    player
		  (do ((card (car deck) (car deck)))
		      ((>= (length hc) 7))
		    (print-card card)
		    (if (find :action (card-type card))
			(if (y-or-n-p "このカードを手札に加えますか?")
			    (progn
			      (push (pop deck) hc)
			      (format t "手札に加えました。~%"))
			    (progn 
			      (push (pop deck) up)
			      (format t "脇に置きました。~%")))
			(progn
			  (push (pop deck) hc)
			  (format t "手札に加えました。~%"))))))

(def-basic-card  chapel ("礼拝堂" (action) 2
				  :document "あなたの手札から最大4枚までのカードを、廃棄する。~%")
		 (player)
		 (format t "何枚廃棄しますか? : ~%")
		 (read-loop num (and (integerp num) 
				     (<= 0 num 
					 (min 4 (length (hand-cards player)))))
			    (x-trushcard player num)
			    (format t "有効な数字を入力してください。~%")))

(def-basic-card gardens ("庭園" (victory) 4 :point 0
			 :document "あなたの山札のカード10枚(端数切り捨て)につき勝利点1点を得る。~%")
		(player)
		(floor (/ (length (deck player)) 10)))

(def-basic-card mine  ("鉱山" (action) 5
			 :document "あなたの手札の財宝カード1枚を廃棄する。~%廃棄した財宝よりもコストが最大3コイン多い財宝カード１枚を獲得し、あなたの手札に加える。~%")
		(player)
		(when (have-treasurep player)
		  (format t "廃棄するカードを選んでください。~%")
		  (read-loop 
		   card 
		   (and (card-typep card :treasure)
			(progn
			  (print-card card)
			  (y-or-n-p "このカードを廃棄しますか？")))
		   (progn
		     (discard player card)
		     (let ((limcost (+ 3 (card-cost card))))
		       (format t "コストが~a以下のカードを獲得できます。~%"
			       limcost)
		       (read-loop 
			newcard
			(and (<= (card-cost newcard) limcost)
			     (y-or-n-p "本当にこれでいいですか?"))
			(take-card-from-supply (card-name newcard)
					       player
					       1
					       :hand-cards)
			(format t 
				"コストが~a以下のカードを獲得できます。~%"
				limcost)
			:initfn (choice-supply-card))))
		   (format t "廃棄するカードを選んでください。~%")
		   :initfn  (elt (hand-cards player) (choice-card player)))))

(def-basic-card woodcutter ("木こり" (action) 3
			   :document "+1カードを購入~%+2コイン")
		(player)
		(+x-buy player)
		(+x-coin player 2))

(def-basic-card witch ("魔女" (action attack) 5
			 :document "+2カードを引く。~%他のプレイヤーは全員、呪いカードを1枚ずつ獲得する。~%")
		(player)
		(draw player 2)
		(attack-players (p (other-players player))
		  (take-card-from-supply 'curse p)))

(def-basic-card adventurer ("冒険者" (action) 6
			   :document "あなたの山札から財宝カード2枚が公開されるまで、カードを公開する。~%公開した財宝カード2枚を手札に加え、他の公開したカードは捨て札に置く。~%")
		(player)
		(labels 
		    ((bodfn (count)
		       (unless (zerop count)
			 (bind-if 
			  (card (pop (deck player)))
			  (progn
			   (format t "デッキから~aを引きました。~%" (card-jname card))
			   (card-typep card :treasure))
			  (progn
			    (format t "手札に入りました。~%")
			    (push card (hand-cards player))
			    (bodfn (1- count)))
			  (progn
			    (format t "捨て札になりました。~%")
			    (push card (discard-pile player))
			    (bodfn count))))))
		  (bodfn 2)))

(def-basic-card bureaucrat ("役人" (action attack) 4
			 :document "銀貨1枚を獲得し、あなたの山札の上に置く。~%他のプレイヤーは全員、自分の手札から勝利点カード1枚を公開し、自分の山札の上に置く。~%(手札に勝利点カードがない場合、手札を公開する。)~%")
		(player)
		(take-card-from-supply 'silver player :place :deck)
		(attack-players (p (other-players player))
		  (if (have-victoryp p)
		      (progn
			(format t "公開する勝利点カードを選んでください。~%")
			(read-loop card (find :victory (card-type card))
				   (progn (format t "プレイヤー~a : ~%" (id p)) 
					  (show-card card)
					  (move-card card (hand-cards p) (deck p)))
				   (format t 
					   "公開する勝利点カードを選んでください。~%")
				   :initfn (elt (hand-cards p) (choice-card p))))
		      (progn
			(format t "勝利点カードがないので手札が公開されます。~%")
			(show-cards (hand-cards p))))))

(def-basic-card chancellor ("宰相" (action) 3
			 :document "+2コイン~%あなたの山札のカードすべてを、即座に捨て札に置くことができる。~%")
		(player)
		(+x-coin player 2)
		(when (y-or-n-p "全ての山札を捨て札にしますか?")
		  (with-accessors ((dp discard-pile) (deck deck)) player
		    (setf dp (nconc dp deck)
			  deck nil))))

(def-basic-card festival ("祝祭" (action) 5
			 :document "+2アクション~%+1カードを購入~%+2コイン~%")
		(player)
		(+x-action player 2)
		(+x-buy player)
		(+x-coin player 2))

(def-basic-card market ("市場" (action) 5
				:document "+1カードを引く~%+1アクション~%+1カードを購入~%+1コイン~%")
		(player)
		(draw player)
		(+x-action player)
		(+x-coin player))

(def-basic-card feast ("祝宴" (action) 4
			 :document "このカードを廃棄する。~%コスト5以下のカード1枚を獲得する。~%")
		(player)
		(move-card (get-card 'feast) 
			   (using-place player)
			   *trush-pile*)
		(format t "コスト5以下のカードを獲得できます。~%")
		(read-loop card
			   (<= (card-cost card) 5)
			   (take-card-from-supply (card-name card) player)
			   (format t "コスト5以下のカードを選んでください。~%")
			   :initfn (choice-supply-card)))

(def-basic-card moneylender ("金貸し" (action) 4
			   :document "あなたの手札から銅貨1枚を廃棄する。~%そうした場合、+3コインを使用できる。~%")
		(player)
		(awhen (find-if #'(lambda (x) (eq (card-name x) 'copper))
			       (hand-cards player))
		  (move-card it (hand-cards player) *trush-pile*)
		  (format t "銅貨を廃棄しました。~%+3コイン~%")
		  (+x-coin player 3)))
		
			     
			     
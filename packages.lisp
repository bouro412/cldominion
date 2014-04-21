(defvar *dirpath* (pathname "~/Documents/clisp/dominion/"))


(defpackage card
  (:use :cl)
  (:export *card-table*
	   *trush-pile*
	   get-card
	   card
	   card-typep
	   print-card
	   show-card
	   print-cards
	   show-cards
	   card-jtype
	   defcard))

(defpackage player
  (:use :cl
	:card)
  (:import-from :myutil
		def-exporting-class
		with-all-slots
		read-loop)
  (:export player
	   *players-list*	   
	   show-player
	   make-deck
	   draw
	   choice-card
	   other-players
	   have-typep))

(defpackage supply
  (:use :cl
	:card
	:player)
  (:import-from :myutil
		read-loop)
  (:export supply-init
	   push-cardset
	   show-supply
	   supply-set
	   take-card-from-supply
	   choice-supply-card
	   end-gamep
	   have-typep
	   have-actionp
	   have-reactionp
	   have-victoryp
	   have-treasurep))

(defpackage basic-cardset
  (:use :cl
	:card
	:player
	:supply)
  (:import-from :myutil
		when-bind
		read-loop)
  (:export ability
	   *basic-cardlist*
	   move-card
	   deletef
	   remove-index
	   bind-if))

(defpackage game
  (:use :cl
	:card
	:player
	:basic-cardset
	:supply
	:myutil))
  #|(:shadowing-import-from :card
			  card
			  copper
			  silver
			  gold
			  estates
			  duchies
			  province
			  curse
			  print-card
			  print-cards)|#




(load "~/Documents/clisp/dominion/card.lisp")
(load "~/Documents/clisp/dominion/player.lisp")
(load "~/Documents/clisp/dominion/supply.lisp")
(load "~/Documents/clisp/dominion/basic-cardset.lisp")
(load "~/Documents/clisp/dominion/game.lisp")

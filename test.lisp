(load "~/Documents/clisp/dominion/packages.lisp")
(defpackage test
  (:use :cl
	:card
	:player
	:basic-cardset
	:supply
	:myutil)
  (:import-from :game
		play-game
		game-init
		game-loop
		players-init))

(in-package test)

(defun reload ()
	   (load "~/Documents/clisp/dominion/packages.lisp"))
(defun play-test (&key (supply-choice nil) (hand-cards))
  (game::play-game 2))

(defun set-supply (&rest args &key (num 4))
  (supply-init num)
  (mapc #'push-cardset names))

(defun start-game ()
  (game-loop (length *players-list*)))

(defun test-init ()
  (game-init 2)
  (setf p1 (car *players-list*)
	p2 (second *players-list*)))

(defun hc-set (player &rest card-names)
  (setf (hand-cards player)
	(mapcar #'get-card card-names))
  (while (< (length (hand-cards player)) 5)
    (push (get-card 'copper) (hand-cards player))))

(defun reset ()
  (dolist (p *players-list*)
    (with-accessors ((ac action) (buy buy) (coin coin)
		     (guard guard)) p
      (setf ac 1 buy 1 coin 0 guard nil))))

(defvar p1 nil)
(defvar p2 nil)

(defun card-test (card-list)
  (test-init)
  (hc-set p1 card-list)
  (take-card-from-supply 'province p1 7)
  (setf (coin p2) 10)
  (start-game))




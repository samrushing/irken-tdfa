;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "reorder.scm")

(define (test-cycle-decomposition)
  (let ((p (tree/make < (1 3) (2 1) (3 2) (4 5) (5 4) (6 7) (7 6))))
    (printn (cycle-decomposition p)))
  (let ((p (tree/make < (1 2) (2 3) (3 4) (4 5) (5 6) (6 7) (7 1))))
    (printn (cycle-decomposition p)))
  (let ((p (tree/make < (1 7) (7 2) (2 6) (6 3) (3 5) (5 4) (4 1))))
    (printn (cycle-decomposition p)))
  (let ((p (tree/make < (1 2) (2 3) (3 4) (7 6) (6 5) (5 7))))
    (printn (cycle-decomposition p)))
  (let ((p (tree/make < (1 12) (2 13) (3 3) (4 1) (5 11) (6 9) (7 5) (8 10) (9 6) (10 4) (11 7) (12 8) (13 2))))
    (printn (cycle-decomposition p)))
  )

(define (test-cycle->moves)
  (for-list x (cycle->moves 1 '() '(1 2 3 4))
    (printf " " (lpad 3 (int x.src)) " -> " (lpad 3 (int x.dst)) "\n"))
  )

(define (test-reorder)
  (let ((p (tree/make < (1 3) (2 1) (3 2) (4 5) (5 4) (6 7) (7 6))))
    (printf "---------------\n")
    (printn (cycle-decomposition p))
    (for-list x (reorder p)
      (printf " " (lpad 3 (int x.src)) " -> " (lpad 3 (int x.dst)) "\n")))
  (let ((p (tree/make < (5 4) (1 0) (6 5) (7 6) (0 7))))
    (printf "---------------\n")
    (printn (cycle-decomposition p))
    (for-list x (reorder p)
      (printf " " (lpad 3 (int x.src)) " -> " (lpad 3 (int x.dst)) "\n"))
    )
  )

(test-cycle-decomposition)
(test-cycle->moves)
(test-reorder)

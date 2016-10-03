;; -*- Mode: Irken -*-

(include "lib/basis.scm")
(include "cmap.scm")

(define (t0)
  (let ((cmap (cmap/make <)))
    ;; interesting: the first 0 is at position 31!
    (for-list x '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9)
      (cmap/add cmap x))
    (for-range i 9
      (printf "index of " (int (+ i 1)) 
	      " = " (int (cmap->index cmap (+ i 1)))
	      "\n"))
    ))

(define (t1)
  (let ((cmap (cmap/make string<?)))
    (for-list x (string-split "I have sworn upon the altar of god eternal hostility against every form of tyranny over the mind of man" #\space)
      (cmap/add cmap x))
    (printf "unique: " (int cmap.count) "\n")
    (for-map index item cmap.rev
      (printf (lpad 2 (int index)) " : " item "\n")
      )
    )
  )

(t0)
(t1)

	 

;; -*- Mode: Irken -*-

(require "tdfa.scm")
(require "machine.scm")

(define (do-one tdfa block)
  (let ((m (machine/make tdfa))
	(hits '()))

    (define (callback tags)
      (let loop ((tags tags))
	(match tags with
	  ()
	  -> #u
	  ((:tuple tn0 lo) (:tuple tn1 hi) . tl)
	  -> (let ((grp (int->string (/ tn0 2))))
	       (push! hits (format (repeat lo " ") (repeat (- hi lo) grp)))
	       (loop tl))
	  _ -> (impossible) ;; tags are always in pairs
	  )))

    (machine/feed m block 0 callback #t)
    (printf block "\n")
    (for-list hit (reverse hits)
      (printf hit "\n")))
  )

(define (read-line file)
  (match (file/read-line file) with
    (maybe:yes v) -> (if (zero? (string-length v)) (maybe:no) (maybe:yes v))
    (maybe:no)    -> (maybe:no)
    ))

(define (ask prompt file)
  (printf prompt) (flush)
  (read-line file))

(define (y-or-n prompt file default-yes?)
  (match (ask prompt file) with
    (maybe:yes line)
    -> (match (string-ref line 0) with
	 #\Y -> #t
	 #\y -> #t
	 _   -> #f)
    (maybe:no)
    -> default-yes?
    ))

(define (test-tdfa tdfa file)
  (let loop ()
    (match (ask "input or <return> to quit: " file) with
      (maybe:yes line)
      -> (begin (do-one tdfa line) (loop))
      (maybe:no)
      -> #u
      )))

(define (read-and-compile)
  (let ((stdin (file/open-stdin)))
    (let loop ()
      (match (ask "enter regex: " stdin) with
        (maybe:yes line)
        -> (begin
	     (printf "line = '" line "'\n")
	     (let ((tdfa (rx->tdfa line)))
	       (dump-tdfa tdfa)
	       (if (y-or-n "test it (y/n) [y]? " stdin #f)
		   (test-tdfa tdfa stdin))
	       (loop)))
        (maybe:no)
        -> #u
        ))))

(read-and-compile)

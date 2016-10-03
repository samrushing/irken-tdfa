;; -*- Mode: Irken -*-

(include "tdfa.scm")
(include "machine.scm")

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
	       (PUSH hits (format (repeat lo " ") (repeat (- hi lo) grp)))
	       (loop tl))
	  _ -> (impossible) ;; tags are always in pairs
	  )))

    (machine/feed m block 0 callback #f)
    (printf block "\n")
    (for-list hit (reverse hits)
      (printf hit "\n")))
  )

(define (file/read-line file)
  (let loop ((ch (file/read-char file))
	     (r '()))
    (if (eq? ch #\newline)
	(list->string (reverse r))
	(loop (file/read-char file) (list:cons ch r)))))

(define (ask prompt file)
  (printf prompt) (flush)
  (file/read-line file))

(define (y-or-n prompt file default-yes?)
  (let ((line (ask prompt file)))
    (if (= 0 (string-length line))
	default-yes?
	(match (string-ref line 0) with
	  #\Y -> #t
	  #\y -> #t
	  _   -> #f))))

(define (test-tdfa tdfa file)
  (let loop ()
    (let ((line (ask "input or <return> to quit: " file)))
      (cond ((= 0 (string-length line)) #u)
	    (else (do-one tdfa line)
		  (loop))
	    ))))

(define (read-and-compile)
  (let ((stdin (file/open-stdin)))
    (let loop ((line (ask "enter regex: " stdin)))
      (cond ((= 0 (string-length line)) #u)
	    (else 
	     (printf "line = '" line "'\n")
	     (let ((tdfa (rx->tdfa line)))
	       (dump-tdfa tdfa)
	       (if (y-or-n "test it (y/n) [y]? " stdin #f)
		   (test-tdfa tdfa stdin))
	       (loop (ask "enter regex: " stdin))))
	    ))))

(read-and-compile)

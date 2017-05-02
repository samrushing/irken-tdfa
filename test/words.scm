;; -*- Mode: Irken -*-

(include "tdfa.scm")
(include "lib/os.scm")

(define (file/read-line file)
  (let loop ((ch (file/read-char file))
	     (r '()))
    (cond ((eq? ch #\eof) (maybe:no))
	  ((eq? ch #\newline)
	   (maybe:yes (list->string (reverse r))))
	  (else
	   (loop (file/read-char file) (list:cons ch r))))))

(define (read-words path)
  (let ((input (file/open-read path)))
    (let loop ((words (set/empty)))
      (match (file/read-line input) with
	(maybe:yes word)
	-> (loop (set/add words string-compare word))
	(maybe:no)
	-> words))))

(include "emit.scm")

(define (compile-tdfa path)
  (let ((words (set->list (read-words path)))
	(tdfa (rx->tdfa (format ".*{(" (join "|" words) ")=}")))) ;; with minimization
	;;(tdfa (rx->tdfa (format ".*{(" (join "|" words) ")}")))) ;; without
    (emit-hybrid-c (file-writer "t1.c" "  ") tdfa)
    (printf "wrote t1.c\n")
    (tdfa-statistics tdfa)
    ))
  
(if (> sys.argc 1)
    (compile-tdfa sys.argv[1])
    (printf "usage: " sys.argv[0] " <words_file>\n"))

;; (printf "calls " (int *calls*) "\n")
;; (printf "hits  " (int *hits*) "\n")
;; (printf "|charsets| " (int (tree/size *charsets*.map)) "\n")
;; (printf "|pairs| " (int (tree/size *charset-pairs*.map)) "\n")

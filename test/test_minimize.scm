;; -*- Mode: Irken -*-

(include "tdfa.scm")
(include "minimize.scm")

(define (t0 rx)
  (let ((tdfa0 (rx->tdfa rx))
	(tdfa1 (minimize tdfa0)))
    (printf "---- original:\n")
    (dump-tdfa tdfa0)
    (printf "---- minimal:\n")
    (dump-tdfa tdfa1)
    ))

;(t0 "(abb|abbb|abbbbb|ab+)")

(define (TS ts sym)
  {ts=ts sym=sym insns='()})

(defmacro TRANS
  (TRANS)
  -> (list:nil)
  (TRANS (ch to) rest ...)
  -> (list:cons (TS to ch) (TRANS rest ...))
  )

(define (t1)
  (define E (tree:empty))
  (let ((a (parse-charset "a"))
	(b (parse-charset "^a"))
	(dot charset/dot)
	(s0 (TRANS (b 4) (a 1)))
	(s1 (TRANS (b 2) (a 5)))
	(s2 (TRANS (a 3) (b 6)))
	(s3 (TRANS (dot 3)))
	(s4 (TRANS (b 4) (a 1)))
	(s5 (TRANS (b 4) (a 1)))
	(s6 (TRANS (b 7) (a 3)))
	(s7 (TRANS (b 6) (a 3)))
	)
    {machine=(list->vector (LIST s0 s1 s2 s3 s4 s5 s6 s7))
	     finals=(tree/make int-cmp (2 E) (7 E))
	     nregs=0}
    ))

(rx->tdfa "abc")

(let ((dfa0 (t1))
      (dfa1 (minimize dfa0)))
  (dump-tdfa dfa0)
  (dump-tdfa dfa1)
  )

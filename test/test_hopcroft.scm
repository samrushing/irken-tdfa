;; -*- Mode: Irken -*-

(include "tdfa.scm")

(define (TS ts sym)
  {ts=ts sym=sym insns='()})

(defmacro TRANS
  (TRANS)
  -> (list:nil)
  (TRANS (ch to) rest ...)
  -> (list:cons (TS to ch) (TRANS rest ...))
  )

(define E (tree:empty))

;; example from http://www.cs.engr.uky.edu/~lewis/essays/compilers/min-fa.html
(define (e1)
  (let ((a (parse-charset "a"))
	(b (parse-charset "^a"))
	(dot charset/dot)
	(s0 (TRANS (b 4) (a 1)))
	(s1 (TRANS (b 2) (a 5)))
	(s2 (TRANS (a 3) (b 6)))
	(s3 (TRANS (a 3) (b 3)))
	(s4 (TRANS (b 4) (a 1)))
	(s5 (TRANS (b 4) (a 1)))
	(s6 (TRANS (b 7) (a 3)))
	(s7 (TRANS (b 6) (a 3)))
	)
    {machine=(list->vector (LIST s0 s1 s2 s3 s4 s5 s6 s7))
	     finals=(tree/make < (2 E) (7 E))
	     nregs=0}
    ))

;; example from http://www.csee.umbc.edu/~squire/cs451_l12.html
(define (e2)
  (let ((a (parse-charset "a"))
	(b (parse-charset "^a"))
	(dot charset/dot)
	(s0 (TRANS (a 1) (b 4)))
	(s1 (TRANS (a 2) (b 3)))
	(s2 (TRANS (a 7) (b 8)))
	(s3 (TRANS (a 8) (b 7)))
	(s4 (TRANS (a 5) (b 6)))
	(s5 (TRANS (a 7) (b 8)))
	(s6 (TRANS (a 7) (b 8)))
	(s7 (TRANS (a 7) (b 7)))
	(s8 (TRANS (a 8) (b 8)))
	)
    {machine=(list->vector (LIST s0 s1 s2 s3 s4 s5 s6 s7 s8))
	     finals=(tree/make < (2 E) (3 E) (5 E) (6 E))
	     nregs=0}
    ))

(define (t0 tdfa)
  (dump-tdfa tdfa)
  (dump-tdfa (hopcroft tdfa)))

(t0 (rx->tdfa "(abb|abbb|abbbbb|ab+)"))
(t0 (e1))
(t0 (e2))

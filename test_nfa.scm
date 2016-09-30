;; -*- Mode: Irken -*-

;(include "nfa.scm")
(include "tdfa.scm")

(define (t r)
  (let ((rx (parse-rx r)))
    (let-values (((nfa nstates) (rx->nfa rx (starts-with r ".*"))))
      (printf (rx-repr rx) "\n")
      (print-nfa nfa)
      (printf (int nstates) " states\n")
      (printn (nfa->map nfa nstates))
      )))

;; (t "ab")
;; (t "abc")
;; (t "a+")
;; (t "a*")
;; (t "(abc)*")
;; (t "((ab)|c)+")
;; (t "ab")
;; (t "a?b")
;; (t "a{b+}c")
;; (t ".*{BBB\\-BB\\-BBBB}")

;(t ".*{a+}")
;(t ".*{({ab}|{bc})e}")
(t "x+(abc)~")


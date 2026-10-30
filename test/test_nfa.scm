;; -*- Mode: Irken -*-

;; tdfa rather than nfa because of nfa:min
(require "tdfa.scm")

(define (t r)
  (let ((rx (parse-rx r)))
    (let (((nfa nstates) (rx->nfa rx (starts-with r ".*"))))
      (printf (rx-repr rx) "\n")
      (print-nfa nfa)
      (printf (int nstates) " states\n")
      ;;(printn (nfa->map nfa nstates))
      )))

;; (t "a")
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
;; (t ".*{a+}")
;; (t ".*{({ab}|{bc})e}")
(t "{.*(abc)~}")
(t ".*(abc)=")
(t ".*(abc)~")


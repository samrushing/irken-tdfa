;; -*- Mode: Irken -*-

(include "tdfa.scm")

(define (test-partition)
  (let ((c0 (parse-charset "abc"))
	(c1 (parse-charset "bcd"))
	(c2 (parse-charset "cde"))
	(parts (make-partition (list->set (LIST c0 c1 c2) charset< (set/empty)))))
    (printf "parts: " (join charset-repr ", " parts) "\n")
    ))

(define (test-partition2)
  (let ((c0 (parse-charset "A"))
	(c1 (parse-charset "M"))
	(parts (make-partition (list->set (LIST c0 c1) charset< (set/empty)))))
    (printf "parts: " (join charset-repr ", " parts) "\n")
    ))

(include "lib/random.scm")
(define (test-substate-range)
  (let ((k0 (list->set (LIST {tn=0 ti=0} {tn=1 ti=0} {tn=2 ti=1}) tag< (set/empty)))
	(ss (set/empty)))
    (for-range i 15
      (set/add! ss substate< {u=(mod (random) 10) p=0 k=k0}))
    (printf "all {\n")
    (for-set x ss
      (printf "  " (substate-repr x) "\n"))
    (printf "}\n")
    (printf "range [2-7) {\n")
    (for-list x 
	(set/range->list ss substate< 
			 {u=2 p=0 k=(set/empty)}
			 {u=7 p=0 k=(set/empty)})
      (printf "  " (substate-repr x) "\n"))
    (printf "}\n")    
    ))

(define (t0 r)
  (let ((rx (parse-rx r)))
    (let-values (((nfa nstates) (rx->nfa rx (starts-with r ".*"))))
      (printf (rx-repr rx) "\n")
      (print-nfa nfa)
      (printf (int nstates) " states\n")
      (let ((nfa0 (nfa->map nfa nstates))
	    (tdfa (nfa->tdfa nfa0)))
	(dump-flat-dfa tdfa.machine)
	(printf (int tdfa.nregs) " registers\n")
	(printf "finals:\n")
	(for-map f insns tdfa.finals
	  (printf "  " (int f) ": " (tag-set-repr insns) "\n"))
	(printf "\n")
	tdfa
	))))

;(test-partition)
;(test-substate-range)
;(t0 "{[abc]|[bcd]|[cde]}")
;(t0 "(a|b)c")
;(t0 "{a(b|c)+d*}")
;(t0 ".*({BBB[-]BB[-]BBBB}|{BBBB[-]BBBB[-]BBBB[-]BBBB})")
;(t0 ".*{BBB[-]BB[-]BBBB}")
;(t0 ".*({a+}|{b+})")
;(t0 ".*{a+}")
;(t0 ".*{({ab}|{bc}|{cd})e}")
;(t0 ".*{({ab}|{bc})e}")
;(t0 "{abc}")
;(t0 "{(ab|abb|abbb|abbbbb|ab+)=}")
;(t0 ".*{((a|b|c)+(a|b)+)=}y")

(define scheme-keywords 
  "lambda|define|if|else|cond|and|or|case|let|let\\*|letrec|begin|do|delay|set\\!"
  )

;(test-partition2)
;;(t0 (format ".*{(" scheme-keywords ")=}"))
;(t0 (format "{(" scheme-keywords ")=}"))
;(t0 "(abc)~")
;(t0 ".*{([ab]+)^([bc]+)}")
;(t0 ".*{a~}")
;(t0 ".*{a=}")
;(t0 ".*nS{(.*n[^st].*)~}n[^st]")
;(t0 ".*{([ab]+)^([bc]+)}")
;(t0 "(abc)~")
;(t0 "{.*(abc)~}")
;(t0 ".*{(abc)~x}")
;(t0 ".*{xa~y}")
;(t0 ".*a{(b+)-(bbb)}c")
;(t0 ".*({BBB[-]BB[-]BBBB}|{BBBB[-]BBBB[-]BBBB[-]BBBB})")
;(t0 ".*{(a|m)+}")
(t0 ".*{ab|bc}")


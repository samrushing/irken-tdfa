;; -*- Mode: Irken -*-

(include "lib/io.scm")
(include "tdfa.scm")
(include "emit.scm")

(define (t0 r)
  (let ((rx (parse-rx r)))
    (printf "rx: " (rx-repr rx) "\n")
    (let-values (((nfa nstates) (rx->nfa rx (starts-with r ".*"))))
      (printf (rx-repr rx) "\n")
      (print-nfa nfa)
      (printf (int nstates) " states\n")
      (let ((nfa0 (nfa->map nfa nstates))
	    (tdfa (nfa->tdfa nfa0)))
	(dump-tdfa tdfa)
	(let ((o (file-writer "t1.c" "  ")))
	  ;;(emit-python o tdfa)
	  (emit-hybrid-c o tdfa)
	  (o.close)
	  )
	))))

;(t0 ".*({a+}|{b+})")
;(t0 ".*({a+})")
;(t0 ".*{ab|bc}")

(let ((rx0 ".*({ddd[-]dd[-]dddd}|{dddd[-]dddd[-]dddd[-]dddd})")
      (rx1 (format (join "[0-9]" (string-split rx0 #\d)))))
  (printf "rx = " rx1 "\n")
  (t0 rx1))

;(t0 ".*({ddd\\-dd\\-dddd}|{dddd\\-dddd\\-dddd\\-dddd})")

;(define scheme-keywords 
;  "lambda|define|if|else|cond|and|or|case|let|let\\*|letrec|begin|do|delay|set\\!"
;  )

;(t0 (format ".*{(" scheme-keywords ")=}"))
;(t0 ".*{[A-Z][0-9]+}")
;(t0 ".*{(the|if|not|splits|future)=}")


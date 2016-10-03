;; -*- Mode: Irken -*-

(include "lib/io.scm")
(include "tdfa.scm")
(include "emit.scm")

(define (make-writer file)
  (let ((level 0))
    (define (write-string s)
      (write file.fd
	     (format (repeat level "    ") s "\n"))
      #u)
    (define (copy s)
      (write file.fd s))
    (define (indent) (set! level (+ level 1)))
    (define (dedent) (set! level (- level 1)))
    (define (close-file) (close file.fd))
    {write=write-string indent=indent dedent=dedent copy=copy close=close-file}
    ))

(define (t0 r)
  (let ((rx (parse-rx r)))
    (printf "rx: " (rx-repr rx) "\n")
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
	(let ((ofile (file/open-write "t1.py" #t #o644))
	      (o (make-writer ofile)))
	  (emit-python o tdfa)
	  (o.close)
	  )
	))))

;(t0 ".*({a+}|{b+})")
;(t0 ".*({a+})")
;(t0 ".*{ab|bc}")
(t0 ".*({BBB\\-BB\\-BBBB}|{BBBB\\-BBBB\\-BBBB\\-BBBB})")

;(define scheme-keywords 
;  "lambda|define|if|else|cond|and|or|case|let|let\\*|letrec|begin|do|delay|set\\!"
;  )

;(t0 (format ".*{(" scheme-keywords ")=}"))


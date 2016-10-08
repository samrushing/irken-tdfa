;; -*- Mode: Irken -*-

(define (machine/make tdfa)
  (let ((finals (make-vector (vector-length tdfa.machine) (set/empty))))
    (for-map s tags tdfa.finals
      (set! finals[s] tags))
    {regs=(make-vector tdfa.nregs 0) dfa=tdfa state=0 final=#f finals=finals}))

(define (machine/insns m p insns)
  (for-list insn insns
    (match insn with
      {src=-2  dst=dst} -> (set! m.regs[dst] p)
      {src=src dst=dst} -> (set! m.regs[dst] m.regs[src])
      )))

(define (regs-repr regs)
  (let ((r '()))
    (for-vector reg regs
      (PUSH r (format (lpad 2 (int reg)))))
    (format "[" (join " " (reverse r)) "]")))

(define (machine/feed m block vpos callback verbose?)

  (define (do-final tags)
    (let ((taghits '()))
      (for-set tag tags
	(printf "hit " (int tag.tn) " " (int m.regs[tag.ti]) "\n")
	(PUSH taghits (:tuple tag.tn m.regs[tag.ti])))
      (callback (reverse taghits))))

  (define (do-tran i tran)
    (set! m.state tran.ts)
    (match m.finals[m.state] with
      (tree:empty) -> #u
      tags	   -> (do-final tags))
    (machine/insns m (+ i 1 vpos) tran.insns)
    )

  (let ((blen (string-length block)))
    (let loop0 ((i 0))
      (when verbose? 
	(printf "> i " (lpad 3 (int i)) " s " (lpad 3 (int m.state)) " " (regs-repr m.regs) "\n"))
      (if (< i blen)
	  (let loop1 ((trans m.dfa.machine[m.state]))
	    (match trans with
	      ;; note: no need to test last charset range, it
	      ;;   has to match.
	      (tran)
	      -> (begin
		   (do-tran i tran)
		   (loop0 (+ i 1)))
	      (tran . trans)
	      -> (cond ((charset/in tran.sym (string-ref block i))
			(do-tran i tran)
			(loop0 (+ i 1)))
		       (else (loop1 trans)))
	      () ;; NOTREACHED
	      -> (loop0 (+ i 1))
	      ))
	  ))
    ))

	   
	     
    
    

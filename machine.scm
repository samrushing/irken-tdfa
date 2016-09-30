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

(define (machine/feed m block vpos callback)

  (define (do-final tags)
    (let ((taghits '()))
      (for-set tag tags
	(PUSH taghits (:tuple tag.tn m.regs[tag.ti])))
      (callback (reverse taghits))))

  (define (do-tran i tran)
    (set! m.state tran.ts)
    (machine/insns m (+ i 1 vpos) tran.insns)
    (match m.finals[m.state] with
      (tree:empty) -> #u
      tags	   -> (do-final tags)))

  (let ((blen (string-length block)))
    (let loop0 ((i 0))
      ;;(printf "> i " (int i) " s " (int m.state) "\n")
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
	  )
      )
    )
  )

	   
	     
    
    

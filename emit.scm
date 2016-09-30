;; -*- Mode: Irken -*-

(define (emit-python o tdfa)

  (define charset-test
    (charset:t s) 
    -> (let ((parts '()))
	 (for-map lo hi s
	   (PUSH parts (format (int lo) " <= ch < " (int hi))))
	 (format (join " or " parts))))

  (define (tags->list tags)
    (let ((result '()))
      (for-set tag tags
	(PUSH result (format "[" (int tag.tn) "," (int tag.ti) "]")))
      (format "[" (join "," (reverse result)) "]")))

  (define (emit-finals)
    (let ((result '()))
      (o.write "self.finals = [")
      (o.indent)
      (for-range i (vector-length tdfa.machine)
	(match (tree/member tdfa.finals < i) with
	  (maybe:yes tags)
	  -> (o.write (format (tags->list tags) ","))
	  (maybe:no)
	  -> (o.write "False,")))
      (o.dedent)
      (o.write "]")
      ))

  (o.write "class machine:")
  (o.indent)
  (o.write "def __init__ (self):")
  (o.indent)
  (o.write (format "self.regs = [0] * " (int tdfa.nregs)))
  (emit-finals)
  (o.write "self.state = 0")
  (o.dedent)
  (o.write "def feed (self, block, p0, callback):")
  (o.indent)
  (o.write "regs = self.regs")
  (o.write "state = self.state")
  ;; (o.write "final0 = False")
  ;; (o.write "final1 = False")
  (o.write "for i in range (len (block)):")
  (o.indent)
  (o.write "ch = ord (block[i])")
  (o.write "p = p0 + i")
  (for-range i (vector-length tdfa.machine)
    (if (= i 0)
	(o.write "if state == 0:")
	(o.write (format "elif state == " (int i) ":")))
    (o.indent)
    (let ((flag #t))
      (for-list tran tdfa.machine[i]
	(if flag
	    (o.write (format "if " (charset-test tran.sym) ":"))
	    (o.write (format "elif " (charset-test tran.sym) ":")))
	(set! flag #f)
	(o.indent)
	(for-list insn tran.insns
	  (if (= insn.src -2)
	      (o.write (format "regs[" (int insn.dst) "] = p"))
	      (o.write (format "regs[" (int insn.dst) "] = regs[" (int insn.src) "]"))))
	(o.write (format "state = " (int tran.ts)))
	(o.dedent)))
    (o.dedent))
  (o.write "probe = self.finals[state]")
  (o.write "if probe is not False:")
  (o.indent)
  (o.write "final0 = [(tn, regs[ti]) for tn, ti in probe]")
  (o.write "callback (final0)")
  (o.dedent)
  (o.dedent)
  (o.write "self.state = state")
  ;; (o.write "self.final0 = final0")
  ;; (o.write "self.final1 = final1")
  (o.dedent)
  )

    
      
    


  
      
    

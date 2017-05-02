;; -*- Mode: Irken -*-

(define (make-writer file indent-string)
  (let ((level 0))
    (define (write-string s)
      (write file.fd
	     (format (repeat level indent-string) s "\n"))
      #u)
    (define (copy s)
      (write file.fd s))
    (define (indent) (set! level (+ level 1)))
    (define (dedent) (set! level (- level 1)))
    (define (close-file) (close file.fd))
    (define (newline) (write file.fd "\n"))
    {write=write-string
     indent=indent 
     dedent=dedent 
     copy=copy 
     close=close-file 
     newline=newline}
    ))

(define (file-writer path indent-string)
  (let ((file (file/open-write path #t #o644)))
    (make-writer file indent-string)))

;; note: assumes 'o'.
(defmacro with-indent
  (with-indent body ...)
  -> (begin (o.indent) body ... (o.dedent)))

(define (emit-python o tdfa)

  (define (charset-test s)
    (let ((parts '()))
      (for-list r s
	(PUSH parts (format (int r.lo) " <= ch < " (int r.hi))))
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
	(match (tree/member tdfa.finals int-cmp i) with
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

;; direct version (no tables for insns)
;; note: not finished!
(define (emit-c o tdfa)

  (define (charset-test s)
    (let ((parts '()))
      (for-list r s
	(if (= (- r.hi 1) r.lo)
	    (PUSH parts (format "ch==" (int r.lo)))
	    (PUSH parts (format "ch>=" (int r.lo) " && ch<" (int r.hi)))))
      (format (join " || " parts))))

  (define (emit-tran tran)
    (with-indent
     (for-list insn tran.insns
       (if (= insn.src -2)
	   (o.write (format "r" (int insn.dst) " = i;"))
	   (o.write (format "r" (int insn.dst) " = r" (int insn.src) ";"))))
     (o.write (format "state = " (int tran.ts) ";"))
     ))

  (o.write "typedef struct {")
  (with-indent
   (o.write "unsigned int state;")
   (o.write "unsigned int vpos;")  
   (for-range i tdfa.nregs
     (o.write (format "unsigned int r" (int i) ";"))))
  (o.write "} tdfa_state;")
  
  (o.write "void tdfa_search (")
  (with-indent
   (o.write "tdfa_state * search,")
   (o.write "unsigned char * buffer,")
   (o.write "unsigned int buffer_len,")
   (o.write "unsigned int * start,")
   (o.write "void (*callback)(int, int, int)"))
  (o.write ")")
  (o.write "{")
  (with-indent
   (o.write "unsigned int i = *start;")
   (o.write "unsigned int final=0;")
   ;; copy in
   (o.write "unsigned int state= search->state;")
   (for-range i tdfa.nregs
     (o.write (format "unsigned int r" (int i) " = search->r" (int i) ";")))
   (o.write "for(; i < buffer_len; i++) {")
   (with-indent
    (o.write "unsigned char ch = buffer[i];")
    (o.write "switch (state) {")
    (for-range i (vector-length tdfa.machine)
      (with-indent
       (o.write (format "case " (int i) ":"))
       (let loop ((trans tdfa.machine[i])
		  (first #t))
	 (match trans with
	   ()
	   -> (o.write "}")
	   (tran) 
	   -> (begin
		(o.write "} else { ")
		(emit-tran tran)
		(o.write "}"))
	   (tran . trans)
	   -> (begin
		(if first
		    (o.write (format "if (" (charset-test tran.sym) ") {"))
		    (o.write (format "} else if (" (charset-test tran.sym) ") {")))
		(emit-tran tran)
		(loop trans #f)
		)
	   ))
       (o.write "break;")
       ))
    (o.write "}"))
   (o.write "}")
   ;; copy out
   (for-range i tdfa.nregs
     (o.write (format "search->r" (int i) " = r" (int i) ";")))
   (o.write "search->state = state;")
   )
  (o.write "}"))
    
;; a hybrid between code-gen and tables - emit code to 
;;  test character ranges, but use tables for insns and
;;  finals.  This is suitable for very large machines.
(define (emit-hybrid-c o tdfa)

  (define (charset-test s)
    (let ((parts '()))
      (for-list r s
	(if (= (- r.hi 1) r.lo)
	    (PUSH parts (format "ch==" (int r.lo)))
	    (PUSH parts (format "ch>=" (int r.lo) " && ch<" (int r.hi)))))
      (format (join " || " parts))))

  (define (finsn insn)
    (format "{" (int insn.src) ", " (int insn.dst) "}"))

  (define (ftag tag)
    (format "{" (int tag.tn) ", " (int tag.ti) "}"))

  (define (emit-lines lines)
    (for-list line lines
      (o.write line)))

  (define (emit-perform-insns)
    ;; emit code to perform insns
    (emit-lines 
     '("while (1) {"
       "  insn ti = uinsns[insns++];"
       "  if (ti.src == -1) {"
       "    break;"
       "  } else if (ti.src == -2) {"
       "    search->regs[ti.dst] = search->vpos + i + 1;" ;; off by one, why?
       "  } else {"
       "    search->regs[ti.dst] = search->regs[ti.src];"
       "  }"
       "}")))

  (define (emit-finals-test)
    (emit-lines 
     '("if (finals[state] != -1) {"
       "  int fpos = finals[state];"
       "  while (utags[fpos].tag != -1) {"
       "    int group_num = utags[fpos].tag / 2;"
       "    int start = search->regs[utags[fpos].reg];"
       "    int stop = search->regs[utags[fpos+1].reg];"
       "    callback (group_num, start, stop);"
       "    fpos += 2;"
       "  }"
       "}")))

  (define (emit-finals-table ufinals f2p)
    (o.write (format "int finals[" (int (vector-length tdfa.machine)) "] = {"))
    (with-indent
     (let ((line (list:nil)))
       (for-range i (vector-length tdfa.machine)
	 (match (tree/member tdfa.finals int-cmp i) with
	   (maybe:yes tags)
	   -> (let ((index (cmap/add ufinals tags)))
		(PUSH line (int->string (tree/get f2p int-cmp index))))
	   (maybe:no) 
	   -> (PUSH line "-1"))
	 (when (= 19 (mod i 20))
	   (o.write (format (join "," (reverse line)) ","))
	   (set! line (list:nil))))
       (o.write (format (join "," (reverse line)) "};"))
       )))

  (let ((uinsns (tdfa->insns-cmap tdfa))
	(i2p (tree/empty))
	(ufinals (tdfa->finals-cmap tdfa))
	(f2p (tree/empty))
	(ipos 0)
	(fpos 0))

    (define (emit-tran tran)
      (with-indent
       (let ((index (cmap->index uinsns tran.insns))
	     (pos (tree/get i2p int-cmp index)))
	 (o.write (format "insns = " (int pos) ";"))
	 (o.write (format "state = " (int tran.ts) ";"))
	 )))

    ;; emit all insns in a flat array, separated by {-1,-1} sentinels
    ;;  keep track of the position of each unique insn-list and set a
    ;;  that as a pointer at each state transition.
    (o.newline)
    (emit-lines 
     '("/* generated by irken-tdfa */"
       ""
       "#include <stdint.h>"
       "typedef struct {"
       "  int8_t src;"
       "  int8_t dst;"
       "} insn;"))
    (o.newline)
    (o.write "insn uinsns[] = {")
    (with-indent
     (for-map index insns uinsns.rev
       (tree/insert! i2p int-cmp index ipos)
       (o.write (format (join finsn "," insns) ",{-1,-1},"))
       (set! ipos (+ ipos 1 (length insns))))
     (o.write "{-1,-1}};"))
    (o.newline)
    (emit-lines 
     '("typedef struct {"
       "  int8_t tag;"
       "  int8_t reg;"
       "} tag;"))
    (o.newline)
    (o.write "tag utags[] = {")
    (with-indent 
     (for-map index tags ufinals.rev
       (tree/insert! f2p int-cmp index fpos)
       (o.write (format (join ftag "," (set->list tags)) ",{-1,-1},"))
       (set! fpos (+ fpos 1 (set/size tags))))
     (o.write "{-1,-1}};"))
    (o.newline)

    (emit-finals-table ufinals f2p)
    (o.newline)

    (o.write "typedef struct {")
    (with-indent
     (o.write "unsigned int state;")
     (o.write "unsigned int vpos;")  
     (o.write (format "unsigned int regs[" (int tdfa.nregs) "];")))
    (o.write "} tdfa_state;")
    (o.newline)  
    (emit-lines 
     '("void tdfa_search ("
       "  tdfa_state * search,"
       "  unsigned char * buffer,"
       "  unsigned int buffer_len,"
       "  unsigned int * start,"
       "  void (*callback)(int, int, int)"
       ")"))
    (o.write "{")
    (with-indent
     (o.write "unsigned int i = *start;")
     (o.write "unsigned int final = 0;")
     ;; copy in
     (o.write "unsigned int state = search->state;")
     (o.write "unsigned int insns = 0;")
     (o.write "for(; i < buffer_len; i++) {")
     (with-indent
      (o.write "unsigned char ch = buffer[i];")
      (o.write "switch (state) {")
      (for-range i (vector-length tdfa.machine)
	(with-indent
	 (o.write (format "case " (int i) ":"))
	 (let loop ((trans tdfa.machine[i])
		    (first #t))
	   (match trans with
	     ()
	     -> (o.write "}")
	     (tran) 
	     -> (begin
		  (o.write "} else { ")
		  (emit-tran tran)
		  (o.write "}"))
	     (tran . trans)
	     -> (begin
		  (if first
		      (o.write (format "if (" (charset-test tran.sym) ") {"))
		      (o.write (format "} else if (" (charset-test tran.sym) ") {")))
		  (emit-tran tran)
		  (loop trans #f)
		  )
	     ))
	 (o.write "break;")
	 ))
      (o.write "}") ;; switch
      (emit-perform-insns)
      (emit-finals-test))
     (o.write "}") ;; for
     ;; copy out
     (o.write "search->state = state;")
     ) ;; body indent
    (o.write "}") ;; body
    ))

(define (tdfa->dot o tdfa)

  (define (insns->label insns)
    (format 
     (join "\\n" 
	   (map (lambda (insn)
		  (format (if (= insn.src -2) "p" (int->string insn.src))
			  "->" (int insn.dst)))
		insns))))

  (define (tags->label state tags)
    (let ((result '()))
      (for-set tag tags
	(PUSH result (format (int tag.tn) ":" (int tag.ti))))
      (format (int state) "\\n" (join "\\n" (reverse result)))))

  (o.write "digraph xxx {")
  (o.indent)
  (o.write "size=\"8,5\"")
  (o.write "node [shape = circle];")
  (o.write "rankdir = LR;")
  (o.write "edge [fontsize = 10];")
  (for-range i (vector-length tdfa.machine)
    (match (tree/member tdfa.finals int-cmp i) with
      (maybe:yes tags)
      -> (o.write (format "node [ shape = doublecircle, label = \"" 
			  (tags->label i tags) "\" ] "
			  (int i) ";"))
      (maybe:no)
      -> (o.write (format "node [ shape = circle, label = \"" 
			  (int i) "\" ] " (int i) ";"))
      ))
  (for-range i (vector-length tdfa.machine)
    (for-list tran tdfa.machine[i]
      (o.write (format (int i) 
		       " -> " (int tran.ts) 
		       " [ label = \"" 
		       (charset-repr tran.sym) "\\n"
		       (insns->label tran.insns)
		       "\" ];"))))
  (o.dedent)
  (o.write "}")
  )

(define (tnfa->dot o nfa)
  
  (o.write "digraph xxx {")
  (o.indent)
  (o.write "size=\"8,5\"")
  (o.write "node [shape = circle];")
  (o.write "rankdir = LR;")
  (o.write "edge [fontsize = 10];")

  (define epsilon "&#x3b5;")

  (match nfa with
    (nfa:t start ends trans)
    -> (let ((states (nfa->states nfa)))
	 (for-set state states
	   (o.write (format "node [ shape = "
			    (if (set/member ends int-cmp state)
				"doublecircle"
				"circle")
			    "] " (int state))))
	 (for-list tran trans
	   (match tran with
	     (tran:t fs ts sym)
	     -> (let ((label 
		       (match sym with
			 (sym:t cs) -> (charset-repr cs)
			 (sym:epsilon tag) -> (format epsilon (int tag)))))
		  (o.write (format (int fs) " -> " (int ts) " [ label = \"" label "\" ];")))
	     ))))

  (o.dedent)
  (o.write "}")
  )

    


  
      
    

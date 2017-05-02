;; -*- Mode: Irken -*-

(include "nfa.scm")
(include "reorder.scm")
(include "hopcroft.scm")

;; nfa-target	:= {ts=int sym=sym}
;; nfa		:= {start=int end=int map=(vector (list nfa-target))}
;; tag		:= {tn=int ti=int}
;; substate	:= {u=int p=int k=(set tag)})
;; superstate	:= (set substate)

;; a <substate> represents an nfa state:
;;  u := nfa state number
;;  p := priority
;;  k := tag set (tn.ti := tag-number <tn> with subscript <ti>)
;; a <superstate> represents a set of substates, i.e. a dfa state.

(define tag-cmp
  {tn=an ti=ai} {tn=bn ti=bi}
  -> (match (int-cmp an bn) with
       (cmp:<) -> (cmp:<)
       (cmp:>) -> (cmp:>)
       (cmp:=) -> (int-cmp ai bi)))

(define (tag-set-cmp a b)
  (set/cmp a b tag-cmp))

(define (tag-set-repr k)
  (let ((parts '()))
    (for-set x k
      (PUSH parts (format (int x.tn) "." (int x.ti))))
    (format "<" (join " " (reverse parts)) ">")))

(define (substate-cmp a b)
  (let ((cmpu (int-cmp a.u b.u)))
    (if (eq? cmpu (cmp:=))
        (let ((cmpp (int-cmp a.p b.p)))
          (if (eq? cmpp (cmp:=))
              (tag-set-cmp a.k b.k)
              cmpp))
        cmpu)))

;; ---------------------------------------------------------------------
;; The following sort functions are used to provide an ordering that
;; only considers u (nfa state) and tag numbers (k[i].tn).  This
;; allows us to quickly identify superstates that are congruent via a
;; permutation of tag indices (i.e., registers).

(define (tag-perm-cmp a b)
  (int-cmp a.tn b.tn)
  )

(define (tag-set-perm-cmp a b)
  (set/cmp a b tag-perm-cmp))

(define (substate-perm-cmp a b)
  (match (int-cmp a.u b.u) with
    (cmp:<) -> (cmp:<)
    (cmp:>) -> (cmp:>)
    (cmp:=) -> (tag-set-perm-cmp a.k b.k)))

;; this is as bad as it gets. a set of sets of things that also contain sets.
;; [why this is not written in C]
(define superstate-cmp (make-set-cmp substate-perm-cmp))

;; ---------------------------------------------------------------------

(define (substate-repr s)
  (if (= s.p 0)
      (format "(" (int s.u) " " (tag-set-repr s.k) ")")
      (format "{u=" (int s.u) " p=" (int s.p) " k=" (tag-set-repr s.k) "}")))

(define (superstate-repr s)
  (let ((parts (set->list s)))
    (format "[" (join substate-repr " " parts) "]")))

;; given a set of charsets, create a list of unique disjoint sets that
;;  covers '.'.  These new sets will represent all possible unique
;;  transitions out of a given NFA state (i.e.  all possible unique
;;  sets of of NFA states, thus possibly unique DFA states).

;; We do this by iterating over each range of each charset.
;; The start and end of each range indicates a 'cutpoint', where
;;   a new result range will start.  The ranges between all cutpoints
;;   define the partitioning of all char-space.

;; Possible optimization: if A,B,and C are charsets, then NOT(A|B|C)
;;   will (possibly) be split into multiple ranges, when it could be
;;   represented as a single charset.  This will only matter with
;;   pathological charsets, (say, A,B,C pick 100 even-numbered chars,
;;   leaving 100 odd holes).

;; XXX consider memoizing this.

(define (make-partition charsets)
  (let ((cutpoints (set/empty)))
    (set/add! cutpoints int-cmp 0)
    (set/add! cutpoints int-cmp 256)    
    (for-set c charsets
      (for-list r c
	(set/add! cutpoints int-cmp r.lo)
	(set/add! cutpoints int-cmp r.hi)))
    ;;(printf "cutpoints: " (join int->string "," (set->list cutpoints)) "\n")
    (let ((start -1)
	  (r '()))
      (for-set cut cutpoints
	(PUSH r (charset/range start cut))
	(set! start cut))
      ;; ignore the bogus (-1,0) range we started with
      (let ((r1 (cdr (reverse r))))
	(if (not (set/member cutpoints int-cmp #xff))
	    (let ((merged (charset/merge (first r1) (last r1))))
	      (list:cons merged (butlast (cdr r1))))
	    (reverse r)))
      )))

(define (dump-states states)
  (printf "states: {\n")
  (for-map super index states
    (printf "  " (int index) " " (superstate-repr super) "\n"))
  (printf "}\n"))

(define (nfa->tdfa nfa)
  (let ((dfa0 (set/add (set/empty) substate-cmp {u=nfa.start p=0 k=(set/empty)}))
	(dfa-index 0)
	;; a set/map from superstate to index (XXX can we use cmap?)
	(superstates (tree/empty))
	;; map from index->superstate
	(ssrevmap (tree/empty))
	;; holds dfa transitions as we build them.
	;; (this will later be flattened).
	(dfa-trans (set/empty))
	)

    ;; add a new superstate to the dfa.
    (define (add-superstate superstate)
      (let ((index dfa-index))
	;; add to the forward map (note: uses the map-as-set value)
	(set/addv! superstates superstate-cmp superstate dfa-index)
	;; reverse map from index->superstate
	(tree/insert! ssrevmap int-cmp dfa-index superstate)
	(set! dfa-index (+ dfa-index 1))
	index))

    (define (state->index superstate)
      (match (tree/member superstates superstate-cmp superstate) with
	(maybe:yes index) -> index
	(maybe:no) -> (error1 "state->index: no such state" superstate)
	))

    (define (index->state index)
      (match (tree/member ssrevmap int-cmp index) with
	(maybe:yes state) -> state
	(maybe:no) -> (error1 "index->state: no such index" index)
	))

    ;; get the next available map index (or a new slot) for <tagnum>
    ;; [this is like register allocation for the tag insns]
    (define (get-next-slot superstate tagnum)
      (let ((taken (set/empty))
    	    (n 0))
    	(for-set x superstate
    	  (for-set ki x.k
    	    (when (= ki.tn tagnum)
    		  (set/add! taken int-cmp ki.ti)
    		  (set! n (+ n 1)))))
    	(let/cc return
    	    (for-range i n
    	      (if (not (set/member taken int-cmp i))
    		  (return i)))
    	  n)))
	  
    ;; scan through the closure, removing redundant states by keeping
    ;;  the lowest priority one.  [since our NFA is numbered left to
    ;;  right, this gives us the leftmost-longest behavior expected of
    ;;  search].
    ;;
    ;; Note: I'm pretty sure that having explicit control over priority
    ;;  would give complete control over greediness.  This could allow
    ;;  the addition of 'non-greedy' operators.  This would be best
    ;;  introduced with an s-expression syntax.
    (define (keep-lowest-priority closure0)
      (let ((closure1 closure0)
	    (last {u=-1 p=0 k=(set/empty)}))
	(for-set x closure0
	  ;; as we scan through the (sorted) set, if we come across
	  ;;  a duplicate substate, remove it. [favoring the lower
	  ;;  priority]
	  ;; KEEP LOWEST
	  (if (= x.u last.u)
	     (set/delete! closure1 substate-cmp x)
	     (set! last x))
	  ;; KEEP HIGHEST
	  ;;(if (= x.u last.u)
	  ;;    (set/delete! closure1 substate-cmp last))
	  ;;(set! last x)
	  )
	;; (when (not (= (set/size closure0) (set/size closure1)))
	;;   (printf "KLP -- " (superstate-repr closure0) "\n")
	;;   (printf "KLP ++ " (superstate-repr closure1) "\n\n"))
	(check-closure closure1)
	closure1))

    (define (check-closure closure)
      (let ((last -1))
	(for-set x closure
	  (when (= x.u last)
	    (printf "bad closure " (superstate-repr closure) "\n")
	    (error "bad closure"))
	  (set! last x.u))))

    (define (epsilon-closure superstate)
      (let ((stack (set->list superstate))
	    (new-slots (set/empty))
	    (closure superstate))
	(let loop ()
	  (match stack with
	    () -> (:tuple (keep-lowest-priority closure) new-slots)
	    (hd . tl)
	    -> (begin 
		 (set! stack tl) ;; i.e. "pop".
		 (for-list x nfa.map[hd.u]
		    (match x.sym with
		      (sym:epsilon tag)
		      -> (let ((k2 (set/empty)))
			   ;; remove any map items with the same tag
			   (for-set y hd.k
			     (if (not (= y.tn tag))
				 (set/add! k2 tag-cmp y)))
			   (let ((next-slot (get-next-slot superstate tag))
				 (tag0 {tn=tag ti=next-slot}))
			     (set/add! new-slots tag-cmp tag0)
			     (set/add! k2 tag-cmp tag0)
			     ;; use the sum of starting and ending state number as priority
			     ;;(let ((new-substate {u=x.ts p=(+ hd.p hd.u x.ts) k=k2}))
			     (let ((new-substate {u=x.ts p=(+ hd.u x.ts) k=k2}))
			       ;;(printf "epsln from " (int hd.u) " to " (int x.ts) " " (substate-repr new-substate) "\n")
			       (set! closure (keep-lowest-priority (set/add closure substate-cmp new-substate)))
			       ;; if we added a new state, push it on the stack.
			       (if (set/member closure substate-cmp new-substate)
				   (PUSH stack new-substate))
			       #u
			       )))
		      _ -> #u
		      ))
		 (loop))
	    ))))

    ;; collect all substates reachable from this superstate with this symbol.
    (define (reach state symbol)
      (let ((result (set/empty)))
	(for-set x state
	  (for-list y nfa.map[x.u]
	     (match y.sym with
	       (sym:t sym0)
	       -> (if (charset/overlap? symbol sym0)
		      (begin
			(let ((new-substate {u=y.ts p=(+ 1 x.p x.u y.ts) k=x.k}))
			  ;;(printf "reach: from " (int x.u) " to " (int y.ts) " " (substate-repr new-substate) "\n")
			  (set/add! result substate-cmp new-substate)))
		      #u)
	       _ -> #u
	       )))
	result))

    (define (add-tran tran)
      (let ((probe (set/getkey dfa-trans dfa-tran-cmp tran)))
	(match probe with 
	  (maybe:yes tran1) -> (set! tran1.sym (charset/merge tran1.sym tran.sym))
	  (maybe:no)        -> (set/add! dfa-trans dfa-tran-cmp tran)
	  )))

    (define (walk index state)
      (let ((moves (set/empty)))
	;; collect all possible non-epsilon moves from this superstate
	(for-set x state
	  (for-list y nfa.map[x.u]
	     (match y.sym with
	       (sym:t cs)
	       -> (set/add! moves charset-cmp cs)
	       _ -> #u
	       )))
	;;(printf "------------------------------------------------------\n")
	;;(printf "moves (before partition): ")
	;;(for-set x moves
	;;  (printf (charset-repr x) " "))
	;;(printf "\n")
	;; partition the set of moves into a disjoint set of unique symbols/charsets.
	(let ((partitions (make-partition moves)))
	  ;; (printf "moves (after partition): ")
	  ;; (for-list x partitions
	  ;;   (printf (charset-repr x) " "))
	  ;; (printf "\n")
	  (for-list sym partitions
	    ;; for each unique symbol, compute the states reachable from this superstate...
	    (let ((r (reach state sym)))
	      ;; (printf "=================\n")
	      ;; (printf "parts again: index=" (int index) " ")
	      ;; (for-list z partitions
	      ;; 	(printf (charset-repr z) " "))
	      ;; (printf "\n")
	      ;;(printf "state " (int index) " for symbol " (charset-repr sym) ":\n")
	      ;;(printf "  reach = (" (join substate-repr " " (set->list r)) ")\n")
	      ;; ... and take the epsilon closure.
	      (let-values (((closure new-slots) (epsilon-closure r)))
		(check-closure closure)
		;;(printf "closure: " (superstate-repr closure) "\n")
		;;(printf "new slots: " (tag-set-repr new-slots) "\n")
		(let ((insns '()))
		  (for-set x new-slots
		    (PUSH insns {tn=x.tn src=-2 dst=x.ti}))
		  (set! insns (reverse insns))
		  ;;(printn new-slots)
		  ;; (dump-states superstates)
		  ;; is this a new state, or can it be permuted into an old state?
		  (match (tree/member superstates superstate-cmp closure) with
		    (maybe:yes other-index)
		    -> (let ((other (index->state other-index))
			     (perm (get-permutation closure other)))
			 ;;(printf "equal to " (int other-index) " |perm|= " (int (tree/size perm)) "\n")
			 ;;(printf "  from " (superstate-repr closure) "\n")
			 ;;(printf "    to " (superstate-repr other) "\n")
			 (if (tree/empty? perm) ;; state was already equal.
			     (add-tran {fs=index sym=sym ts=other-index insns=insns})
			     (add-permuted-transition perm index other-index sym new-slots insns)
			     ))
		    (maybe:no)
		    -> (let ((new-index (add-superstate closure)))
			 ;;(printf "not equal, added: " (superstate-repr closure) "\n")
			 ;;(printf "new index: " (int new-index) "\n")
			 (add-tran {fs=index ts=new-index sym=sym insns=insns})
			 (walk new-index closure)
			 ;;#u
			 )
		    )
		  ))
	      )))))

    (define (get-permutation s0 s1)
      ;; we know that s0 and s1 are congruent modulo tag indices.
      ;;   build a (possibly empty) permutation map from s0 to s1.
      (let ((perm (tree/empty)))
	(for-set2 a b s0 s1
	  (for-set2 ta tb a.k b.k
	    (if (not (= ta.ti tb.ti))
		(tree/insert! perm tag-cmp ta tb.ti))))
	perm))

    (define (partition-perm perm)
      ;; partition a permutation by tagnum.
      (let ((tagmap (tree/empty)))

	(define (getpart tn)
	  (match (tree/member tagmap int-cmp tn) with
	    (maybe:yes part) 
	    -> part
	    (maybe:no) 
	    -> (let ((part {cell=(tree/empty)}))
		 (tree/insert! tagmap int-cmp tn part)
		 part)
	    ))

	(for-map k v perm
	  (let ((part (getpart k.tn)))
	    (tree/insert! part.cell int-cmp k.ti v)))
	tagmap))

    (define (add-permuted-transition perm from-index to-index sym new-slots insns)
      ;; note: new-slots holds what would otherwise be new registers to use
      ;;  for a *new* state.  Instead, since we are permuting, any src that
      ;;  is in the new-slots set will be replaced with 'p'.  [does this mean
      ;;  we can elide the assignments to these particular new slots?]
      ;; hmmm the more I think about this I think we could 'optimize' this with
      ;;  a second pass, cutting down on the total # of registers.
      ;;(printf "*** permutable {")
      (for-map tag part (partition-perm perm)
	(let ((moves (reorder part.cell)) ;; handle permutation cycles.
	      (tinsns '()))		   ;; insns for this tag
	  (for-list move moves
	    (PUSH tinsns {tn=tag src=move.src dst=move.dst}))
	  (set! insns (append insns tinsns))))
      (add-tran {fs=from-index ts=to-index sym=sym insns=insns})
      )

    ;; collect the valid tag sets for all final states.
    (define (find-final-states cmap)
      (let ((finals (set/empty))
	    (insns (make-vector dfa-index (set/empty)))
	    (result (tree/empty)))
	(for-map super index superstates
	  (for-set sub super
	    (when (set/member nfa.end int-cmp sub.u)
	      (set/add! finals int-cmp index)
	      (for-set tag sub.k
		(set/add! insns[index] tag-cmp {tn=tag.tn ti=tag.ti}))
	      )))
	;; map from final->tagset
	(for-set f finals
	  (for-set tag insns[f]
	     ;; translate tag->register
	     (set! tag.ti (cmap->index cmap tag)))
	  (tree/insert! result int-cmp f insns[f]))
	result
	))

    (define (flatten trans)
      (let ((machine (make-vector dfa-index '())))
	;; collect all transitions from each state
	(for-set t trans
	  (set! machine[t.fs] (list:cons t machine[t.fs])))
	;; sort them from smallest to largest charset
	(for-range i dfa-index
	  (set! machine[i] 
		(sort (lambda (a b)
			;; XXX no type error here when leaving off '.sym'!
			(< (charset/size a.sym) (charset/size b.sym)))
		      machine[i])))
	machine))

    ;; map all tags (eg '1.2') to registers ('7').
    ;;   return the modified dfa transition list as well
    ;;   as the reverse map (from register->tag).
    (define (tags->registers new-slots)
      (let ((m (cmap/make tag-cmp)))

	(define (T tn ti)
	  (cmap/add m {tn=tn ti=ti}))

	(define Tinsn
	  {tn=tn src=-2  dst=dst} -> {src=-2 dst=(T tn dst)}
	  {tn=tn src=src dst=dst} -> {src=(T tn src) dst=(T tn dst)}
	  )

	;; make sure slots that never show up in trans are included.
	(for-set x new-slots (T x.tn x.ti))

	(:tuple
	 m
	 (set-map dfa-trans dfa-tran-cmp
		  (lambda (t)
		    {fs=t.fs sym=t.sym ts=t.ts 
			     insns=(map Tinsn t.insns)})))
	))
	    
    (let-values (((initial new-slots) (epsilon-closure dfa0)))
      (add-superstate initial)
      ;; (printn (tag-set-repr new-slots))
      ;; (printf "state 0: " (superstate-repr initial) "\n")
      (walk 0 initial)
      ;; (for-map x index superstates
      ;; 	(printf (int index) " " (superstate-repr x) "\n")
      ;; 	)
      ;; (printf "dfa trans {\n")
      ;; (for-set x dfa-trans
      ;;   (printf "  " (int x.fs) " " (rpad 8 (charset-repr x.sym)) " -> " (int x.ts)
      ;; 	  "  " (join insn-repr " " x.insns) "\n"))
      ;; (printf "}\n")
      (let-values (((cmap dfa-trans0) (tags->registers new-slots)))
	;; (printf "tags->registers: {\n")
	;; (for-list x dfa-trans0
	;;   (printf "  " (lpad 3 (int x.fs)) 
	;; 	  " " (rpad 8 (charset-repr x.sym)) 
	;; 	  " -> " (lpad 3 (int x.ts))
	;; 	  "  " (join reginsn-repr " " x.insns) "\n"))
	;; (printf "}\n")
	;; (printf "finals: \n")
	(let ((finals (find-final-states cmap)))
	  ;; (for-map f insns finals
	  ;;   (printf "  " (int f) ": " (tag-set-repr insns) "\n"))
	  ;; (printf "\n")
	  {machine=(flatten dfa-trans0) finals=finals nregs=cmap.count}
	  ))
      )))

;; deliberately not comparing syms, so we can easily merge them.
(define dfa-tran-cmp
  {fs=afs sym=asym ts=ats}  {fs=bfs sym=bsym ts=bts}
  -> (match (int-cmp afs bfs) with
       (cmp:<) -> (cmp:<)
       (cmp:>) -> (cmp:>)
       (cmp:=) -> (int-cmp ats bts)))

(define insn-repr
  {tn=tn src=-2 dst=dst}
  -> (format "  p->" (int tn) "." (int dst))
  {tn=tn src=-1 dst=dst}
  -> (format "  t->" (int tn) "." (int dst))
  {tn=tn src=src dst=-1}
  -> (format (int tn) "." (int src) "->t  ")
  {tn=tn src=src dst=dst}  
  -> (format (int tn) "." (int src) "->" (int tn) "." (int dst))
  )
  
(define reginsn-repr
  {src=-2 dst=dst}
  -> (format "p->" (int dst))
  {src=src dst=dst}  
  -> (format (int src) "->" (int dst))
  )

(define (dump-tdfa tdfa)
  (printf "tdfa: {\n")
  (for-range i (vector-length tdfa.machine)
    (printf (lpad 2 (int i)) ":\n")
    (for-list j tdfa.machine[i]
      (printf (lpad 10 (charset-repr j.sym))
	      " -> " (lpad 3 (int j.ts)) " "
	      (join reginsn-repr " " j.insns)
	      "\n")))
  (printf "finals:\n")
  (for-map f insns tdfa.finals
    (printf "  " (int f) ": " (tag-set-repr insns) "\n"))
  (printf "nregs: " (int tdfa.nregs) "\n")
  (printf "}\n"))

(define (rx->tdfa r)
  (let ((rx (parse-rx r)))
    (let-values (((nfa nstates) (rx->nfa rx (starts-with r ".*"))))
      ;; (let ((charsets (nfa->charsets nfa)))
      ;; 	(printf "|charsets| = " (int (set/size charsets)) "\n")
      ;; 	(printf "|partitioned| = " (int (length (make-partition charsets))) "\n")
      ;; 	)
      (nfa->tdfa (nfa->map nfa nstates)))))

(define (list/cmp a b cmp)
  (let loop ((a a)
	     (b b))
    (match a b with
      () () -> (cmp:=)
      () _  -> (cmp:<)
      _  () -> (cmp:>)
      (ha . tla) (hb . tlb)
      -> (match (int-cmp ha hb) with
           (cmp:<) -> (cmp:<)
           (cmp:>) -> (cmp:>)
           (cmp:=) -> (loop tla tlb)))))

(define insn-cmp
  {src=sa dst=da} {src=sb dst=db}
  -> (match (int-cmp sa sb) with
       (cmp:<) -> (cmp:<)
       (cmp:>) -> (cmp:>)
       (cmp:=) -> (int-cmp da db)))

(define (insns-cmp a b)
  (list/cmp a b insn-cmp))

(define (tinsns-cmp a b)
  (match (int-cmp a.ts b.ts) with
    (cmp:<) -> (cmp:<)
    (cmp:>) -> (cmp:>)
    (cmp:=) -> (insns-cmp a.insns b.insns)))

;; collect all unique insns
(define (tdfa->insns-cmap tdfa)
  (let ((uinsns (cmap/make insns-cmp)))
    (for-range i (vector-length tdfa.machine)
      (for-list tran tdfa.machine[i]
	(cmap/add uinsns tran.insns)))
    uinsns))

;; collect all unique final tagsets
(define (tdfa->finals-cmap tdfa)
  (let ((ufinals (cmap/make tag-set-cmp)))
    (for-map f insns tdfa.finals
      (cmap/add ufinals insns))
    ufinals))

(define (tdfa-statistics tdfa)
  ;; how many unique insns?
  ;; how many unique (ts, insns)?
  (let ((uinsns (cmap/make insns-cmp))
	(utinsns (cmap/make tinsns-cmp))
	(count 0))
    (for-range i (vector-length tdfa.machine)
      (for-list tran tdfa.machine[i]
	(cmap/add uinsns tran.insns)
	(cmap/add utinsns tran)
	(set! count (+ 1 count))
	))
    (for-map k v uinsns.map
      (printf (join reginsn-repr " " k) "\n"))
    (printf (int uinsns.count) " unique insns (of " (int count) ")\n")
    (printf (int utinsns.count) " unique tinsns (of " (int count) ")\n")
    ))
	
    

;; -*- Mode: Irken -*-

;; minimize a DFA using Moore's refinement algorithm.

;; this *functions*, but fails to (fully) minimize examples from the internets.
;; thus, I am switching to the hopcroft code.

;; key := (set {sym ts})
;; refinement := (map key (set int))

(define rkeyset-cmp (make-set-cmp magic-cmp))

;; this is a somewhat-custom imperative map used
;;   to track refinements.
(define (refinement/make)

  (let ((map (tree/empty)))

    (define (add key state)
      (match (tree/member map rkeyset-cmp key) with
	(maybe:yes cell) 
	-> (set/add! cell.val int-cmp state)
	(maybe:no) 
	-> (tree/insert! map rkeyset-cmp key 
			 {val=(set/add (set/empty) int-cmp state)})
	))

    (define (getmap) map)
    {add=add getmap=getmap}
    ))

(define (minimize tdfa)

  (define superstate-cmp (make-set-cmp int-cmp))
  
  (let ((dfa tdfa.machine)
	(nstates (vector-length dfa))
	(partition (cmap/make superstate-cmp)) ;; tracks/indexes state partitions
	(final (set/empty))
	(non-final (set/empty)))
    ;; partition into final/non-final
    (for-range i nstates
      (match (tree/member tdfa.finals int-cmp i) with
	(maybe:yes _) -> (set/add! final int-cmp i)
	(maybe:no) -> (set/add! non-final int-cmp i)))
    ;; use [final, non-final] as the starting partition
    (cmap/add partition final)
    (cmap/add partition non-final)
    (let loop ()
      (printf "# partitions: " (int partition.count) "\n")
      (let ((s2p (tree/empty))
	    (new-part (cmap/make superstate-cmp)))
	;; make a state->partition map
	(for-map states index partition.map
	  (for-set s states
	    (tree/insert! s2p int-cmp s index)))
	;; create new partitioning
	(for-map states index partition.map
	  (if (= 1 (set/size states))
	      (discard (cmap/add new-part states))
	      ;; iterate over transitions
	      (let ((key (set/empty))
		    (refined (refinement/make)))
		(for-set fs states
		  (for-list tran dfa[fs]
		    (set/add! key magic-cmp {sym=tran.sym ts=(tree/get s2p int-cmp tran.ts)}))
		  ;; add this key to the refinement map
		  (refined.add key fs))
		;; build this new partition
		(for-map _ states-cell (refined.getmap)
		  (cmap/add new-part states-cell.val))
		)))
	;; did we refine anything?
	(printf "new-part.count = " (int new-part.count) "\n")
	(cond ((not (= new-part.count partition.count))
	       ;; yes, continue partitioning
	       (set! partition new-part)
	       (loop))
	      (else
	       ;; done.  make a map from old->new states
	       (let ((o2n (tree/empty))
		     (newdfa (make-vector new-part.count '())))
		 (printf "done\n")
		 (for-map states new new-part.map
		   (for-set old states
		     (tree/insert! o2n int-cmp old new)))
		 ;; translate the original dfa
		 (let ((newdfa (make-vector new-part.count '()))
		       (newfinals (tree/empty)))
		   (for-map states index new-part.map
		     (if (> (set/size states) 0)
			 ;; pick the first state of the equiv set 
			 (let ((equiv (set/least states)))
			   (for-list tran dfa[equiv]
			      (PUSH newdfa[index]
				    {ts=(tree/get o2n int-cmp tran.ts) sym=tran.sym insns='()})
			      ))))
		   ;; translate the final states
		   (for-set f tdfa.finals
		     (tree/insert! newfinals int-cmp 
				   (tree/get s2p int-cmp f) ;; translated final
				   (tree/empty)))     ;; empty insns
		   ;; the new machine (many new machines on Ix)
		   {machine=newdfa finals=newfinals nregs=0}
		   ))))
	))))

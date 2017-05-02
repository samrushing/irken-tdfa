;; -*- Mode: Irken -*-

;; https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft.27s_algorithm
;;
;; map from a key to a [mutable] cell value.

(define (cellmap/make cmp default)
  {map=(tree/empty) cmp=cmp default=default}
  )

(define (cellmap/get m key)
  (match (tree/member m.map m.cmp key) with
    (maybe:yes cell) -> cell
    (maybe:no) 
    -> (let ((cell {val=m.default}))
	 (tree/insert! m.map m.cmp key cell)
	 cell)))

;; The Xmap is used to answer the question in the sixth line,
;;   "let X be the set of states ..."
;; It provides a map of (sym, ts) -> fs
;;   where <sym> is a member of <alphabet> computed by
;;   partition-symbols.
;;
;; key := (set {sym ts})
;; map := key -> (set int))

;; XXX maybe rewrite this to use cellmap.

(define (Xmap/make)
  (let ((map (tree/empty)))

    (define rkey-cmp
      {sym=asym ts=ats} {sym=bsym ts=bts}
      -> (match (int-cmp ats bts) with
           (cmp:<) -> (cmp:<)
           (cmp:>) -> (cmp:>)
           (cmp:=) -> (charset-cmp asym bsym)))

    (define (getcell key)
      (match (tree/member map rkey-cmp key) with
	(maybe:yes cell) 
	-> cell
	(maybe:no) 
	-> (let ((cell {val=(set/empty)}))
	     (tree/insert! map rkey-cmp key cell)
	     cell)))

    (define (add key state)
      (let ((cell (getcell key)))
	(set/add! cell.val int-cmp state)))
      
    (define (get key)
      (let ((cell (getcell key)))
	cell.val))

    (define (getmap) map)
    {add=add get=get getmap=getmap}
    ))

(define (build-Xmap dfa alphabet)
  (let ((Xmap (Xmap/make)))
    (for-list sym alphabet
      (for-range i (vector-length dfa)
	(for-list tran dfa[i]
	  (if (charset/overlap? sym tran.sym)
	      (Xmap.add {sym=sym ts=tran.ts} i))
	  )))
    Xmap
    ))

(define (hopcroft tdfa)

  ;; partition all symbols into distinct subsets.
  ;; [think of this as an optimized version of the 'alphabet']
  (define (partition-symbols dfa)
    (let ((syms (set/empty)))
      (for-range i (vector-length dfa)
	(for-list tran dfa[i]
	  (set/add! syms charset-cmp tran.sym)))
      (make-partition syms)))

  (define (find-trans-to-A Xmap sym A)
    ;; use Xmap to produce X, the "set of states for which a transition
    ;; on c leads to a state in A"
    (let ((result (set/empty)))
      (for-set s A 
	(set! result (set/union int-cmp result (Xmap.get {sym=sym ts=s}))))
      result))

  (define (super-repr s)
    (format "{" (join int->string " " (set->list s)) "}"))

  (define super-cmp (make-set-cmp int-cmp))

  (define (merge-transactions dfa)
    ;; simplify the DFA with possibly/partially-redundant transitions
    ;; e.g., if 0 -> 1 with a|b|c, turn this into 0 -> 1 [abc]
    (let ((nstates (vector-length dfa))
	  (newdfa (make-vector nstates '())))
      (for-range i nstates
	(let ((m (cellmap/make int-cmp (charset/empty))))
	  (for-list tran dfa[i]
		    (let ((cell (cellmap/get m tran.ts)))
		      (set! cell.val (charset/merge cell.val tran.sym))))
	  (for-map k v m.map
	    (PUSH newdfa[i] {ts=k sym=v.val insns='()}))))
      newdfa))

  ;; translate the machine given the partitioning P
  (define (translate tdfa P)
    ;; build the reverse map to translate the dfa
    (let ((o2n (tree/empty))
	  (i 0))
      ;; note: since the set is sorted, the start state 0 will always
      ;;  be in the first super-set.
      (for-set x P
	(for-set y x
	  (tree/insert! o2n int-cmp y i))
	(set! i (+ i 1)))
      ;; translate the dfa
      (let ((machine (make-vector i '()))
	    (finals (tree/empty)))
	(for-range i (vector-length tdfa.machine)
	  (for-list tran tdfa.machine[i]
	    (let ((fs0 (tree/get o2n int-cmp i))
		  (ts0 (tree/get o2n int-cmp tran.ts)))
	      (PUSH machine[fs0] {ts=ts0 sym=tran.sym insns=(list:nil)}))))
	;; translate final states
	(for-set f tdfa.finals
	  (tree/insert! finals int-cmp (tree/get o2n int-cmp f) (set/empty)))
	{machine=(merge-transactions machine) finals=finals nregs=0}
	)))

  ;; P := {F, Q \ F};
  ;; W := {F};
  ;; while (W is not empty) do
  ;;      choose and remove a set A from W
  ;;      for each c in Σ do
  ;;           let X be the set of states for which a transition on c leads to a state in A
  ;;           for each set Y in P for which X ∩ Y is nonempty and Y \ X is nonempty do
  ;;                replace Y in P by the two sets X ∩ Y and Y \ X
  ;;                if Y is in W
  ;;                     replace Y in W by the same two sets
  ;;                else
  ;;                     if |X ∩ Y| <= |Y \ X|
  ;;                          add X ∩ Y to W
  ;;                     else
  ;;                          add Y \ X to W
  ;;           end;
  ;;      end;
  ;; end;

  (let ((alphabet (partition-symbols tdfa.machine))
	(Xmap (build-Xmap tdfa.machine alphabet))
	(F (set/empty))
	(N (set/empty))
	(P (set/empty))
	(W (set/empty)))
    ;; partition into final/non-final
    (for-range i (vector-length tdfa.machine)
      (match (tree/member tdfa.finals int-cmp i) with
	(maybe:yes _) -> (set/add! F int-cmp i)
	(maybe:no)    -> (set/add! N int-cmp i)))
    (set/add! P super-cmp F N)
    (set/add! W super-cmp F)
    ;;(printf "|W| = " (int (set/size W)) "\n")
    (while (not (set/empty? W))
      (let ((A (set/pop-least! W super-cmp)))
	;;(printf "A: " (super-repr A) "\n")
	(for-list c alphabet
	  (let ((X (find-trans-to-A Xmap c A))
		(P0 P))
	    (for-set Y P
	      (let ((X∩Y (set/intersection int-cmp X Y))
		    (Y\X (set/difference int-cmp Y X)))
		(when (and (not (set/empty? X∩Y))
			   (not (set/empty? Y\X)))
		  (set/replace! P0 super-cmp Y X∩Y Y\X)
		  (cond ((set/member W super-cmp Y)
			 (set/replace! W super-cmp Y X∩Y Y\X))
			((<= (set/size X∩Y) (set/size Y\X))
			 (set/add! W super-cmp X∩Y))
			(else
			 (set/add! W super-cmp Y\X)))
		  )))
	    (set! P P0)
	    ))
	))
    (translate tdfa P)
    ))



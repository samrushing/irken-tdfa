;; -*- Mode: Irken -*-

(include "rx.scm")
(include "lib/counter.scm")
(include "set.scm")
(include "cmap.scm")

;; note: nfa.scm and tdfa.scm are mutually dependent.

;; consider rewriting with tran/nfa as records rather than datatypes.

(datatype sym
  (:t charset)    ;; charset
  (:epsilon int)  ;; tagnum
  )

(datatype tran	   ;; transition
  (:t int int sym) ;; start-state end-state sym
  )

;; note: (tree int bool) currently equals (set int)
(datatype nfa ;; non-deterministic finite automata
  (:t int (tree int bool) (list tran)) ;; start end-states trans
  )

(define sym-repr
  (sym:t s) -> (charset-repr s)
  (sym:epsilon tag) -> (format "e" (int tag))
  )

(define print-nfa
  (nfa:t start ends trans)
  -> (begin
       (printf "tnfa {\n")
       (for-list t trans
	 (match t with
	   (tran:t start end sym)
	   -> (printf (lpad 3 (int start)) " "
		      (rpad 8 (sym-repr sym)) " -> "
		      (lpad 3 (int end)) "\n")))
       (printf "  start = " (int start) "\n")
       (printf "  end   = " (join int->string "," (set->list ends)) "\n")
       (printf "}\n")
       ))

(define nfa->states
  (nfa:t start ends trans)
  -> (let ((states ends))
       (set/add! states < start)
       (for-list t trans
	 (match t with
	   (tran:t start end _)
	   -> (begin (set/add! states < start)
		     (set/add! states < end))))
       states))

;;; This is the heart of the regexp->nfa conversion algorithm.
;;; The technique used here generates a 'glushkov' NFA, one that
;;; has no epsilon transitions.  Ok, I lied.  Tagged epsilons are
;;; left in.  So there are no 'untagged' epsilons.  The goal is
;;; to start with a smaller NFA to make the DFA conversion faster
;;; and smaller.
;;;
;;; Note: we can't minimize the resulting DFAs without losing the group tags.
;;;
;;; See "Handbook of Formal Languages" (Rozenberg/Salomaa Eds.), section 3.2.2 p.72.

;;; N.B: the state numbers generated during this traversal are NOT
;;;  arbitrary.  They are used to compute priorities during the
;;;  dfa conversion, and therefore affect the greediness of the
;;;  sub-expressions (important when group matching).
;;;  This manifests itself in the special care taken choosing the
;;;  order to walk sub-expressions, and especially in the way
;;;  the tagged epsilons bracket their sub-expression.

(define (rx->nfa rx search?)

  (let ((count (make-counter 0))
	(tag (make-counter 0)))

    (define (sym->nfa sym)
      (let ((s (count.inc))
	    (e (count.inc)))
	(nfa:t s (set/make < e) (LIST (tran:t s e (sym:t sym))))))

    (define (cat->nfa a b)
      (let ((ea (walk a))   ;; using a let here to get a specific
	    (eb (walk b)))  ;;  ordering of nfa states.
	(match ea eb with
	  (nfa:t astart aend atrans) (nfa:t bstart bend btrans)
	  -> (let ((b-start-trans '())
		   (b-other-trans '())
		   (new-trans '())
		   (e-end (set/empty)))
	       ;; The start state of <b> is merged with each
	       ;; of the end states of <a>.  The start state of
	       ;; <b> is discarded.
	       ;; 1) find all transitions out of <b_start>
	       (for-list t btrans
		 (match t with
		   (tran:t fs ts sym)
		   -> (if (= fs bstart)
			  (PUSH b-start-trans t)
			  (PUSH b-other-trans t))))
	       ;; 2) copy these transitions out of each a_end
	       (for-set e aend
		 (for-list t b-start-trans
		   (match t with
		     (tran:t _ ts sym)
		     -> (PUSH new-trans (tran:t e ts sym)))))
	       (if (set/member bend < bstart)
		   (set! e-end (set/union < aend (set/delete bend < bstart)))
		   (set! e-end bend))
	       (nfa:t astart e-end (append atrans b-other-trans new-trans))
	       ))))

    (define (starplus->nfa a star?)
      (match (walk a) with
	(nfa:t s es trans)
	-> (let ((e-start-trans '())
		 (new-trans '()))
	     ;; 1) collect all e-start transitions
	     (for-list t trans
	       (match t with
		 (tran:t fs _ _)
		 -> (if (= fs s)
			(PUSH e-start-trans t))))
	     ;; 2) add arcs from all <e-end> to all <e-start>
	     (for-set fs es
		(for-list t e-start-trans
		  (match t with
		    (tran:t _ ts sym)
		    -> (PUSH new-trans (tran:t fs ts sym))
		    )))
	     (if (and star? (not (set/member es < s)))
		 (set/add! es < s))
	     (nfa:t s es (append trans new-trans))
	     )))

    (define (or->nfa a b)
      (let ((ea (walk a))
	    (eb (walk b)))
	(match ea eb with
	  (nfa:t astart aend atrans) (nfa:t bstart bend btrans)
	  -> (let ((b-start-trans '())
		   (b-other-trans '())
		   (new-trans '()))
	       ;; copy all <bstart> trans to <astart>, remove <bstart>
	       (for-list t btrans
		 (match t with
		   (tran:t fs ts sym)
		   -> (if (= fs bstart)
			  (PUSH b-start-trans t)
			  (PUSH b-other-trans t))))
	       (for-list t atrans
		 (match t with
		   (tran:t fs ts sym)
		   -> (if (= fs astart)
			  (for-list t0 b-start-trans
			    (match t0 with
			      (tran:t fs0 ts0 sym0)
			      -> (PUSH new-trans (tran:t fs ts0 sym0)))))
		   ))
	       (nfa:t astart (set/union < aend bend) (append new-trans atrans b-other-trans))
	       ))))

    (define (opt->nfa a)
      (match (walk a) with
	(nfa:t s es trans)
	-> (nfa:t s (set/add es < s) trans)
	))

    (define (group->nfa a)
      (let ((tag0 (tag.inc))
	    (tag1 (tag.inc))
	    (new-start (count.inc)))
	(match (walk a) with
	  (nfa:t s es trans)
	  -> (let ((new-end (count.inc))) ;; N.B. order of state generation.
	       (PUSH trans (tran:t new-start s (sym:epsilon tag0)))
	       (for-set e es
		 (PUSH trans (tran:t e new-end (sym:epsilon tag1))))
	       (nfa:t new-start (set/make < new-end) trans)
	       ))))

    (define (min->nfa a)
      (let-values (((nfa nfa-nstates) (rx->nfa a #f)))
    	(let ((tdfa0 (nfa->tdfa (nfa->map nfa nfa-nstates)))
    	      (tdfa1 (hopcroft tdfa0))
    	      ;;(tdfa1 tdfa0)
    	      (nstates (vector-length tdfa1.machine))
    	      (offset (count.get)) ;; current state number
    	      (trans '()))
    	  ;; take dfa transitions, place them into this nfa.
    	  (for-range i nstates
    	    (for-list tran tdfa1.machine[i]
    	      (PUSH trans (tran:t (+ i offset) (+ tran.ts offset) (sym:t tran.sym))))
    	    (count.inc))
    	  (nfa:t offset	;; start
		 ;; finals
		 (list->set (map (lambda (x) (+ x offset))
				 (tree/keys tdfa1.finals))
			    < (set/empty))
    		 (reverse trans)))))

    (define (not->nfa a)
      (let ((nfa (walk (rx:min a)))
	    (states (nfa->states nfa)))
    	;; invert final/non-final
    	(match nfa with
    	  (nfa:t start ends trans)
    	  -> (let ((non-final (set/difference < states ends)))
    	       (nfa:t start non-final trans)
    	       ))))

    (define (int->nfa a b)
      ;; De Morgan's: (A ∩ B)' == (A' ∪ B')
      (walk (rx:~ (rx:or (rx:~ a) (rx:~ b)))))

    (define (diff->nfa a b)
      (walk (rx:int a (rx:~ b))))

    (define walk
      (rx:sym sym) -> (sym->nfa sym)
      (rx:cat a b) -> (cat->nfa a b)
      (rx:* a)	   -> (starplus->nfa a #t)
      (rx:+ a)	   -> (starplus->nfa a #f)
      (rx:or a b)  -> (or->nfa a b)
      (rx:? a)     -> (opt->nfa a)
      (rx:group a) -> (group->nfa a)
      (rx:min a)   -> (min->nfa a)
      (rx:~ a)     -> (not->nfa a)
      (rx:int a b) -> (int->nfa a b)
      (rx:- a b)   -> (diff->nfa a b)
      )

    (renumber (walk rx) search?)

    ))

;; renumber the nfa states, removing gaps caused by the
;;   glushkov algorithm.
;;
;; note: the relative order of nfa states must be preserved,
;;   because these state numbers are used to compute the priority
;;   used when merging duplicate tagged dfa states.
;;
;; note: renumber also removes duplicate transitions, which can
;;   cause epsilon-closure to fail (death by infinite loop).

(define tran<
  (tran:t fs0 ts0 _) (tran:t fs1 ts1 _)
  -> (cond ((< fs0 fs1) #t)
	   ((< fs1 fs0) #f)
	   (else (< ts0 ts1))))

(define (renumber nfa search?)
  (match nfa with
    (nfa:t start end trans)
    -> (let ((used (set/empty))
	     (smap (cmap/make <))
	     (new-trans (set/empty))
	     (nstates 0))
	 ;; collect all used states
	 (set/add! used < start)
	 (for-set s end
	   (set/add! used < s))
	 (for-list t trans
	   (match t with
	     (tran:t fs ts _)
	     -> (begin
		  (set/add! used < fs)
		  (set/add! used < ts))))
	 ;; renumber them (in order!)
	 (for-set s used
	   (cmap/add smap s))
	 (set! nstates smap.count)

	 ;; a prefix of ".*" always leaves a redundant start state.
	 (when search?
	   (tree/delete! smap.map < start)
	   (tree/insert! smap.map < start (+ 1 start)))

	 (define (T n)
	   (- (cmap->index smap n)
	      ;; note: eliminates gap at 0.
	      (if search? 1 0)))

	 ;; renumber
	 (for-list t trans
	   (match t with
	     (tran:t fs ts sym)
	     -> (set/add! new-trans tran< (tran:t (T fs) (T ts) sym))
	     ))
	 (:tuple (nfa:t (T start) (set-map end < T) (set->list new-trans)) nstates)
	 )))

;; convert nfa to a map of fs->(list {ts sym})
;; note: assumes nfa has been renumbered/sorted.

(define (nfa->map nfa size)
  (let ((last -1)
	(current '())
	(m (make-vector size '())))
    (match nfa with
      (nfa:t start end trans)
      -> (begin
	   (for-list t trans
	     (match t with
	       (tran:t fs ts sym)
	       -> (PUSH m[fs] {ts=ts sym=sym})
	       ))
	   {start=start end=end map=m}
	   )
      )))

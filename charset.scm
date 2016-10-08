;; -*- Mode: Irken -*-

(include "lib/basis.scm")

;; this needs to go into core.scm
(define assert
  #t -> #u
  #f -> (error "assertion failed.")
  )

;; this is tricky: I prefer a datatype that is incapable of
;;  representing an invalid charset.  Can this be done with
;;  a range-based representation?  re2 uses a "balanced binary
;;  tree of ranges" in order to handle unicode.  Can we represent
;;  a canonical charset like this?  I think a sorted list of ints
;;  (i.e., a set<int>) could do the trick.
;; so (A Z a z) would work.
;; but would (A Z a) have any meaning?  could it mean A-Za-255?
;; what would (A) mean?  A-255?

;; how about this: a map where the key represents the starting
;;  point and the value represents either the end of the range
;;  or the number of chars in the range.
;; we can iterate over the charset easily, and do only the work
;;  needed rather than a bunch of bit-twiddling.
;; it's still possible to have an invalid charset (with ranges
;;  that overlap) but I think this is a good compromise.

;; hi is not inclusive. this simplifies merging.
;(datatype charset
;  (:t (list {lo=int hi=int})) ;; [lo hi)
;  )

;; Note: as written, this assumes 8-bit characters.  Supporting wider
;;  charsets should be trivial (simply replace the occurrences of "256" below).

;; XXX make a separate list of chars that need backslash-escaping.
(define printable-chars 
  "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~ ")

(define printable-map
  (let ((chars (string->list printable-chars)))
    (fold 
     (lambda (ch t) (tree/insert t < (char->ascii ch) (char->string ch)))
     (tree:empty)
     chars)))

(define (char-repr ch)
  (match (tree/member printable-map < ch) with
    (maybe:yes ch0) -> ch0
    (maybe:no)      -> (format "<" (zpad 2 (hex ch)) ">")
    ))

(define (charset/size s)
  (let ((size 0))
    (for-list r s
      (set! size (+ size (- r.hi r.lo))))
    size))

(define (charset-repr* s)
  (let ((ranges '()))
    (for-list r s
      (cond ((= r.hi (+ 1 r.lo)) 
	     (PUSH ranges (char-repr r.lo)))
	    ((= r.hi (+ 2 r.lo))
	     (PUSH ranges (char-repr (+ 1 r.lo)))
	     (PUSH ranges (char-repr r.lo)))
	    (else
	     (PUSH ranges (format (char-repr r.lo) "-" (char-repr (- r.hi 1)))))))
    (string-concat ranges)
    ))

(define (charset-repr s)
  (let ((size (charset/size s)))
    (cond ((= size 256) ".")
	  ((> size 128)
	   (format "[^" (charset-repr* (charset/invert s)) "]"))
	  (else
	   (format "[" (charset-repr* s) "]")))))

(define (charset-repr-raw s)
  (let ((r '()))
    (for-list x s
      (PUSH r (format (zpad 2 (hex x.lo)) "-" (zpad 2 (hex x.hi)))))
    (format "{" (join "," (reverse r)) "}")))

;; note: this is not meant for overlap detection, it is
;;  for maintaining sets/maps of charsets.
(define (charset< a b)
  (match a b with
    () ()  -> #f
    () _   -> #t
    _ ()   -> #f
    (ra . tla) (rb . tlb)
    -> (cond ((< ra.lo rb.lo) #t)
	     ((< rb.lo ra.lo) #f)
	     ((< ra.hi rb.hi) #t)
	     ((< rb.hi ra.hi) #f)
	     (else (charset< tla tlb)))))

(define (charset/empty)
  (list:nil))

(define (charset/single n)
  (LIST {lo=n hi=(+ n 1)}))

(define (charset/range lo hi)
  (assert (< lo hi))
  (LIST {lo=lo hi=hi}))

(define charset/dot (charset/range 0 256))

;;    -----XXX-----XXXX-----XXX----
;; a)  ch
;; b)      ch
;; c)          ch

;; XXX this could be smarter
(define (charset/in* s ch)
  (let/cc return 
    (for-list r s
      (cond ((< ch r.lo) (return #f)) ;; a
	    ((<= r.hi ch) #f)	      ;; c [note: not a return]
	    (else (return #t))))      ;; b
    (return #f)))

(define (charset/in set ch)
  (charset/in* set (char->ascii ch)))

;; cases                       merging              overlap
;; a) -----AAAAAA--------- alo <= blo <= ahi	alo <= blo < ahi 
;;    ---------BBBBBB-----			                  
;; b) -----AAAAAA--------- alo <= bhi <= ahi	alo < bhi <= ahi 
;;    ----BBBBB-----------			                  
;; c) -----AAAAAA--------- ahi < blo		ahi < blo	     
;;    ------------BBBB----			                  
;; d) -----AAAAAA--------- bhi < alo		bhi < alo	     
;;    -BBB----------------

;; Note: for merging, only a & b imply overlap. so we need only
;;   test for c & d.
(define (cmp-range0 alo ahi blo bhi)
  (cond ((< ahi blo) (cmp:<)) ;; case c
	((< bhi alo) (cmp:>)) ;; case d
	(else (cmp:=))))

;; this version is used for merging
(define (cmp-range a b)
  (cmp-range0 a.lo a.hi b.lo b.hi))

;; this version is used for overlap detection
;; [note: the hi end of the range is open, so
;;  we need to decrement it to detect overlap]

(define (cmp-range-overlap a b)
  (cmp-range0 a.lo (- a.hi 1) b.lo (- b.hi 1)))

;; XXX consider using generators here (especially for unicode)

;; Note: this is O(#ranges), but I bet it's possible
;;  to do better using their sorted nature. [probably by adding
;;  matches like this: (ra) (rb . tlb) where ahi < blo

(define (charset/overlap? a b)
  (match a b with
    () _ -> #f
    _ () -> #f
    (ra . tla) (rb . tlb)
    -> (match (cmp-range-overlap ra rb) with
	 (cmp:=) -> #t
	 (cmp:<) -> (charset/overlap? tla b)
	 (cmp:>) -> (charset/overlap? a tlb)
	 )
    ))

;; memoization

;; (define memo<
;;   {a=a0 b=b0} {a=a1 b=b1}
;;   -> (cond ((< a0 a1) #t)
;; 	   ((< a1 a0) #f)
;; 	   (else (< b0 b1))))

;; (define *charsets* (cmap/make charset<))
;; (define *charset-pairs* (cmap/make memo<))
;; (define *overlaps* (make-vector 20000 0))

;; (define *calls* 0)
;; (define *hits* 0)

;; XXX ai/bi should be canonicalized, could cut
;;   the size of *charset-pairs* in half.

;; (define (charset/overlap? a b)
;;   (set! *calls* (+ 1 *calls*))
;;   (let ((ai (cmap/add *charsets* a))
;; 	(bi (cmap/add *charsets* b))
;; 	(oi (cmap/add *charset-pairs* {a=ai b=bi})))
;;     (cond ((= *overlaps*[oi] 0)
;; 	   (let ((overlap? (charset/overlap?* a b)))
;; 	     (set! *overlaps*[oi] (if overlap? 2 1))
;; 	     overlap?))
;; 	  (else
;; 	   (set! *hits* (+ 1 *hits*))
;; 	   (= *overlaps*[oi] 2)))))

;;; generator version
;; (define (charset/overlap? a b)
;;   (let ((ga (tree/make-generator (charset->map a)))
;; 	(gb (tree/make-generator (charset->map b))))
;;     (let loop ((a (ga))
;; 	       (b (gb)))
;;       (match a b with
;; 	(maybe:no) _ -> #f
;; 	_ (maybe:no) -> #f
;; 	(maybe:yes ra) (maybe:yes rb)
;; 	-> (match (cmp-range-overlap ra rb) with
;; 	     (cmp:=) -> #t
;; 	     (cmp:<) -> (loop (ga) b)
;; 	     (cmp:>) -> (loop a (gb))
;; 	     )
;; 	))))

;; merge two charsets.
;; merge both charsets into a single map, then iterate through
;;  the result.  Collect overlapping sets into an accumulator,
;;  which is flushed whenever it fails to overlap with the set
;;  in the front.

(define (charset/merge a b)

  ;; merge-sort two range lists
  (define merge
    () lb -> lb
    la () -> la
    (ha . ta) (hb . tb)
    -> (if (< ha.lo hb.lo)
	   (list:cons ha (merge ta (list:cons hb tb)))
	   (list:cons hb (merge (list:cons ha ta) tb))))

  ;; we know these ranges overlap, return a new merged range.
  (define (range/merge ra rb)
    {lo=(min ra.lo rb.lo) hi=(max ra.hi rb.hi)})

  (let ((m (merge a b)))
    (if (null? m)
	(charset/empty)
    	(let loop ((acc (car m))
    		   (l (cdr m))
    		   (r (list:nil)))
	  (match l with
	    () -> (reverse (list:cons acc r))
	    (hd . tl)
	    -> (match (cmp-range acc hd) with
	  	 (cmp:=) -> (loop (range/merge acc hd) tl r)
	  	       _ -> (loop hd tl (list:cons acc r))
		 )
	    )
	  ))))

;;    a-b c-d e-f
;; -> \00-a b-c d-e f-\FF
;; XXX rewrite this so 256 doesn't show up on the left
;;   side of a match? [so we can make it a global constant]
(define (charset/invert s)
  (let loop ((end 0) 
	     (r (list:nil))
	     (s s))
    (match s with
      ()                   -> (reverse (list:cons {lo=end hi=256} r))
      ({lo=lo hi=256})     -> (reverse (list:cons {lo=end hi=lo} r))
      ({lo=0 hi=hi} . tl)  -> (loop hi r tl)
      ({lo=lo hi=hi} . tl) -> (loop hi (list:cons {lo=end hi=lo} r) tl)
      )))

(define (parse-charset s)
  (parse-charset0 (string->list s)))

(define (parse-charset0 s)
  (let ((s s)
	(r (charset/empty)))
    (match s with
      (#\^ . s) -> (charset/invert (parse-charset* s r))
      _         -> (parse-charset* s r)
      )))

(define (parse-charset* s r)
  (match s with
    ()             -> r
    (a #\- b . tl) -> (parse-charset* 
		       tl (charset/merge 
			   r (charset/range (char->ascii a) 
					    (+ 1 (char->ascii b)))))
    (a . tl)       -> (parse-charset* 
		       tl (charset/merge 
			   r (charset/single (char->ascii a))))
    ;; backslashes? hex-escapes, ']', '[', etc..
    ))


;;; thoughts on speeding up the tdfa build:
;;;   rather than repeatedly compare character sets, how can
;;;   we memoize it?  One idea, compute the full distinct alphabet
;;;   (as in hopcroft.scm), and then do a table of S X S.  Replace
;;;   all nfa.sym with an index into this table.
;;; we could test this idea without too much work... if we make a 
;;;   table of all distinct charsets, then we can do simple memoization
;;;   using a map or 2d vector.
;;; how would this work?
;;;   we use a cmap to collect all unique charsets.
;;;   then we use a map of (a,b) pairs to memoize.
;;; I think this could be used with many datatypes, since
;;;   the user only need provide a sort function to feed the
;;;   cmap.
;;; ...
;;; ok that didn't help.
;;;
;;; calls 1070010
;;; hits  1055767
;;; |charsets| 402
;;; |pairs| 14243
;;;
;;; good hit rate, but it took twice as long to run.
;;; looks like we could benefit from memoization, but
;;;   how to do it quickly?


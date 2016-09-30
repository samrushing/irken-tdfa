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
(datatype charset
  (:t (tree int int)) ;; [lo hi)
  )

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

(define charset/size
  (charset:t s)
  -> (let ((size 0))
       (for-map lo hi s
	 (set! size (+ size (- hi lo))))
       size))

(define charset-repr*
  (charset:t m)
  -> (let ((ranges '()))
       (tree/reverse
	(lambda (lo hi)
	  (cond ((= hi (+ 1 lo)) 
		 (PUSH ranges (char-repr lo)))
		((= hi (+ 2 lo))
		 (PUSH ranges (char-repr (+ 1 lo)))
		 (PUSH ranges (char-repr lo)))
		(else
		 (PUSH ranges (format (char-repr lo) "-" (char-repr (- hi 1)))))))
	m)
       (string-concat ranges)
       ))

(define (charset-repr s)
  (let ((size (charset/size s)))
    (cond ((= size 256) ".")
	  ((> size 128)
	   (format "[^" (charset-repr* (charset/invert s)) "]"))
	  (else
	   (format "[" (charset-repr* s) "]")))))

(define charset->list
  (charset:t m)
  -> (let ((r '()))
       (tree/reverse
	(lambda (lo hi)
	  (PUSH r (:tuple lo hi)))
	m)
       r))

(define (charset-repr-raw s)
  (let ((r '()))
    (for-list x (charset->list s)
      (match x with
	(:tuple lo hi)
	-> (PUSH r (format (zpad 2 (hex lo)) "-" (zpad 2 (hex hi))))))
    (format "{" (join "," (reverse r)) "}")))

;; note: this is not meant for overlap detection, it is
;;  for maintaining sets/maps of charsets.
(define charset<
  (charset:t a) (charset:t b)
  -> (let ((g0 (tree/make-generator a))
	   (g1 (tree/make-generator b)))
       (let loop ()
	 (match (g0) (g1) with
	   (maybe:no)    (maybe:no)    -> #f
	   (maybe:no)    (maybe:yes _) -> #t
	   (maybe:yes _) (maybe:no)    -> #f
	   (maybe:yes (:tuple ak av)) (maybe:yes (:tuple bk bv))
	   -> (cond ((< ak bk) #t)
		    ((< bk ak) #f)
		    ((< av bv) #t)
		    ((< bv av) #f)
		    (else (loop)))
	   ))
       ))

(define list->charset*
  ()                    acc -> acc
  ((:tuple lo hi) . tl) acc -> (list->charset* tl (tree/insert acc < lo hi))
  )

(define (list->charset l)
  (charset:t (list->charset* l (tree:empty))))

(define (charset/empty)
  (charset:t (tree:empty)))

(define (charset/single n)
  (charset:t (tree/make < (n (+ n 1)))))

(define (charset/range lo hi)
  (assert (< lo hi))
  (charset:t (tree/make < (lo hi))))

(define charset/dot (charset/range 0 256))

(define charset->map
  (charset:t m) -> m
  )

;;    -----XXX-----XXXX-----XXX----
;; a)  ch
;; b)      ch
;; c)          ch

;; XXX this could be smarter
(define charset/in*
  (charset:t m) ch
  -> (let/cc return 
       (for-map lo hi m
	 (cond ((< ch lo) (return #f)) ;; a
	       ((<= hi ch) #f)	       ;; c [note: not a return]
	       (else (return #t))))    ;; b
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
(define cmp-range0
  alo ahi blo bhi
  -> (cond ((< ahi blo) (cmp:<))	;; case c
	   ((< bhi alo) (cmp:>))	;; case d
	   (else (cmp:=))))

;; this version is used for merging
(define cmp-range
  (:tuple alo ahi) (:tuple blo bhi)
  -> (cmp-range0 alo ahi blo bhi))

;; this version is used for overlap detection
;; [note: the hi end of the range is open, so
;;  we need to decrement it to detect overlap]
(define cmp-range-overlap
  (:tuple alo ahi) (:tuple blo bhi)
  -> (cmp-range0 alo (- ahi 1) blo (- bhi 1)))

;; XXX consider using generators here (especially for unicode)

;; Note: this is O(#ranges), but I bet it's possible
;;  to do better using their sorted nature. [probably by adding
;;  matches like this: (ra) (rb . tlb) where ahi < blo
(define (charset/overlap? a b)
  (let loop ((la (charset->list a))
	     (lb (charset->list b)))
    (match la lb with
      () _ -> #f
      _ () -> #f
      (ra . tla) (rb . tlb)
      -> (match (cmp-range-overlap ra rb) with
	   (cmp:=) -> #t
	   (cmp:<) -> (loop tla lb)
	   (cmp:>) -> (loop la tlb)
	   )
      )))

;; merge two charsets.  iterate through both sets of ranges.  if the
;;   two front ranges overlap, then merge them on the front of <a> and
;;   continue.  otherwise add each isolated range and loop.

(define (charset/merge-old a b)

  (define charset/add
    s (:tuple lo hi) -> (tree/insert s < lo hi)
    )

  ;; we know these ranges overlap, return a new merged range.
  (define range/merge 
    (:tuple alo ahi) (:tuple blo bhi)
    -> (:tuple (min alo blo) (max ahi bhi))
    )

  (let loop ((la (charset->list a))
	     (lb (charset->list b))
	     (r (tree/make <)))
    (match la lb with
      () () -> (charset:t r)
      () (rb . tlb) -> (loop la tlb (charset/add r rb))
      (ra . tla) () -> (loop tla lb (charset/add r ra))
      (ra . tla) (rb . tlb) 

      ;; XXX BORKEN
      ;; merge {00-61,62-100} {61-62} -> {00-62,62-100}
      ;; I think we need a running accumulator that collects the fronts
      ;;  as long as they merge

      -> (match (cmp-range ra rb) with
	   (cmp:=) -> (loop (list:cons (range/merge ra rb) tla) tlb r)
	   (cmp:<) -> (loop tla lb (charset/add r ra))
	   (cmp:>) -> (loop la tlb (charset/add r rb))
	   )
      )))

(define (charset/merge a b)

  (define charset/add
    s (:tuple lo hi) -> (tree/insert s < lo hi)
    )

  (define range/merge 
    ;; we know these ranges overlap, return a new merged range.
    (:tuple alo ahi) (:tuple blo bhi)
    -> (:tuple (min alo blo) (max ahi bhi))
    )

  (let ((m0 (charset->map a))
	(m1 '()))
    (for-map lo hi (charset->map b)
      (tree/insert! m0 < lo hi))
    (set! m1 (charset->list (charset:t m0)))
    (if (null? m1)
    	(charset:t (tree:empty))
    	(let loop ((acc (car m1))
    		   (l (cdr m1))
    		   (r (tree/make <)))
	  (match l with
	    () -> (charset:t (charset/add r acc))
	    (hd . tl)
	    -> (match (cmp-range acc hd) with
	  	 (cmp:=) -> (loop (range/merge acc hd) tl r)
	  	       _ -> (loop hd tl (charset/add r acc))
	  	 )
	    )
	  ))
    ))

;;    a-b c-d e-f
;; -> \00-a b-c d-e f-\FF
;; XXX rewrite this so 256 doesn't show up on the left
;;   side of a match? [so we can make it a global constant]
(define (charset/invert s)
  (let loop ((end 0) 
	     (r (tree:empty))
	     (ls (charset->list s)))
    (match ls with
      ()                    -> (charset:t (tree/insert r < end 256))
      ((:tuple lo 256))     -> (charset:t (tree/insert r < end lo))
      ((:tuple 0 hi) . tl)  -> (loop hi r tl)
      ((:tuple lo hi) . tl) -> (loop hi (tree/insert r < end lo) tl)
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



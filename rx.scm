;; -*- Mode: Irken -*-

(include "charset.scm")

;; consider implementing some optimizations/simplifications here,
;;   using identities.  e.g. (a*b*)* = (a|b)*

(datatype rx
  (:sym (list {lo=int hi=int})) ;; symbol
  (:cat rx rx)	 ;; concatentation
  (:or rx rx)	 ;; union
  (:int rx rx)	 ;; intersection
  (:- rx rx)	 ;; difference
  (:? rx)	 ;; optional
  (:+ rx)	 ;; plus
  (:* rx)	 ;; star
  (:~ rx)        ;; not
  (:group rx)	 ;; group
  (:min rx)      ;; minimize
  )

;; this could be smarter (fold concats, use parens only when needed)
(define pp-rx
  (rx:sym s)	-> (charset-repr s)
  (rx:cat a b)	-> (format (pp-rx a) (pp-rx b))
  (rx:or a b)	-> (format (pp-rx a) "|" (pp-rx b))
  (rx:int a b)	-> (format (pp-rx a) "^" (pp-rx b))
  (rx:- a b)	-> (format (pp-rx a) "-" (pp-rx b))
  (rx:+ a)	-> (format "(" (pp-rx a) ")+")
  (rx:? a)	-> (format "(" (pp-rx a) ")?")
  (rx:* a)	-> (format "(" (pp-rx a) ")*")
  (rx:~ a)	-> (format "(" (pp-rx a) ")~")
  (rx:group a)	-> (format "{" (pp-rx a) "}")
  (rx:min a)	-> (format "(" (pp-rx a) ")=")
  )

(define rx-repr
  (rx:sym s)	-> (charset-repr s)
  (rx:cat a b)	-> (format "(cat " (rx-repr a) " " (rx-repr b) ")")
  (rx:or a b)	-> (format "(or " (rx-repr a) "" (rx-repr b) ")")
  (rx:int a b)	-> (format "(intersection " (rx-repr a) " " (rx-repr b) ")")
  (rx:- a b)	-> (format "(difference " (rx-repr a) " " (rx-repr b) ")")
  (rx:+ a)	-> (format "(plus " (rx-repr a) ")")
  (rx:? a)	-> (format "(optional " (rx-repr a) ")")
  (rx:* a)	-> (format "(star " (rx-repr a) ")")
  (rx:~ a)	-> (format "(not " (rx-repr a) ")")
  (rx:group a)	-> (format "(group " (rx-repr a) ")")
  (rx:min a)	-> (format "(min " (rx-repr a) ")")
  )

(define (find-and-parse-charset s pos0)
  (let loop ((chars '())
	     (pos pos0))
    (match (string-ref s pos) with
      #\] -> (:tuple (parse-charset0 (reverse chars)) (+ 1 pos))
      #\\ -> (loop (list:cons (string-ref s (+ pos 1)) chars) (+ pos 2))
      ch  -> (loop (list:cons ch chars) (+ pos 1))
      )))

(define (p-rx rx pos0)

  ;; build a list of items to be concatenated.
  (let loop ((exp '())
	     (pos pos0))

    ;; note: done in reverse
    (define concat
      ()       -> (error "empty concat?")
      (x)      -> x
      (a b)    -> (rx:cat b a)
      (x . tl) -> (rx:cat (concat tl) x)
      )

    (define (bracketed how)
      ;; consume one (bracketed) item, loop
      (let-values (((sub pos0) (p-rx rx (+ 1 pos))))
	(loop (list:cons (how sub) exp) pos0)))

    (define (postfix how)
      ;; apply an operator to previous item, loop
      (loop (list:cons (how (car exp)) (cdr exp)) (+ 1 pos)))

    (define (infix how)
      ;; combine previous items with the rest of the expression.
      (let-values (((sub pos0) (p-rx rx (+ 1 pos))))
	(:tuple (how (concat exp) sub) pos0)))

    (define (charset)
      (let-values (((set pos0) (find-and-parse-charset rx (+ 1 pos))))
	(loop (list:cons (rx:sym set) exp) pos0)))

    (if (>= pos (string-length rx))
	(:tuple (concat exp) pos)
	(match (string-ref rx pos) with
	  #\) -> (:tuple (concat exp) (+ 1 pos))
	  #\} -> (:tuple (concat exp) (+ 1 pos))
	  #\( -> (bracketed id)
	  #\{ -> (bracketed rx:group)
	  #\* -> (postfix rx:*)
	  #\+ -> (postfix rx:+)
	  #\? -> (postfix rx:?)
	  #\~ -> (postfix rx:~)
	  #\= -> (postfix rx:min)
	  #\| -> (infix rx:or)
	  #\^ -> (infix rx:int)
	  #\- -> (infix rx:-)
	  #\[ -> (charset)
	  #\\ -> (loop (list:cons 
			(rx:sym (charset/single (char->ascii (string-ref rx (+ pos 1)))))
			exp) (+ pos 2))
	  #\. -> (loop (list:cons (rx:sym charset/dot) exp) (+ pos 1))
	  ch  -> (loop (list:cons (rx:sym (charset/single (char->ascii ch))) exp) (+ pos 1))
	  ))
    ))
	
(define (parse-rx r)
  (match (p-rx r 0) with
    (:tuple rx pos)
    -> (begin 
	 (assert (= pos (string-length r)))
	 rx)))


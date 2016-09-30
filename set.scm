;; -*- Mode: Irken -*-

;; create a set interface for the red-black tree.
;; XXX put this in lib/

(define (set/empty)
  (tree:empty)
  )

(define set/empty?
  s -> (eq? s (set/empty))
  )

(define (set/size s)
  (tree/size s))

(define (set/addv s < k v)
  (match (tree/member s < k) with
    (maybe:no)    -> (tree/insert s < k v)
    (maybe:yes _) -> s
    ))

(define (set/add s < k)
  (set/addv s < k #f)
  )

(define set/delete tree/delete)

(defmacro set/delete!
  (set/delete! s < k)
  -> (set! s (set/delete s < k)))

(defmacro set/add*
  (set/add* s <) -> s
  (set/add* s < item items ...) -> (set/add (set/add* s < items ...) < item)
  )
  
(defmacro set/add!
  (set/add! s < x ...)
  -> (set! s (set/add* s < x ...))
  )

(defmacro set/make
  (set/make < a ...)
  -> (set/add* (set/empty) < a ...)
  )

;; remove x, then add a ...
(defmacro set/replace
  (set/replace s < x a ...)
  -> (set/add* (set/delete s < x) < a ...)
  )

(defmacro set/replace!
  (set/replace! s < x a ...)
  -> (set! s (set/replace s < x a ...))
  )

;; make this one nary as well?
(defmacro set/addv!
  (set/add! s < k v) 
  -> (set! s (set/addv s < k v))
  )

(define (set/member s < k)
  (match (tree/member s < k) with
    (maybe:yes _) -> #t
    (maybe:no)    -> #f
    ))

(define (set/iterate s p)
  (tree/inorder (lambda (k _) (p k)) s))

(define (set/make-generator t)
  (make-generator
   (lambda (consumer)
     (tree/inorder
      (lambda (k v)
        (consumer (maybe:yes k)))
      t)
     (forever (consumer (maybe:no))))))

;; real-world use of same-fringe!
(define (set/cmp a b <)
  (let ((g0 (set/make-generator a))
        (g1 (set/make-generator b)))
    (let loop ()
      (match (g0) (g1) with
        (maybe:no)    (maybe:no)    -> (cmp:=)
        (maybe:no)    (maybe:yes _) -> (cmp:<)
        (maybe:yes _) (maybe:no)    -> (cmp:>)
        (maybe:yes ia) (maybe:yes ib)
        -> (cond ((< ia ib) (cmp:<))
                 ((< ib ia) (cmp:>))
                 (else (loop)))
        ))
    ))

(define list->set
  ()        < acc -> acc
  (hd . tl) < acc -> (list->set tl < (set/add acc < hd))
  )

(define (set->list s)
  (let ((r '()))
    (tree/reverse (lambda (k _) (PUSH r k)) s)
    r))

(defmacro for-set
  (for-set vname set body ...)
  -> (set/iterate set (lambda (vname) body ...))
  )

(defmacro for-set2
  (for-set2 v0 v1 set0 set1 body ...)
  -> (let (($g0 (set/make-generator set0))
	   ($g1 (set/make-generator set1)))
       (let/cc $return
	   (let loop (($m0 ($g0))
		      ($m1 ($g1)))
	     (match $m0 $m1 with
	       (maybe:no) (maybe:no) 
	       -> ($return #u)
	       (maybe:yes v0) (maybe:yes v1) 
	       -> (begin body ...)
	       _ _ 
	       -> (error "unequal set lengths")
	       )
	     (loop ($g0) ($g1)))))
  )

;; this function 'lifts' a '<' function to a 'set<' function.
(define (make-set-cmp <)
  (lambda (a b)
    (match (set/cmp a b <) with
      (cmp:<) -> #t
      _       -> #f
      )))

(define (set/range->list s < lo hi)
  (let ((r '()))
    (tree/range s < lo hi (lambda (k v) (PUSH r k)))
    ;; could avoid the reverse if tree/range
    ;;   went backward, but that would be counter-intuitive
    (reverse r)))

(define (set/least s)
  (match (tree/least s) with
    (:tuple k _) -> k
    ))

(define (set/pop-least s <)
  (let ((least (set/least s)))
    (:tuple least (set/delete s < least))
    ))

(defmacro set/pop-least!
  (set/pop-least! s <)
  -> (let-values (((least s0) (set/pop-least s <)))
       (set! s s0)
       least))
       
(define (set/intersection < a b)
  (let ((as (set/size a))
	(bs (set/size b))
	(a0 (if (< as bs) a b))
	(b0 (if (eq? a0 b) a b))
	(result (set/empty)))
    (for-set x a0
      (if (set/member b0 < x)
	  (set/add! result < x)))
    result))

(define (set/union < a b)
  (let ((as (set/size a))
	(bs (set/size b))
	(a0 (if (< as bs) a b))
	(b0 (if (eq? a0 b) a b))
	(result b0))
    (for-set x a0
      (set/add! result < x))
    result))

(define (set/difference < a b)
  (let ((result a))
    (for-set x b
      (set/delete! result < x))
    result))

(define (set-map s < p)
  (let ((r (set/empty)))
    (for-set x s
      (set/add! r < (p x)))
    r))

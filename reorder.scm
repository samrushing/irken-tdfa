;; -*- Mode: Irken -*-

;; we need a list of insns to permute a set of registers.
;; since there may be more than one cycle present, we 
;;   need to do a cycle decomposition.
;;
;; https://spellscroll.wordpress.com/2008/12/19/permutation-cycle-decomposition/

;; p is a map from src->dst

;; 0-1 1-2 2-0 => (:cycle 0 1 2)
;; 0-1 1-2 3-4 => (:chain 0 1 2) (:chain 3 4)

(define (cycle-decomposition p)
  (if (eq? p (tree:empty))
      '()
      (let ((chain '()))
        (let-values (((src dst) (tree/pop-least! p <)))
          (PUSH chain src)
          (let loop ((x dst))
            (PUSH chain x)
            (match (tree/member p < x) with
              (maybe:yes dst0)
              -> (begin
                   (tree/delete! p < x)
                   (if (= dst0 src) ;; the cycle is complete
                       (list:cons (:cycle (reverse chain)) (cycle-decomposition p))
                       (loop dst0)))
              (maybe:no)
              -> (list:cons (:chain (reverse chain)) (cycle-decomposition p))
              )
            )))))
          
;; emit moves for a permutation cycle.
;; note: '-1' indicates a temp register.
;;
;; 0 1 2 =>
;;   2 -> t
;;   0 -> 1
;;   1 -> 2
;;   t -> 0

(define cycle->moves
  first acc (a)         -> (list:cons {src=a dst=-1} (reverse (list:cons {src=-1 dst=first} acc)))
  first acc (a b . tl)  -> (cycle->moves first (list:cons {src=a dst=b} acc) (list:cons b tl))
  _ _ _                 -> (impossible)
  )

;; not a cycle, no need for a temp.
(define chain->moves
  (_) acc         -> (reverse acc)
  (a b . tl) acc  -> (chain->moves (list:cons b tl) (list:cons {src=a dst=b} acc))
  _ _             -> (impossible)
  )

(define (reorder p)
  (define recur
    acc ()                -> acc
    acc ((:chain x) . tl) -> (recur (chain->moves x acc) tl)
    acc ((:cycle x) . tl) -> (recur (append (cycle->moves (car x) '() x) acc) tl)
    )
  (recur '() (cycle-decomposition p))
  )

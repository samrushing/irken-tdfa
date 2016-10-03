;; -*- Mode: Irken -*-

(include "tdfa.scm")
(include "machine.scm")

(define (do-one rx block verbose?)
  (let ((tdfa (rx->tdfa rx))
	(m (machine/make tdfa))
	(hits '()))
	
    (define (callback tags)
      (let loop ((tags tags))
	(match tags with
	  () 
	  -> #u
	  ((:tuple tn0 lo) (:tuple tn1 hi) . tl)
	  -> (let ((grp (int->string (/ tn0 2))))
	       (PUSH hits (format (repeat lo " ") (repeat (- hi lo) grp)))
	       (loop tl))
	  ;;_ -> (impossible) ;; tags are always in pairs
	  ((:tuple tn reg)) -> (begin
				 (printf "callback with single tag "
					 "tn " (int tn) 
					 " reg " (int reg) 
					 "\n"))
	  )))

    (when verbose? 
      (dump-flat-dfa tdfa.machine)
      (printf "\"" rx "\"\n"))
    (machine/feed m block 0 callback verbose?)
    (when verbose?
      (printf block "\n")
      (printn m.regs)
      (for-list hit (reverse hits)
	(printf hit "\n")))
    (reverse hits)
    ))

(define *tests* 
  (LIST
    {e="{abc}"   
       b="abcd"
       r='("000")}
			   
    {e=".*{ab|bc}"
       b=  "   ab   bc   abc   "
       r='(
    	   "   00"
    	   "        00"
    	   "             00"
    	   "              00")}

    {e=".*{ab|bc}"
       b=  "   abcabc   "
       r='("   00"
	   "    00"
	   "      00"
	   "       00")}

    {e=".*({sam}|{jim})"
       b=  "   sam   jim   "
       r='("   000"
	   "         111")}

    {e=".*({sam|jim})"
       b=  "   sam   jim   "
       r='("   000"
	   "         000")}

    ;; test rx:?
    {e=".*{abc?def?hij}"
       b=  "   abdehij   abcdehij   abdefhij   "
       r='("   0000000"
	   "             00000000"
	   "                        00000000")}


    {e = ".*{[A-Z][0-9]+}"
       b=  "   A390  "
       r='("   00"
	   "   000"
	   "   0000")}

    ;; test overlapping expressions
    {e = ".*{jim|jimbo}"
       b=  "   jim   jimbo  "
       r='("   000"
	   "         000"
	   "         00000")}

    ;; test overlapping multiple groups
    {e = ".*{({ab}|{bc}|{cd})e}"
       b=  "   bce  "
       r='("   000"
	   "   22")}

    ;; find the string of a's.
    {e = ".*{a*}b"
       b=  "   aaab  "
       r='("   000")}

    ;; minimization
    {e = ".*x{((a|b|c)+(a|b)+)=}y"
       b=  "  xabcay   xabcabcabababy"
       r='("   0000"
	   "            000000000000")}

    ;; finding Subject headers:
    ;; using not
    {e = ".*nS{(.*n[^st].*)~}n[^st]"
       b = "      nS     ns      nt     nF     nS     ns    nn"
       r='("        00000000000000000000"
    	   "                                     00000000000")}

    ;; not using not
    {e = ".*nS{[^n]+(n[st][^n]+)*}n[^st]"
       b = "      nS     ns      nt     nF     nS     ns    nn"
       r='("        00000000000000000000"
	   "                                     00000000000")}

    ;; difference
    {e = ".*a{(b+)-(bbb)}c"
       b = "  abc  abbc  abbbc  abbbbc"
       r='("   0"
	   "        00"
	   "                     0000")}

    ;; same as above, but with "ab" replacing "b".
    {e = ".* {((ab)+)-(ababab)}y"
       b = "   aby   ababy   abababy  ababababy"
       r='("   00"
	   "         0000"
	   "                          00000000")}

    ;; intersection
    {e = ".*{([ab]+)^([bc]+)}"
       b = "        abcabcabc"
       r='("         0"
	   "            0"
	   "               0")}

    ;; SSN or CC
    { e = ".*({BBB[-]BB[-]BBBB}|{BBBB[-]BBBB[-]BBBB[-]BBBB})"
	b = " BBB-BB-BBBB BBBB-BBBB-BBBB-BBBB  "
	r='(" 00000000000"
	    "             1111111111111111111")}

    ;; CapnameYEAR
    { e = ".*{[A-Z][a-z]*[0-9][0-9][0-9][0-9]}"
	b = "  Johnson2016 1776 1453 Goldwater1964  Morbo2032 Zim2036  "
	r='("  00000000000"
	    "                        0000000000000"
	    "                                       000000000"
	    "                                                 0000000")}

   ))

(define (results-match? hits expected)
  (let/cc return
    (for-list2 a b hits expected
      (if (not (string=? a b))
	  (return #f)))
    #t))

(define (do-tests)
  (printf "testing...\n")
  (for-list test *tests*
    (printf "  " (rpad 40 test.e) " - ")
    (let ((results (do-one test.e test.b #f)))
      (if (results-match? results test.r)
	  (printf " GOOD.\n")
	  (begin
	    (printf "\n'" test.b "'\n")
	    (for-list x results
	      (printf "'" x "'\n"))
	    (printf " BAD.\n")
	    )))))

(do-tests)

;(t0 ".*{a+}" "  aaaaaaaa  aaa  aa  ")
;(t0 ".*({a+}|{b+})" "   ab   bc   abc   ")
;(t0 ".*{({ab}|{bc}|{cd})e}" "  bce  ")
;;"   000",
;;"   22",
;(t0 ".*{({ab}|{bc})e}" "  bce   abe  ab  bc ")
;(t0 ".*{[A-Z][a-z]*[0-9]+}" "  Johnson2016 1776 1453 Goldwater1964  Morbo2032 Zim2036  ")
;(do-one "{abq}" "abq" #t)
;(do-one ".*x({a+}|{b+})+c" "      xabbbac   " #t)
;(do-one ".*x({a+}|{b+})c" "      xabbbac   " #t)

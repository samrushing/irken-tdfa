
# Tagged DFA compiler

![.*{[A-Z][0-9]+}](http://dark.nightmare.com/rushing/misc/tdfa.svg ".*{[A-Z][0-9]+}")


This library compiles a regular expression into a 'tagged' DFA.
Please see Ville Laurikari's paper, ["NFAs with Tagged Transitions,
their Conversion to Deterministic Automata and Application to Regular
Expressions"](http://laurikari.net/ville/spire2000-tnfa.pdf) for more
info.

## Quick overview of regexes, NFAs, and DFAs

True regular expressions can be converted to NFAs, which can be
converted to DFAs.  Most modern regex engines use NFA-based engines
which add in many features that actually make it impossible to use a
DFA.  See Russ Cox's [series of
articles](https://swtch.com/~rsc/regexp/regexp1.html) that describe
the genesis of his RE2 library.

But there is one handy feature of modern regex engines that is
somewhat compatible with compiling to DFA: group matching.  (Note:
this does not include backreferences!)  If you need to know exactly
where in the stream a match began and ended, it can be done with this
augmented form of DFA.

## Advantages of a DFA engine

The main advantages to using an DFA engine over an NFA engine:

* Speed.  A DFA visits each character in the input stream once.  Once
  compiled, there are no 'surprises' in the performance of a DFA.  And
  a DFA can be compiled to very fast code; to machine code if
  necessary.

* Scanner size: the state of a DFA is represented by a single integer.

* Streaming Search.  This is a huge, unappreciated advantage.  The DFA
  engine can be fed streaming blocks of any size - all the way down to
  a single character.

## Advantages of streaming search

There are several advantages to streaming search.

* Byte at a Time: Every single NFA-based regex engine I have seen
  requires that the search string be stored in a single contiguous
  block of memory.  If you need to search through a compressed archive
  of email messages, you must decompress each message into a single
  string in memory.  With a DFA you can 'pipe' the uncompressed data
  through a block at a time.

* Parallel Search: because a DFA uses only a single integer to
  represent its state, it is possible to search thousands of streams
  simultaneously.

## How the 'tagged' DFA is different

The DFA is augmented with a set of 'registers' that are used to track
entry to and exit from marked groups.  When the DFA enters an
accepting state, a pair of those registers indicates the stream
position that the sub-expression spans.  This means the scanning
object is somewhat larger (a handful of registers rather than a single
integer), and somewhat slower (the code that moves values between
registers), but if you need group matching, this is the fastest way to
do it.

## Disadvantages of a DFA

The NFA corresponding to a regex is roughly the same size as the
regex.  But compiling that NFA to a DFA can lead to an exponential
increase in size.  The kind of care that one takes to avoid bad
performance with an NFA engine must be used instead to avoid a huge
increase in DFA *size*.

This problem is slightly aggravated by the need to avoid DFA
minimization when working with tagged DFAs (because it destroys the
tag information).

## Other DFA features that come 'for free'

Some unusual regex operators are made available when using a DFA:

* intersection: match only if both sub-expressions match
* complement: match everything that does *not* match a sub-expression.
* difference: match everything in 'A' except for 'B'.

## Syntax

The current syntax is deliberately incompatible with common regex libraries:

* as a reminder that these do not describe the same language. [e.g., no ``^`` or ``$``]
* to keep the parser simple
* to accomodate new features.

The most striking difference is the use of ``{`` and ``}`` to indicate group
capture.  

Other operators:

* ``-`` (infix) indicates set difference.
* ``^`` (infix) indicates intersection.
* ``=`` (postfix) triggers DFA minimization. [you usually don't want to use this].
* ``~`` (postfix) complement.

The 'normal' regex operators use the expected ``+``, ``?``, and ``*``.

Any operators can be escaped with a backslash.

[I hope to design an s-expression syntax to make it easier to write
complex regexes and to help with translation to/from other forms of
regex syntax].

Character sets are still very simple: ``[A-Za-z]``, ``[0-9]``,
etc... there are currently no character classes or other features.

## Unicode

This code internally uses a set of ranges to represent character sets.
Although it is currently hard-coded to match 8-bit ASCII, there's no
reason it couldn't be expanded to support the full unicode range.
This could then be compiled to match against UTF-8 using the same
techniques that RE2 uses.

## Searching vs Matching

Most uses of this library will involve generating 'searching' DFAs,
which usually start with ``.*``.  The resulting machine will report
each possible match by group number.  Groups are numbered starting
from the left at zero, and may overlap/embed.

## Output

Currently, there is some test code in Irken to verify that the
resulting machines work correctly.  A start on a simple code generator
for python is also available (see ``emit.scm``).  There are many
(many) possible options for output, ranging from tables (at various
levels of optimization/size) to code generation.  I will probably
write an LLVM code generator soon.

## As a Library

Though this library is written in Irken, the resulting machines can
be used with any language.  One idea I'm tossing around is to wrap 
this up as a C library, with accompanying table-based matching engines.
I haven't yet done this with Irken, but it's something I've wanted to
try out for years.  Encourage me!

## Examples

Simple scan for a single group:

    rx: ".*{[A-Z][0-9]+}"
    input:    "   A390  "
    matches:  "   00"
              "   000"
              "   0000"

The string of '0' chars in the 'match' string indicates the range
of characters and which group they matched.

Note: this is the TDFA in the image at the top of this README.

Overlapping multiple groups:

    rx: ".*{({ab}|{bc}|{cd})e}"
    input:    "   bce  "
    matches:  "   000"
              "   22"

Here, 'bce' matches the outermost group (numbered zero),
but it also matches the second of the three groups inside
group zero.

Set difference:

    rx: ".*a{(b+)-(bbb)}c"
    input:   "  abc  abbc  abbbc  abbbbc"
    matches: "   0"
             "        00"
             "                     0000"

Here, group zero matches any string of 1+ 'b' chars, *except* for
exactly three of them.  Note: the 'a' and 'c' that bracket this
expression are required to match, but are not part of the group.

## More Complex

This is a 'real-world' example, though abstracted a bit.

This expression mimics searching an email header for 'Received:' lines,
but handles RFC822 continuation lines (i.e., any header can be continued
over multiple lines by starting the next line with a space or tab character.

### In this example:

* ``R`` is short for ``Received:``
* ``n`` = newline
* ``t`` = tab
* ``s`` = space

Complement: 

    rx: ".*nR{(.*n[^st].*)~}n[^st]"
    input:   "      nR     ns      nt     nF     nR     ns    nn"
    matches: "        00000000000000000000"
             "                                     00000000000"

This example uses the complement/negation operator to indicate "eat
lines except those that start a new header" (i.e., newline followed by
a space or tab.)  The first match is a Received: header stretched out
over three lines, the second of which starts with a space, the third
with a tab.  This is followed by a 'From:' header ('F') and another
match which spans two lines.

Note: the trailing "n[^st]" ensures that we receive only one match
report, at the end of the header, rather than matching at every single
character in the received line.

The DFA compiled from this expression has 19 states, and uses six
registers.


## Some nice graphs

Starting with a simple regex: ``.*{[A-Z][0-9]+}``

1) parse regex

```
(cat (star .) (group (cat [A-Z] (plus [0-9]))))
```

2) convert regex to TNFA

![.*{[A-Z][0-9]+}](http://dark.nightmare.com/rushing/misc/tnfa.svg ".*{[A-Z][0-9]+}")

3) convert TNFA to TDFA

![.*{[A-Z][0-9]+}](http://dark.nightmare.com/rushing/misc/tdfa.svg ".*{[A-Z][0-9]+}")


## Irken?

[Irken](https://github.com/samrushing/irken-compiler) is a
statically-typed Scheme/ML dialect that compiles to either C or LLVM.
The data structures needed for this project would be very difficult to
implement in C, and still pretty rough going in C++.  Irken's type
system handles this fairly easily.  For an example, look at the tail
end of ``hopcroft.scm`` and compare it to the pseudocode on [the
wikipedia
page](https://en.wikipedia.org/wiki/DFA_minimization#Hopcroft.27s_algorithm)
for Hopcroft's algorithm.

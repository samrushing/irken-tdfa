
Sample Generated Scanner
------------------------

This scans for SSN and CC numbers.  See `test/test_emit.scm`.

To build:

    $ cc -O3 scanner.c -o scanner
    $ ./scanner test.txt
    match: group=  0 start=297 end=308 <123-45-6789>
    match: group=  1 start=521 end=540 <1111-2222-3333-4444>


To try other regexes, edit `test/test_emit.scm`, run it, and copy the output `t1.c` to `machine.c`.




    

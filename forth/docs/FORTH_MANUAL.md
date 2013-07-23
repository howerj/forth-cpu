
A Portable FORTH implementation: Howe Forth
===========================================

![Howe Forth Logo](https://raw.github.com/howerj/c-forth/master/logo.png "By the power of HOWE FORTH!")

Author:             

* Richard James Howe.

Copyright:          

* Copyright 2013 Richard James Howe.

License:            

* LGPL

Email(s):              

* howe.r.j.89@googlemail.com

* howerj@aston.ac.uk

INTRO
=====

This is a small [FORTH][forthwiki] implementation called **Howe Forth**,
it is written in C with the aim of being portable, even to radically
different systems from the normal desktop to embedded systems with very
few modifications.

This interpreter traces its lineage back to an entry from the [IOCCC][ioccc]. A
person submitting under the name **buzzard** entered two winning entries, one
listed as *buzzard.2* in the year 1992 was a small FORTH interpreter, this one
is a relative of it.

The interpreter should execute fairly quickly, more in due to its small size as
opposed and sparse feature set. I have not performed any benchmarks however.

As I am working and updating the interpreter this documentation is going to lag
behind that and may go out of date, although it should not do so horrendously.

C PROGRAM
=========

GCC Options
-----------

The program is compiled with the following flags:

"-ansi -g -Wall -Wno-write-strings -Wshadow -Wextra -pedantic -O2"

With [GCC][GCC].

There are some extra options that can be added on, both of which are simply
defining macros.

"-DRUN4X"

This definition enables a cycle counter, the forth virtual machine will exit
when the counter is less than zero, it is automatically decrement each time a
primitive is run. It does not denote real time spent as each primitive has a
variable run time. The forth programming running can enable or disable this
counter, as well as update it.

"-DEBUG_PRN"

This definition enables a function the runs after the virtual machine has
exited. It simply writes out all of the memory in hexadecimal encoded ASCII with
a little bit of formatting to a file called "memory.txt".

forth.c
-------

This file implements the forth virtual machine, it is a threaded code
interpreter. It provides mechanisms for error handling and recovery, an
interface for system calls and input and output redirection.

forth.h
-------

Contained in this file are the usual things needed for interfacing with a
library; definitions of structures, #define macros, typedefs, function
prototypes and the like. There is no executable code in here.

To use this code in your program you must first include "stdio\.h" then include
"forth\.h". "main\.c" contains an example of how to initialize the interpreter
memory, I will give a short example of what to do with reference to the code:

~~~

    fobj_t *forth_obj_create(mw reg_l, mw dic_l, mw var_l, mw ret_l, mw str_l)
    {
        /*the vm forth object */
        int i = 0;
        fobj_t *fo = calloc(1, sizeof(fobj_t));


        /*setting i/o streams */
        for (i = 0; i < MAX_INSTRM; i++) {
                fo->in_file[i] = calloc(1, sizeof(fio_t));
                CALLOC_FAIL(fo->in_file[i], NULL);
                fo->in_file[i]->fio = io_stdin;
        }

        fo->out_file = calloc(1, sizeof(fio_t));
        fo->err_file = calloc(1, sizeof(fio_t));

        fo->in_file[0]->fio = io_stdin;
        fo->out_file->fio = io_stdout;
        fo->err_file->fio = io_stderr;

        /*memories of the interpreter */
        fo->reg = calloc(reg_l, sizeof(mw));
        fo->dic = calloc(dic_l, sizeof(mw));
        fo->var = calloc(var_l, sizeof(mw));
        fo->ret = calloc(ret_l, sizeof(mw));
        fo->str = calloc(str_l, sizeof(char));

        /*initialize input file, fclose is handled elsewhere */
        fo->in_file[1]->fio = io_rd_file;
        if ((fo->in_file[1]->iou.f = fopen("forth.fs", "r")) == NULL) {
                fprintf(stderr, "Unable to open initial input file!\n");
                return NULL;
        }

        /*initializing memory */
        fo->reg[ENUM_maxReg] = MAX_REG;
        fo->reg[ENUM_maxDic] = MAX_DIC;
        fo->reg[ENUM_maxVar] = MAX_VAR;
        fo->reg[ENUM_maxRet] = MAX_RET;
        fo->reg[ENUM_maxStr] = MAX_STR;
        fo->reg[ENUM_inputBufLen] = 32;
        fo->reg[ENUM_dictionaryOffset] = 4;
        fo->reg[ENUM_sizeOfMW] = sizeof(mw);
        fo->reg[ENUM_INI] = true;
        fo->reg[ENUM_cycles] = false;   /*Run for X amount of cycles turned off by default. */
        fo->reg[ENUM_ccount] = 0;       /*Run for X amount of cycles turned off by default. */
        fo->reg[ENUM_inStrm] = 1;

        fprintf(stderr, "\tOBJECT INITIALIZED.\n");
        return fo;
    }

~~~

For brevities sake checking whether "calloc()" worked it omitted. This function
"forth\_obj\_create()" takes in a list of lengths for each of the arrays, which
will be checked by the interpreter when called to see if they meet a list of
minimum requirements, and allocates memory for them, that much is obvious. 

Of interest however is what "reg[X]" is being set to and why and what do
"in\_file", "out\_file" and "err\_file" do. The latter are objects which contain
descriptions of where the input, output and the error streams are to be sent to.
You can read or write to "stdin", "stdout" and "stderr" (if possible), to
strings or to file streams.

Each "reg[X]" value is going to require explaining.

~~~

All MAX_X Assignments tell the forth virtual machine what the
maximum offset is for each array passed to it.

    fo->reg[ENUM_maxReg] = MAX_REG;
    fo->reg[ENUM_maxDic] = MAX_DIC;
    fo->reg[ENUM_maxVar] = MAX_VAR;
    fo->reg[ENUM_maxRet] = MAX_RET;
    fo->reg[ENUM_maxStr] = MAX_STR;

"inputBufLen" is the length of the input buffer, it limits how big
a defined word can be as well as some other things.

    fo->reg[ENUM_inputBufLen] = 32;

The first four cells in the dictionary should be zero, this is the offset into
that dictionary. The reason for this is that it contains a fake dictionary
word which pushes zero if accidentally run, points to itself, points to the
currently processed word in string storage and will run the first virtual
machine instruction when called 'push', although this information is not
necessary for the anything, just set it to '4'.

    fo->reg[ENUM_dictionaryOffset] = 4;

Simply the size of the forth virtual machines machine word. It should be at
least two bytes big, preferably signed (if unsigned many warnings will be
present, but apart from that there should be no problems).

    fo->reg[ENUM_sizeOfMW] = sizeof(mw);

This sets a flag to be true, when the "INI" flag is true the first time the
function "forth\_interpreter()" is run it will attempt to set up the forth
environment to an initial state (by calling "forth\_initialize()"). This checks
whether the minimum memory requirements are met, sets up some other registers to
their initial values, it also gets a list of symbols to be used the forth
virtual machine - that is you can change their name to what you want. It finally
creates a forth function that calls a primitive called 'read' which then calls
itself ('read' makes sure the return stack does not blow up). 

    fo->reg[ENUM_INI] = true;

When compiled with the flag "-DRUN4X" each time a primitive is run and 'cycles'
is enabled a counter is decrement when this is less than zero
"forth\_interpreter()" will exit. This makes sure it starts out as false.

    fo->reg[ENUM_cycles] = false; 
    fo->reg[ENUM_ccount] = 0; 


As there is one file opened as input, this file must be put on the input file
stack, it will be removed once the EOF has been reached. This simply indicates
there is one file on the stack.

    fo->reg[ENUM_inStrm] = 1;

~~~

main.c
------

This file simply contains a way to setup the library; it allocates all the
needed memory, sets up the initial input file stream (a file called **forth.fs**)
and finally sets up some sane starting variables for the allocated memory.

Having done this it then runs the interpreter, when it exits it will display the
functions return value and then destroys the object given to it. The interpreter
itself will close the initial input file **forth.fs** when it reaches a EOF
character.

Forth primitives
----------------

* ":": 

This does two things; 

1) Compiles a header for the next space delimited word in the input stream into
the dictionary.

2) Enter compile mode, instead of executing words that are found and pushing
numbers onto the stack, compile are pointer to those words and literals into the
dictionary.

* "immediate":

Make the word just compiled *immediate*, an immediate word will execute
regardless of whether or not we are in compile or command mode. Unlike in normal
FORTHs this word is called just after the definition of the header instead of
after the definition of the entire word, so like this:

~~~

    : define_me immediate ...

~~~

And not:


~~~

    : define_me ... ; immediate

~~~

* "read":

This reads in a single space delimited word or number and what it does depends
on the state. In either state if it is not a defined word or number then it will
throw an error. A word is always looked for before a number is as a word has no
limitations as to what its name is bar its length.

In command mode:

In command mode 'read' executes words and pushes numbers on to the variable stack.

In compile mode:

In compile mode 'read' compiles a pointer to the word in the next available
slot in the dictionary, for numbers it compiles a 'push integer' (described
later) followed by said number. Immediate words however are executed.

The first word that the interpreter runs is one that calls 'read' and then calls
itself recursively so 'read' decrements the return stack pointer as well so the
stack does no blow up.

* "\\":

This is a comment, it is an immediate word, it will ignore all input until the
end of the line has been reached.

* "exit":

Return from a called word, the return address should be on the return stack, it
is popped off.

* "br":

Branch unconditionally to the next space indicated in the dictionary.

* "?br":

Branch conditionally to the next space indicated in the dictionary if the
top of the stack is zero, else continue. The top of the stack is dropped.

* "\+":

Pop two numbers off the variable stack, add
them and push the result.

* "\-":

Pop two numbers off the variable stack, subtract the first off from the second
and push the result.


* "\*":

Pop two numbers off the variable stack, multiply
them and push the result.

* "%":

Pop two numbers off the variable stack, compute the remainder when the first off
divides the second off and push the result.

* "/":

Pop two numbers off the variable stack, compute the first off
dividing the second off and push the result.

* "lshift":

Pop two numbers off the variable stack, compute the first off
shifting the second off logicically towards the left and push the result.

* "rshift":

Pop two numbers off the variable stack, compute the first off
shifting the second off logicically towards the right and push the result.

* "and":

Pop two numbers off the variable stack, compute the bitwise AND of
them and push the result.

* "or":

Pop two numbers off the variable stack, compute the bitwise OR of
them and push the result.

* "invert":

Bitwise inversion of the top of the variable stack.

* "xor":

Pop two numbers off the variable stack, compute the bitwise XOR of
them and push the result.

* "1\+":

Add one to the top of the variable stack.

* "1\-":

Subtract one from the top of the variable stack.

* "=":

Pop two numbers off the variable stack, test for equality of
them and push the result.

* "<":

Pop two numbers off the variable stack, test if the first of is
greater than the second and push the result.

* "\>":

Pop two numbers off the variable stack, test if the first of is
less than the second and push the result.

* "@reg":

Use the top of the variable stack as an index into the register array, push
the data in that address to the variable stack.

* "@dic":

Use the top of the variable stack as an index into the dictionary array, push
the data in that address to the variable stack.

* "pick":

Use the top of the variable stack as an index into the variable stack itself , push
the data in that address to the variable stack.

* "@str":

Use the top of the variable stack as an index into the string storage array, push
the data in that address to the variable stack.

* "\!reg":

Pop two number off the stack, the first off is an index into the register array,
the second off is the data to write there.

* "\!dic":

Pop two number off the stack, the first off is an index into the dictionary array,
the second off is the data to write there.

* "\!var":

Pop two number off the stack, the first off is an index into the variable stack
itself, the second off is the data to write there.

* "\!str":

Pop two number off the stack, the first off is an index into the string storage array,
the second off is the data to write there.

* "key":

Push one character of input to the variable stack.

* "emit":

Pop an item from the variable stack and output the *lower* 8-bits as a
character.

* "dup":

Duplicate the top of the variable stack.

* "drop":

Drop an item from the variable stack.

* "swap":

Swap the first two items on the variable stack.

* "over":

Duplicate the second item on the variable stack.

* "\>r":

Move the top of the variable stack to the return stack.

* "r\>":

Move the top of the return stack to the variable stack.

* "tail":

This allows the next word compiled word to be called recursively, there is
no 'recurse' word in this FORTH.

* "\'":

Push the value of the next compiled word to the variable stack at run time.

* ",":

Write the top of the stack into the next available dictionary field.

* "printnum":

Pop two items off the variable stack, the second off is the base and the second
is the number to print off, print this number off as a string in the selected
base.

* "get\_word":

Use top of stack as an index into string storage and store the next space
delimited word there.

* "strlen":

Use top of stack as an index into string storage, compute that strings length
and push the result to the variable stack.

* "isnumber":

Use top of stack as an index into string storage, test whether or not the string
there is a number and push the result.

* "strnequ":

Pop two numbers of the variable stack, use both as indices into string storage
and test whether they are equal or not, push zero if they are equal, one if they
are not and two if the strings are too long (what is too long is defined in the
source code).

* "find":

Find a word in the dictionary if it exists and push a pointer to that words
execution field.

* "execute":

Given an execution token of a word is on the variable stack, it pops it and
executes it, if it is not a valid execution token the behaviour is undefined.

The code:

    find word execute

Will cause the word 'word' to be executed.


* "kernel":

This executes system calls, for example file opening and reading. It pops off a
number from the variable stack which is used as an index into a number of
function calls. If the function call needs any more arguments it gets them from
the variable stack.

Hidden words
------------

In addition to this there are three 'invisible' forth words which do not have
a name which are:

* "push integer":

This pushes the next dictionary location onto the variable stack.

* "compile":

This compiles a pointer to the next memory location in the dictionary after the
compile to the next memory location available in the dictionary. 

* "run":

You can that being limited to only these primitives would not create a very
forth-like system. However as any forth programmer knows you can extend the
language in itself, which is what the first file does that is read in.

SYSTEM CALLS
------------

ERROR DETECTION AND HANDLING
----------------------------

FORTH PROGRAM
=============

FINAL WORDS
===========

This program is released under the [LGPL][LGPL], feel free to use it in your
project under LGPLs restrictions. Please contact me if you have any problems
at my listed Email: [howe.r.j.89@gmail.com][EMAIL]. 

<!-- REFERENCES -->

[ioccc]: http://www.ioccc.org/winners.html "IOCCC Winner buzzard.2"
[forthwiki]: https://en.wikipedia.org/wiki/FORTH "FORTH (programming language)"
[LGPL]: http://www.gnu.org/licenses/lgpl-3.0.txt "LGPL License"
[EMAIL]: mailto:howe.r.j.89@gmail.com "howe.r.j.89@gmail.com"
[GCC]: http://gcc.gnu.org/ "The GNU C Compiler"
EOF

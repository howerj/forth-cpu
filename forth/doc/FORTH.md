A SMALL TUTORIAL ON HOWE FORTH.
===============================

![Howe Forth Logo](https://raw.github.com/howerj/c-forth/master/doc/logo.png "By the power of HOWE FORTH!")

Author:             

* Richard James Howe.

Copyright:          

* Copyright 2013 Richard James Howe.

License:            

* LGPL

Email(s):              

* howe.r.j.89@googlemail.com

INTRO
=====

There are a few online tutorials dealing with Forth in various formats, but none
dealing with my specific dialect ( *and why would there be?* ). While the code
should itself is not too long and *should* be well documented there is a need
for one consistent tutorial. All the primitives are listed in *MANUAL.md*, this
tutorial just gives examples of how to use those primitives, how the environment
looks to the user and how to use some of the predefined words.

To compile the program you will need a C Compiler (any compiler *should* do so
long as it is for a hosted environment, it is written in pure ANSI C). Type
*make* and then *./forth* on a Unix like environment. You should now see the
greetings message (in color), something like:

~~~

    Howe Forth
    Base System Loaded
    @author   Richard James Howe
    ...


~~~

If this is the case then you are now at the Forth prompt and ready to type in
commands. While this program should compile under Windows with no problems, I
have not tested it and it would require GNU make to be present. There should be
nothing to stop the actual C files from being compiled that is.

Reading List and Keywords
-------------------------

First some keywords, use your favorite Internet search engine to locate articles
about these topics if you are having trouble.

* Reverse Polish Notation

* Stacks, Last-in-first-out, LIFO

* Forth programming language

* Threaded Code Intepreter

* REPL, Read-Evaluate-Print-Loop

* Concatenative and Reflective programming languages

And a list of websites:

* [Forth on Wikipedia](https://en.wikipedia.org/wiki/Forth_%28programming_language%29)

The bane of Academia, Wikipedia, provides an overview of Forth and many links.

* [Lost at C? Forth Maybe the Answer](http://www.forth.org/lost-at-c.html)

(It is not, stick with C, but it is a good reference for understanding Forth)

* [A Beginners Guide to Forth](http://galileo.phys.virginia.edu/classes/551.jvn.fall01/primer.htm)

A Beginners Guide to Forth by J.V Noble is a very good place to start if you
want to learn and understand Forth.

* [Thinking Forth](http://thinking-forth.sourceforge.net/)

Thinking Forth by Leo Brodie is a very good book covering the language and the
philosophy behind it.


RPN and Hello World!
====================

The first thing you will want when dealing with any new programming language is
either perform some basic arithmetic or print out "Hello, World!". So the first
thing we will do is add a few numbers together, type in:

~~~

2 2 + .

~~~

And you should see '4' being displayed. You will probably not be familiar with
the notation used called *Reverse Polish Notation* or *RPN*, but instead with
*infix* notation such as 2+2. It is simply a different way of doing things but
it gets the same job done, the reason it is done like this is that it makes the
implementation of the system much easier and one of the tenets of Forth is
simplicity. 

What this does is simple, it pushes two numbers onto the *variable* stack, '2'
and '2', so when it reads in a number its behavior is to push that number onto
the stack. Then it reads in a function, or a 'word' in Forth parlance and that
word is '+'. This takes two numbers off the stack and adds them, the pushes the
result. The word '.' pops a number off the stack and displays it, and this works
for other functions and numbers.

We can do the simple "Hello, World!" program either in two ways, the first we
just issue a few commands, the second we define a new function that can be
called again to do what we want type in:

~~~

    ." Hello, World!" cr

~~~

And you will see "Hello, World!" printed to the screen. To define this as a
function that can be called again:

~~~

    : hello ." Hello, World!" cr ;

~~~

And we can type "hello" to call that function. 

It is important to note that all words in Forth are space delimited and there is
nothing special about the words that handle strings. Go ahead and type:

~~~

    ."Hello, World!" cr

~~~

And you the output will look like this (but could be slightly different for
you):

~~~

    667   forth.c
    ."Hello,
    Err: Word not found.

~~~"

There is a different of a single space between

~~~

    ." Hello, World!" cr

    and

    ."Hello, World!" cr


~~~

Which makes all the difference, they are *not* the same thing, ." is simply a
word, when ." is called it will do one of two things depending on whether we are
in compile or command mode. In command mode, the first example (simply ." Hello
World!" cr) we type in commands and they are executed. In the second example we
have compile mode, where we create new functions and words. In command mode it
simply prints out the following string which is *Hello World* in this case, 'cr'
adds a newline after this. In compile mode it stores that string to be printed
out by our new function when that function is called, by typing 'hello'. 

And there we have out first chapter, a verbose description of simple addition
and a "Hello, World!" program in Forth. Simple concepts can be quite difficult
to grasp the first time you hear about them, you should consult multiple sources
and play around until you understand how things work.

THE INTERPRETER
===============


BASIC WORDS
===========

OUTPUT
------

Type "." to pop a number off the stack and print it to the screen, as we have
already seen, to see what is on all of the stack type ".s", for example:

~~~

    3 2 1 .s

~~~

Will print out "3 2 1". We will use these two to inspect our program.

STACK MANIPULATION
------------------

As this is a stack based language manipulating the stacks is quite important,
moving data on them, between them and two and from main memory will constitute
a large part of your programs. The main words you need to use are:

~~~

    dup swap drop r> >r @ !


~~~

As well as a:

~~~

    @reg @str !reg !str

~~~

Which are specific to this implementation but perform similar operations to 
"!" and "@".

Let us begin with "dup", "drop" and "swap". If we put two numbers on the stack,
say 1 and 2 our stack now looks like this.

    1 *2*

With the Top Of the Stack (ToS) surrounded by \*. Now type "dup". And the stack
looks like this.

    1 2 *2*

"dup" is short for duplicate and that is what it does, it duplicates the top
most stack element. If we type "drop" the stack looks like this:

    1 2

Back to what it was before, "drop" drops an element from the stack without
displaying it. Now type "swap":

    2 1

"swap" swaps the top two most items on the stack.


ARITHMETIC
----------

WORD DEFINITION
---------------

IMMEDIATE WORDS
---------------

An immediate word is one that is always executed (unless you mess with the input
stream), it will be executed whether we are defining a word instead of compiled
and in command mode it is executed like any other command is.

Let us look at defining some immediate words, using them and comparing immediate
words to normal compiling words.

To make a word immediate in this dialect of Forth we define a word like this:

~~~

    : hello immediate ." Hello, World!" cr ;


~~~

This defines a word called 'hello' that is immediate, in a more standards
compliant system this would be done like this:

~~~

    : hello ." Hello, World!" cr ; immediate


~~~

The effect is still the same, 'hello' is now immediate. We will compare it with
another the none immediate definition:

~~~

    : hello_normal ." Hello, World" cr ;

~~~

Which is the same, except it is not immediate.




INTROSPECTION
-------------

As we know "." and ".s" can show us what is currently on the stack, there are
other words that have been defined that are also useful for learning about the
Forth environment:

* "words":

Displays a list of all the words in the current dictionary.

* "header" (not standard):

Use:

    header WORD

Find "WORD" in the dictionary and print out X amount of cells to the output.
This basically shows you a segment of the dictionary that a word is held in as
decimal numbers. An example

    header +

Prints out:

    33:     63      1       10      32
    37:     65      1       11      36
    41:     67      1       12      40
    45:     69      1       13      44
    49:     71      1       14      48
    53:     73      1       15      52
    57:     80      1       16      56
    61:     87      1       17      60
    65:     91      1       18      64
    69:     94      1       19      68
    73:     101     1       20      72

The left most column being the address of the second left most column in the
dictionary. Do not worry if it does not print out the same as shown above, this
is just an example.

* "show" (not standard):

Use:

    LOW HIGH show

Print out from "LOW" to "HIGH" the contents of the dictionary, as an example we
can print out the same as shown above with "header +" with the command

    33 77 show

* "shstr" (not standard):

Use:

  LOW HIGH shstr

Similar to "show" except it works on string storage instead, printing out
characters. Example:

    32 64 shstr

Prints out:

    :               i       m
    m       e       d       i
    a       t       e
    r       e       a       d
            \               e
    x       i       t
    b       r               ?
    b       r               +
            -               *

Which are the first few words in the dictionary.

* "here":

Pushes the next free space in the dictionary onto the variable stack.

Example:

    here .


Happens to print out "1236" on my current session. Now if I define a new word
that number will increase. For example:

~~~

: square dup * ;
here . 

~~~

Shows that the number has gone up to "1243", if you are curious as to what the
newly defined word 'square' does it takes a number off the stack, duplicates it
and multiplies it by itself, pushing the result, squaring the original number.
So typing:

~~~

4 square . 

~~~

Will produce '16'.



CREATE...DOES>
--------------

EXAMPLES
========

FINAL WORDS
===========

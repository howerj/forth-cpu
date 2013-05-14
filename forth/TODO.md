
TO DO:
======

![Howe Forth Logo](https://raw.github.com/howerj/c-forth/master/logo.png "By the power of HOWE FORTH!")


These are some notes for myself:

* Documentation. This should be exhaustive, every enum should be documented
as the this program might be used as a library where only the header is
available. Also expand the file "MANUAL.md", which should contain the main
manual.

* Clean up code.

Rename "\!var".

strnequ should have an absolute address which it can not pass instead of a
maximum length as should a few other words.

* String manipulation words.

* Stress testing, stress testing with Valgrind (currently gives no errors).

* Make the system more standards compliant where possible.

I should not go out of my way to do so, just renaming a few things and checking
how a few words behave.

* Pushes and Pops should be put in there own functions, it would save
duplicating so much checking code.

* Clean up code with splint, static code analysis tool.

* Eval()

* On INI Forth should check for valid pointers, ie. Not NULL. It current does
not do this for certain pointers (input/output io)

* Remove options for changing the settings for io_stderr, it should always
point to stderr. This may pose a problem when porting to embedded devices.

* The dictionary could be implemented with a 16-bit hash instead to save
space.

* Test on more platforms, with different compilers.

* Profiling.

* Possible ways of decreasing compile time and why: 

The search algorithm used in the dictionary search is incredibly simple,
changing it so searches are quicker would speed compilation up potentially
greatly. At the moment it is a simple link list of strings. Here are
some ways of changing the search function:

Use hashes (length depending on machine word choice, this would have to
selected at compile time), a simple hash of the string could
be stored instead of the string itself to reduce the string comparison to one of
an equality test.

Move more commonly used words higher up in the dictionary space so they are
found quicker, this may reduce average search times.

Binary search.

I would like to make a compromise between complexity and performance.

Using the following and a test rotating XOR hash available from 
http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx


~~~

  : hash \ Hashes a string, rotating XOR hash
      0 hvar !dic
      begin
          dup @str dup 0= \ if null
          if
              2drop 1       
          else
              
              hvar @dic 4 lshift hvar @dic 28 rshift xor
              xor hvar !dic 1+ 0
          then
      until
      hvar @dic
  ;

  : words
      tabvar pwd @reg 
      begin
          dup 1+ @dic dup hash prnn 32 emit prn cr
          @dic dup 0=   
      until
      cr
      2drop
  ;

  foutput words.log
  words

~~~

Then:

~~~

    awk '{ print $1 }' words.log | sort | uniq -d

~~~

We can see collisions, which should exclude the command 'words' itself due to
both its redefinition and a quirk. This hash fails the test, 'immediate' and
'_immediate' and 'immediate' both hash to the same value.

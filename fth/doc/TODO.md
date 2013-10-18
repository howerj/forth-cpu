## TO DO:

![Howe Forth Logo](https://raw.github.com/howerj/c-forth/master/doc/logo.png "By the power of HOWE FORTH!")


These are some notes for myself:

### Hosted Forth

* Separate out all the code for a hosted desktop into another file
  to simplify things

* For the hosted forth remove sys calls and put them in the hosted
  section, a lot of the sys calls I have are only available for a
  hosted platform.

* Remove all fprintfs?

## General

* do...loop should be able to be nested.

* print "ok" after each loop?

* Define "recurse"

* Replace all memory operations with functions to check if it with in bounds
first, same with all stack operations.

* Turn #define TRUE into enums, replace function returns with bool type
where I can.

* Simplify I/O redirection...

* Documentation. This should be exhaustive, every enum should be documented
as the this program might be used as a library where only the header is
available. Also expand the file "MANUAL.md", which should contain the main
manual.

Rename "\!var".

* On an error (on\_err) all the input streams that have not been closed properly
should be. They currently are not, be careful not to close file pointers twice.

* Test strnequ, rename, remove calls and functions.

strnequ should have an absolute address which it can not pass instead of a
maximum length as should a few other words.

* SIGFPE on ( minimal value of int / -1 ). Needs checking.
  try:
  1 31 lshift -1 /

* Special text encoding for strings and help messages.
  Space, e, t, a, o, i, n, ... (Encoding based on word frequency, Huffman).

* String manipulation words.

I should not go out of my way to do so, just renaming a few things and checking
how a few words behave.

* Pushes and Pops should be put in there own functions, it would save
duplicating so much checking code.

* Eval()

* The dictionary could be implemented with a 16-bit hash instead to save
space.

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
[here](http://eternallyconfuzzled.com/tuts/algorithms/jsw_tut_hashing.aspx)


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
'\_immediate' and 'immediate' both hash to the same value.

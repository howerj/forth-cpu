## readme.md for the H2 Compiler

The idea of this subproject is to create a C compiler for a *strict* and
*limited* subset of C, which you could compile and run either on the H2 CPU or
on the PC for testing, so the C code must be written for that in mind. 

The normal C preprocessor will be used, one will not be recreated unnecessarily.

There will need to be an interface between the code that is to be cross compiled
for when it is to be run on a desktop computer. For example, because our target
has only one type, an unsigned 16 bit integer, we could typedef whatever we use
to declare variables in our C subset.

For example, the keyword "variable", a keyword in the C subset, is effectively a
typedef. So "typedef uint16\_t variable" is all that is needed in the interface
file for *that* issue.

### link.md

This compiler was based on and extended from:

<http://epaperpress.com/lexandyacc/>

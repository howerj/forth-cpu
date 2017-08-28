# Simple Forth File System
## Proposal and Ideas

This is a specification and a proposal for a simple Forth Block based file
system. The file system would support the usual operations expected from a file
system (with the restriction that files have to be multiples of the block
size).

This document is currently more of a list of ideas anything, and it will just
be that until there is some working code for a simple file system.

# Definition of terms

* Forth

A programming language!

* Block

A 1024 Byte block of contiguous memory (this proposal has provision for block sizes 
other than 1024 bytes as well), which by some mechanism can be transfered to and 
from a non-volatile storage. The block is accessed through the Forth Block
word set.

* Line

A 64 byte contiguous piece of memory beginning on a 64 byte boundary that
exists within a block.

* Block Number

A number used to index into block storage, each block has a block number.

## File layout

* Endianess
* Maximum storage capacity

	Example File Structure
	BB
	SR
	BFL BFL
	RDT
	|-----DT
	      |----FDB DB DB DB
	      |----DT
	           |---FDB DB DB
	           |---FDB DB DB DB
	           |---DT
	           |   |---FDB DB DB DB 
	           |   |---FDB
	           |   |---FDB DB DB DB DB
	           |---FDB
	           |---DT
	               |---FDB DB
	FDB - File Description Block
	DT  - Directory Tree
	RDT - Root Directory Tree
	BB  - Boot Block
	SR  - System Record
	BFL - Block Free List
	DB  - Data Block


### Boot block

A 1024 Byte block

This is a block containing 

### System Record

A 1024 Byte block

* Start of block storage (Boot block = block 0)
* Start of block free list
* End of block free list
* Block containing the root directory
* Size of a block (all blocks after the system record are in terms of
this block size)

### Block Free List

The block free list would consist of a bit field representing the status of
each block, a bit set would mean the block is taken, unset that it is free. Bit
zero would be the boot block, one the system record, then the free list itself,
after this would be the data.

### Directory Tree

* 1st line contains meta data
* All lines after that contain directory entries, which are either files
or directories. All directory entries are valid forth word names ( for
example they can contain no white space).

| Cell  15        | Cell 14      | Cell  13 - 0         |
|-----------------|--------------|----------------------|
| DIRECTORY ENTRY | BLOCK NUMBER | COUNTED STRING NAME  |

### File Structure

A file consists of two things; a single block containing meta data for the file
and an array of block numbers that comprise the file, and the data itself. 

#### File Description Block

* 1st line contains meta data
* All cells after the first line contain block numbers that the file is
comprised of, terminated by the number for an invalid block number (-1).

## Rebuilding broken disks and recovering data

## Proposal

* Make a prototype in [libforth][] or in [C][].

## Words needed

* find-free-block    : Find free block 
* toggle-free-block  : Given a bit in a free block, toggle it
* file-next          : Get next block in a file
* directory-next     : Get next directory entry in a file
* ls                 : Display a directory
* rmdir
* set-current-directory
* delete-file
* add-block-to-file

## Forth file-access word layer

* How to implement the file-access word layer in terms of the
file system word primitives

[Forth Blocks]: http://wiki.c2.com/?ForthBlocks
[Forth]: https://en.wikipedia.org/wiki/Forth_(programming_language)
[C]: https://en.wikipedia.org/wiki/C_programming_Language
[libforth]: https://github.com/howerj/libforth

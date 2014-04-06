#!/usr/bin/perl -w
###############################################################################
# H2 assembler
#
#   This assembler is *very* basic and is dependant on a C Pre Processor being
#   present, one should be available on most systems, I am going to be using
#   GCC's CPP.
#
###############################################################################

#### Includes #################################################################

use warnings;
use strict;

###############################################################################

#### Globals ##################################################################

my $maxmem = 8192;        # maximum memory of h2 cpu
my $entryp = 4;           # offset into memory to put program into
my $inputfile   = "cpu.asm";            # input file name
my $tmpfile     = "partial";            # partially processed file suffix
my $symfile     = "sym";                # symbol file
my $outputfile  = "mem_h2.hexadecimal"; # final assembled file
my $verbosity = 0;        # how verbose should we be?
my $outputbase = 16;      # output base of assembled file, 2 or 16
my @mem;                  # our CPUs memory
my $pc = $entryp;         # begin assembling here
my $max_irqs = $entryp;   # maximum number of interrupts
my %labels;               # labels to jump to in program
my %macros;               # all our macro definitions
my %variables;            # compile time variables
my $ifcount = 0;          # number of nested ifs
my @filestack;            # input file stack, for %include directive
my $keeptempfiles = 0;    # !0 == true, keep temporary files
my $dumpsymbols = 0;      # !0 == true, dump all symbols

my $linecount = 0;        # current line count
my $alu = 0;              # for enumerating all alu values.
my %cpuconstants = (      # ALU instruction field, 0-31, main field
## ALU instructions
"T"         => $alu++ << 8,
"N"         => $alu++ << 8,
"R"         => $alu++ << 8,
"[T]"       => $alu++ << 8,
"get_info"  => $alu++ << 8,
"set_interrupts"  => $alu++ << 8,
"T|N"       => $alu++ << 8,
"T&N"       => $alu++ << 8,
"T^N"       => $alu++ << 8,
"~T"        => $alu++ << 8,
"T>N"       => $alu++ << 8,
"N=T"       => $alu++ << 8,
"T+N"       => $alu++ << 8,
"N-T"       => $alu++ << 8,
"set_dptr"  => $alu++ << 8,
"get_dptr"  => $alu++ << 8,
"NrolT"     => $alu++ << 8,
"NrorT"     => $alu++ << 8,

## ALU instruction, other bits
"T->N"   => 1<<7,
"T->R"   => 1<<6,
"N->[T]" => 1<<5,
"R->PC"  => 1<<4,

## ALU stack, variable
"d+0" => 0,
"d+1" => 1,
"d-1" => 3,
"d-2" => 2,

## ALU stack, return
"r+0" => 0 << 2,
"r+1" => 1 << 2,
"r-1" => 3 << 2,
"r-2" => 2 << 2,
);

my @irqnames = ( 
  "Reset/Entry Point",
  "Clock  00        ",
  "Unused 00        ",
  "Unused 01        "
);

###############################################################################

#### Get opts #################################################################

my $intro = 
"H2 CPU Assembler.
\t31/Jan/2014
\tRichard James Howe
\thowe.r.j.89\@gmail.com\n";


my $helpmsg = 
"Usage: ./asm.pl (-(h|i|o|t|x|b|v)+( +filename)*)*
Assembler for the \"H2\" CPU architecture.

  Options:

  --, ignored
  -h, print this message and quit.
  -i, next argument the input file to be assembled.
  -o, next argument is the output file we generate.
  -t, next argument is a temporary file we use.
  -x, output file in base 16 (hexadecimal).
  -b, output file in base 2  (binary).
  -v, increase verbosity level.
  -s, keep all temporary files
  -d, dump table of jump locations

Author:
  Richard James Howe
Email (bug reports to):
  howe.r.j.89\@gmail.com
For complete documentation look at \"asm.md\" which should be
included alongside the assembler.\n";

sub getopts(){
  while (my $arg = shift @ARGV){
    if($arg =~ /^-/m){
      my @chars = split //, $arg;
      foreach my $char (@chars){
        if($char eq '-'){     # ignore
        }elsif($char eq 'h'){ # print help
          print $helpmsg;
          exit;
        }elsif($char eq 'i'){ # read from input file instead of default
          $inputfile = shift @ARGV;
        }elsif($char eq 'o'){ # print to output file instead of default
          $outputfile = shift @ARGV;
        }elsif($char eq 't'){ # temporary file to use
          $tmpfile = shift @ARGV;
        }elsif($char eq 'x'){ # print hex instead
          $outputbase = 16;
        }elsif($char eq 'b'){ # print binary (default)
          $outputbase = 2;
        }elsif($char eq 'v'){ # increase the verbosity, increase it!
          $verbosity++;
        }elsif($char eq 's'){ # keep temporary files
          $keeptempfiles = 1;
        }elsif($char eq 'd'){ # dump symbols
          $dumpsymbols = 1;
        }else{
          die "$char is not a valid option";
        }
      }
    }
  }
}
&getopts();

print $intro;
print "Memory available:\t$maxmem\n"                 if $verbosity > 1;
print "Prog entry point:\t$entryp\n"                 if $verbosity > 0;
print "Input file name :\t$inputfile\n";            #if $verbosity > 0;
print "Temporary file  :\t$inputfile.$tmpfile\n"     if $verbosity > 1;
print "Output file name:\t$outputfile\n";           #if $verbosity > 0;
print "Verbosity level :\t$verbosity\n"              if $verbosity > 2;
print "Output base     :\t$outputbase\n";           #if $verbosity > 0;

###############################################################################

#### Instruction set encoding helper functions ################################

sub printalu{ ## creates and prints an alu instruction
  my $i = 0;
  my $instr = 0;
  while (defined $_[$i]){ # for each argument
    if(exists $cpuconstants{$_[$i]}){ # if this is a valid ALU instruction
      $instr = $instr | $cpuconstants{$_[$i]}; # or it in
    } else {
      die "$_[$i] not a key\n";
    }
    $i++;
  }
  $mem[$pc++] = $instr | 1 << 14 | 1 << 13; # put instruction into memory
}

sub unimplemented{
  print "unimplemented word\n";
}

# instructions to put into memory
# these get called when we find a token that corresponds
# to one of these
sub s_dup       {&printalu("T","T->N","d+1")};
sub s_over      {&printalu("N","T->N","d+1")};
sub s_invert    {&printalu("~T")};
sub s_add       {&printalu("T+N","d-1")};
sub s_sub       {&printalu("N-T","d-1")};
sub s_equal     {&printalu("N=T","d-1")};
sub s_more      {&printalu("N>T","d-1")};
sub s_and       {&printalu("T&N","d-1")};
sub s_or        {&printalu("T|N","d-1")};
sub s_swap      {&printalu("N","T->N")};
sub s_nip       {&printalu("T","d-1")};
sub s_drop      {&printalu("N","d-1")};
sub s_exit      {&printalu("T","R->PC","r-1")};
sub s_tor       {&printalu("N","T->R","d-1","r+1")};
sub s_fromr     {&printalu("R","T->N","T->R","d+1","r-1")};
sub s_rload     {&printalu("R","T->N","T->R","d+1")};
sub s_load      {&printalu("[T]", "d-1", "T->N")};
sub s_store     {&printalu("N","d-2","N->[T]")};
sub s_depth     {&printalu("depth","T->N","d+1")};
sub s_set_int   {&printalu("set_interrupts", "T->N", "d-1")};
sub s_get_dptr  {&printalu("set_dptr", "d+1")};
sub s_set_dptr  {&printalu("get_dptr", "d+1")};

# associate token keywords with the functions that implement
# that instruction, aliases indented
my %keywords = (
  "dup"         => \&s_dup,
  "over"        => \&s_over,
  "invert"      => \&s_invert,
  "+"           => \&s_add,
  "-"           => \&s_sub,
  "="           => \&s_equal,
  ">"           => \&s_more,
  "and"         => \&s_and,
  "or"          => \&s_or,
  "xor"         => \&s_xor,
  "swap"        => \&s_swap,
  "nip"         => \&s_nip,
  "drop"        => \&s_drop,
  "exit"        => \&s_exit,
  "rshift"      => \&s_rshift,
  "lshift"      => \&s_lshift,
  ">r"          => \&s_tor,
  "r>"          => \&s_fromr,
  "r@"          => \&s_rload,
  "@"           => \&s_load,
  "!"           => \&s_store,
  "depth"       => \&s_depth,
  "set_interrupts" => \&s_togglei,
  "set_dptr"        => \&s_set_dptr,
  "get_dptr"        => \&s_get_dptr
);

print "Initializing memory.\n";
for my $i ( 0 .. $maxmem - 1 ){
  $mem[$i] = 0;
}

print "Setting up interrupts.\n";
$mem[0] = $entryp;

#### Parsing helper functions #################################################

# See http://www.perlmonks.org/?node_id=520826
# by use "eyepopslikeamosquito" 
# on Jan 05, 2006 at 09:23 UTC
sub evaluate {
  my ($expr) = @_;
  my @stack;
  for my $token (split ' ', $expr) {
    # no pops
    if ($token =~ /^\d+$/) {
      push @stack, $token;
      next;
    } elsif ($token =~ /^\$.*$/){ # implements variable assignment
     if(exists $variables{$token}){
      push @stack, $variables{$token};
     } else {
      die "token \"$token\" not a valid variable\n";
     }
     next;
    }

    my $x = pop @stack;
    defined $x or die "Stack underflow\n";

    ## one pop
    if($token eq 'drop'){
      next;
    } elsif($token eq '.'){
      print "$x\n";
      next;
    } elsif($token eq 'dup'){
      push @stack, $x, $x;
      next;
    } elsif($token eq 'invert'){
      push @stack,  ~$x;
      next;
    }
    ## two pops
    my $y = pop @stack;
    defined $y or die "Stack underflow\n";

    if ($token eq '+') {
      push @stack, $y + $x;
    } elsif ($token eq '-') {
      push @stack, $y - $x;
    } elsif ($token eq '<<') {
      push @stack, $y << $x;
    } elsif ($token eq '>>') {
      push @stack, $y >> $x;
    } elsif ($token eq 'and') {
      push @stack, $y & $x;
    } elsif ($token eq 'or') {
      push @stack, $y | $x;
    } elsif ($token eq 'xor') {
      push @stack, $y ^ $x;
    } elsif ($token eq '*') {
      push @stack, $y * $x;
    } elsif($token eq 'swap'){
     push @stack, $x, $y;
    } elsif ($token eq '/') {
      push @stack, int($y / $x);
    } else {
      die "Invalid token:\"$token\"\n";
    }
  }

  @stack >= 1 or $stack[0] = 0;
  return $stack[0];
}


# numbers between 0 and 2**15 - 1 take one instruction
# numbers between 2**15 and 2**16 take two instructions
sub inc_by_for_number($){
  my $number = $_[0];
  my $incby = 0;
  if($number < 2**15){
    $incby=1;
  } elsif($number >= 2**15 and $number < 2**16){
    $incby=2;
  } else {
    die "number \"$number\" to large to handle\n";
  }
  return $incby;
}


###############################################################################

#### First Pass ###############################################################
# Get all labels
###############################################################################



###############################################################################

#### Second Pass ##############################################################
# Now we have the labels we can assemble the source
###############################################################################

#### Some options #############################################################
#
###############################################################################


#### Write output file ########################################################
#
###############################################################################

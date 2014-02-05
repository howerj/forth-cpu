#!/usr/bin/perl -w
###############################################################################
# h2 assembler
#
# syntax
#   # (comment)
#   keyword
#   number
#   label:
#   branch label
#
#   %macro label
#     ...
#   %endmacro
#
#   macro
#
# TODO:
#   Conditional compilation
#    %if %elsif %else %endif
#     don't forget to handle file inclusion as well!
#    %ifdef %ifndef
#    %include
#   isr instructions
#   section to load program into, which if
#     > interrupt jmp table end is a fail
#     = interrupt jmp table end is normal
#     < interrupt jmp table end means the interrupt
#       jump table is not filled in, and the assumption
#       is made we have a bootloader already in there.
#   macro parameters
#   nested if statements
#   strings?
#   special variables, %pc
#   variables and ifdef
#   commenting the code
#   add interrupt handling routines
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
my $tmpfile     = "asm.partial";        # partially processed file
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


my $linecount = 0;        # current line count
my $alu = 0;              # for enumerating all alu values.
my %cpuconstants = (      # ALU instruction field, 0-31, main field
## ALU instructions
"T"         => $alu++ << 8,
"N"         => $alu++ << 8,
"R"         => $alu++ << 8,
"[T]"       => $alu++ << 8,
"depth"     => $alu++ << 8,
"T|N"       => $alu++ << 8,
"T&N"       => $alu++ << 8,
"T^N"       => $alu++ << 8,
"~(T^N)"    => $alu++ << 8,
"~T"        => $alu++ << 8,
"T+N"       => $alu++ << 8,
"N-T"       => $alu++ << 8,
"N<<T"      => $alu++ << 8,
"N>>T"      => $alu++ << 8,
"NrolT"     => $alu++ << 8,
"NrorT"     => $alu++ << 8,
"L(T)*L(N)" => $alu++ << 8,
"Nu<T"      => $alu++ << 8,
"N<T"       => $alu++ << 8,
"N=T"       => $alu++ << 8,
"T<0"       => $alu++ << 8,
"T=0"       => $alu++ << 8,
"swapbytes" => $alu++ << 8,
"togglei"   => $alu++ << 8,
"T-1"       => $alu++ << 8,
"clr"       => $alu++ << 8,
"setcarry"  => $alu++ << 8,
"flags"     => $alu++ << 8,
"dptr"      => $alu++ << 8,

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
        }else{
          die "$char is not a valid option";
        }
      }
    }
  }
}
&getopts();

print $intro;
print "Memory available:\t$maxmem\n"      if $verbosity > 1;
print "Prog entry point:\t$entryp\n"      if $verbosity > 0;
print "Input file name :\t$inputfile\n"   ;# if $verbosity > 0;
print "Temporary file  :\t$tmpfile\n"     if $verbosity > 1;
print "Output file name:\t$outputfile\n"  ;#if $verbosity > 0;
print "Verbosity level :\t$verbosity\n"   if $verbosity > 2;
print "Output base     :\t$outputbase\n"  ;#if $verbosity > 0;

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
sub s_dec       {&printalu("T-1")};
sub s_equal     {&printalu("N=T","d-1")};
sub s_and       {&printalu("T&N","d-1")};
sub s_or        {&printalu("T|N","d-1")};
sub s_xor       {&printalu("T^N","d-1")};
sub s_swap      {&printalu("N","T->N")};
sub s_nip       {&printalu("T","d-1")};
sub s_drop      {&printalu("N","d-1")};
sub s_exit      {&printalu("T","R->PC","r-1")};
sub s_rshift    {&printalu("N>>T","d-1")};
sub s_lshift    {&printalu("N<<T","d-1")};
sub s_tor       {&printalu("N","T->R","d-1","r+1")};
sub s_fromr     {&printalu("R","T->N","T->R","d+1","r-1")};
sub s_rload     {&printalu("R","T->N","T->R","d+1")};
sub s_load      {&printalu("[T]")};
sub s_store     {&printalu("N","d-2","N->[T]")};
sub s_multiply  {&printalu("L(T)*L(N)","d-1")};
sub s_depth     {&printalu("depth","T->N","d+1")};
sub s_togglei   {&printalu("togglei")};
sub s_swapbytes {&printalu("swapbytes")};
sub s_dptr      {&printalu("dptr", "d+1")};

# associate token keywords with the functions that implement
# that instruction, aliases indented
my %keywords = (
  "dup"         => \&s_dup,
  "over"        => \&s_over,
  "invert"      => \&s_invert,
  "+"           => \&s_add,
    "add"         => \&s_add,
  "-"           => \&s_sub,
    "sub"         => \&s_sub,
  "1-"          => \&s_dec,
    "decrement"   => \&s_dec,
  "equal"       => \&s_equal,
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
    "tor"         => \&s_tor,
  "r>"          => \&s_fromr,
    "fromr"       => \&s_fromr,
  "r@"          => \&s_rload,
    "rload"       => \&s_rload,
  "@"           => \&s_load,
    "load"        => \&s_load,
  "!"           => \&s_store,
    "store"       => \&s_store,
  "*"           => \&s_multiply,
    "multiply"    => \&s_multiply,
  "depth"       => \&s_depth,
  "toggle_interrupts" => \&s_togglei,
  "swapbytes"   => \&s_swapbytes,
  "dptr"        => \&s_dptr
);

print "Initializing memory.\n";
for my $i ( 0 .. $maxmem - 1 ){
  $mem[$i] = 0;
}

print "Setting up interrupts.\n";
$mem[0] = $entryp;

#### Parsing helper functions #################################################

sub splitline($){
  if($_[0] eq ""){
    return;
  }
  my $line = $_[0];
  $line =~ s/#.*$//g;
  my @tokens = split(' ', $line);
  return @tokens;
}

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
    $incby++;
  } elsif($number >= 2**15 and $number < 2**16){
    $incby+=2;
  } else {
    die "number \"$number\" to large to handle\n";
  }
  return $incby;
}


###############################################################################

#### First Pass ###############################################################
# Get all labels
print "First pass:\n";
open TMPOUT,">", $tmpfile or die "unable to open $tmpfile for writing.\n";
$linecount = 1;
open my $INPUT, "<", $inputfile or die "unable to open $inputfile for reading.\n";
reprocess:
while(<$INPUT>){
  chomp;
  my @tokens = &splitline($_);
  while (my $token = shift @tokens){
    if (exists $keywords{$token}){
      print TMPOUT "$token\n";
      $pc++;
    } elsif($token =~ /^\d+$/){ # print literal, special case
      print TMPOUT "$token\n";
      $pc += &inc_by_for_number($token);
    } elsif($token eq "isr"){
      # print out for second pass
      print TMPOUT "$token ";
      my $token = shift @tokens; 

      if ($token =~ /^\d+$/){
        print TMPOUT "$token\n";
      } elsif(exists $variables{ "\$" . $token}){
        # derefence compile time variable.
        print TMPOUT $variables{"\$".$token}, "\n";
      } else{
        die "invalid token \"$token\" used for isr number";
      }
    } elsif($token eq "allocate"){
      die "allocate is not implement as of yet";
    } elsif($token =~ /^\$.*$/m){
      # compile time variables
      my $expr = join (" ", @tokens);
      $expr =~ s/\"//g;
      $variables{$token} = evaluate $expr;
      last;
    } elsif($token =~ /^%if(n?def)?$/m){
      if($token eq "%ifdef"){
        my $macroname = shift @tokens;
        if(not exists $macros{$macroname}){
          my $line = "";
          while(not $line =~ /%endif|%else/m){
            $line = <$INPUT>;
          }
        }
      } elsif ($token eq "%ifndef"){
        my $macroname = shift @tokens;
        if(exists $macros{$macroname}){
          my $line = "";
          while(not $line =~ /%endif|%else/){
            $line = <$INPUT>;
          }
        }
      }
    } elsif($token eq "%elsif"){
      print "$token not implemented yet\n";
    } elsif($token eq "%else"){
      my $line = "";
      while(not $line =~ /%endif/){
        $line = <$INPUT>;
      }
    } elsif($token eq "%endif"){
      die "$token unexpected, missing %if?\n";
    } elsif($token eq "%include"){
#     $token = shift @tokens;
#     open(my $fh, '<', $token) or die "open failed on $token: $!\n";
#     my $slurp = do { local($/); <$fh> };
#     close($fh);
#
#     $slurp =~ tr{\n}{ };
#     my @tmptokens = reverse &splitline($slurp);
#     foreach my $token (@tmptokens){
#       unshift @tokens, $token;
#     }
      $token = shift @tokens;
      push @filestack, $INPUT;
      open my $tmp, "<", $token or die "open $token failed:$!\n"; 
      $INPUT = $tmp;
    } elsif($token =~ /jumpc?|call/m){
      print TMPOUT "$token ";
      $token = shift @tokens;
      print TMPOUT "$token\n";
      $pc++;
    } elsif($token eq "%macro"){ 
      # macro found, add to dictionary
      # the macros name is the next token.
      # adds anything in between %macro and %endmacro
      my $macroname = shift @tokens;
      die "macros need names!" if not defined $macroname;
      my $macro = "";
      my $line = "";
      while(not $line =~ /%endmacro/){
        $line =~ s/#.*$/ /g;
        $macro = $macro . $line;
        $line = <$INPUT>;
      }
      $macros{$macroname} = $macro;
    } elsif($token =~ /.*:/){ 
      print TMPOUT "$token\n";
      #label found, add to dictionary
      $token =~ tr/://d; # remove labels ';'
      $labels{$token} = $pc;
    } elsif(exists $macros{$token}){
      my $macrostr = $macros{$token};
      $macrostr =~ tr{\n}{ };
      my @tmptokens = reverse &splitline($macrostr);
      foreach my $token (@tmptokens){
        unshift @tokens, $token;
      }
    } elsif(exists $variables{ "\$" . $token}){
      # derefence compile time variable.
      print TMPOUT $variables{"\$".$token}, "\n";
      $pc += &inc_by_for_number($variables{"\$" . $token});
    } else {
      die "\"$token\" is invalid\n";
    }
  }
}
# close $INPUT;

if(0 <= $#filestack){
  $INPUT = pop @filestack; 
  goto reprocess;
}
close TMPOUT;
print "\t", $pc - $entryp, " instructions found; $pc / $maxmem mem used\n";
$pc = $entryp;
print "Complete.\n";
###############################################################################

#### Second Pass ##############################################################
#
print "Second pass:\n";
open INPUT, "<", $tmpfile or die "unable to open tmp.out for reading.\n";

while(<INPUT>){
  chomp;
  my @tokens = &splitline($_);
  while (my $token = shift @tokens){
    if (exists $keywords{$token}){
      print "\t\t$token\n";
      my $func = $keywords{$token};
      &$func();
    } elsif($token =~ /^\d+$/m){ 
      # assemble literal
      print "\t\t$token\n";
      if($token < 2**15){
        $mem[$pc++] = $token | 1 << 15;
      } elsif($token >= 2**15 and $token < 2**16){
        $mem[$pc++] = (~$token & 0xFFFF) | 1<<15;
        &s_invert();
      } else {
        die "number to large to handle\n";
      }
    } elsif($token eq "isr"){
      # sets an interrupt to trigger at this label
      $token = shift @tokens;
      die "isr $token too big" if $token > $max_irqs or $token < 0 ;
      $mem[$token] = $pc;
    } elsif($token eq "allocate"){
    } elsif($token =~ /%if(n?def)?|%elsif|%else|%endif|%macro/m){
      die "$token managed to make its way to the output\n";
    } elsif($token =~ /jumpc?|call/m){
      my $type = $token;
      $token = shift @tokens;
      print "\t$type $token\n";
      if(exists $labels{$token}){
        if($type eq "jump"){
          $mem[$pc++] = $labels{$token};
        } elsif($type eq "jumpc"){
          $mem[$pc++] = $labels{$token} | 1 << 13;
        } elsif($type eq "call"){
          $mem[$pc++] = $labels{$token} | 1 << 14;
        } else{
          die "$token should not have matched regex!\n";
        }
      } else {
        die "label \"$token\" does not exist\n";
      }
    } elsif($token =~ /.*:/){ 
      # label found, do nothing! (except print!)
      print "\t$token\n";
    } elsif(exists $macros{$token}){
      die "$macros{$token} has not been expanded.";
    } else {
      die "\"$token\" is invalid\n";
    }
  }
  die "program to large $pc" if $pc > ($maxmem - 1);
}

## print out interrupt service routine pointers
for(my $i = 0; $i < $max_irqs; $i++){
  print "isr $irqnames[$i] = $mem[$i]\n";
}

close INPUT;
print "Complete.\n";
###############################################################################

#### Write output file ########################################################
open OUTPUT, ">", $outputfile or die "unabled to open output\n";
for (my $i = 0; $i < $maxmem ; $i++){
  if($outputbase eq 2){
    printf OUTPUT "%016b\n", $mem[$i];
  }elsif($outputbase eq 16){
    printf OUTPUT "%04X\n", $mem[$i];
  }else{
    die "invalid output base of $outputbase\n";
  }
}
close OUTPUT;
###############################################################################

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
#    %ifdef %ifndef
#    %include
#   macro parameters
#   strings?
#   compile time variables
#   command line arguments
#   commenting the code
#
###############################################################################

use warnings;
use strict;

my $maxmem = 8192;        # maximum memory of h2 cpu
my $entryp = 4;           # offset into memory to put program into
my $filename = "cpu.asm"; # input file name
my @mem;                  # our CPUs memory

my $pc = $entryp;         # begin assembling here
my %labels;               # labels to jump to in program
my %macros;               # all our macro definitions

my $linecount = 0;
my $i = 0;
my %cpuconstants = (      # ALU instruction field, 0-31, main field
## ALU instructions
"T"         => $i++ << 8,
"N"         => $i++ << 8,
"R"         => $i++ << 8,
"[T]"       => $i++ << 8,
"depth"     => $i++ << 8,
"T|N"       => $i++ << 8,
"T&N"       => $i++ << 8,
"T^N"       => $i++ << 8,
"~(T^N)"    => $i++ << 8,
"~T"        => $i++ << 8,
"T+N"       => $i++ << 8,
"N-T"       => $i++ << 8,
"N<<T"      => $i++ << 8,
"N>>T"      => $i++ << 8,
"NrolT"     => $i++ << 8,
"NrorT"     => $i++ << 8,
"L(T)*L(N)" => $i++ << 8,
"Nu<T"      => $i++ << 8,
"N<T"       => $i++ << 8,
"N=T"       => $i++ << 8,
"T<0"       => $i++ << 8,
"T=0"       => $i++ << 8,
"swapbytes" => $i++ << 8,
"togglei"   => $i++ << 8,
"T-1"       => $i++ << 8,
"clr"       => $i++ << 8,
"setcarry"  => $i++ << 8,
"flags"     => $i++ << 8,
"dptr"      => $i++ << 8,

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
  $mem[$pc++] = $instr | 1 << 13 | 1 << 14; # put instruction into memory
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
sub s_from      {&printalu("R","T->N","T->R","d+1","r-1")};
sub s_rload     {&printalu("R","T->N","T->R","d+1")};
sub s_load      {&printalu("[T]")};
sub s_store     {&printalu("N","d-2","N->[T]")};
sub s_multiply  {&printalu("L(T)*L(N)","d-1")};
sub s_depth     {&printalu("depth","T->N","d+1")};
sub s_togglei   {&printalu("togglei")};
sub s_swapbytes {&printalu("swapbytes")};
sub s_dptr      {&printalu("dptr", "d+1")};

# associate token keywords with the functions that implement
# that instruction
my %keywords = (
  "dup"         => \&s_dup,
  "over"        => \&s_over,
  "invert"      => \&s_invert,
  "+"           => \&s_add,
  "-"           => \&s_sub,
  "1-"          => \&s_d,
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
  "r>"          => \&s_from,
  "r@"          => \&s_rload,
  "@"           => \&s_load,
  "!"           => \&s_store,
  "*"           => \&s_multiply,
  "depth"       => \&s_depth,
  "interrupts"  => \&s_togglei,
  "swapbytes"   => \&s_swapbytes,
  "dptr"        => \&s_dptr
);

print "initializing memory\n";
for my $i ( 0 .. $maxmem - 1 ){
  $mem[$i] = 0;
}

print "setting up interrupts\n";
$mem[0] = $entryp;

#### Parsing helper functions #################################################

sub splitline($){
  if($_[0] eq ""){
    return;
  }
  my @line = split('#',$_[0]);
  my @tokens = split(' ', $line[0]);
  return @tokens;
}


###############################################################################

#### First Pass ###############################################################
# Get all labels
print "first pass\n";
open INPUT, "<", $filename or die "unable to open $filename for reading.\n";
$linecount = 1;
while(<INPUT>){
  chomp;
  my @tokens = &splitline($_);
  while (my $token = shift @tokens){
    #print "$token\n";
    if (exists $keywords{$token}){
      $pc++;
    } elsif($token =~ /\d+/){ # print literal, special case
      if($token < 2**15){
        $pc++;
      } elsif($token >= 2**15 and $token < 2**16){
        $pc+=2;
      } else {
        die "number \"$token\" to large to handle\n";
      }
    } elsif($token =~ /%if(n?def)?/m){
      if($token eq "%ifdef"){
        my $macroname = shift @tokens;
        if(not exists $macros{$macroname}){
          my $line = "";
          while(not $line =~ /%endif|%else/m){
            $line = <INPUT>;
          }
        }
      }
    } elsif($token eq "%elsif"){
      print "$token not implemented yet\n";
    } elsif($token eq "%else"){
      my $line = "";
      while(not $line =~ /%endif/){
        $line = <INPUT>;
      }
    } elsif($token eq "%endif"){
    } elsif($token =~ /jumpc?|call/m){
      $token = shift @tokens;
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
        $macro = $macro . $line;
        $line = <INPUT>;
      }
      $macros{$macroname} = $macro;
    } elsif($token =~ /.*:/){ 
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
    } else {
      print "\"$token\" is invalid\n";
    }
  }
}
print $pc - $entryp, " instructions found; $pc / $maxmem mem used\n";
$pc = $entryp;
close INPUT;
###############################################################################

#### Second Pass ##############################################################
#

print "second pass\n";
open INPUT, "<", $filename or die "unable to open $filename for reading.\n";

while(<INPUT>){
  chomp;
#  my @line = split('#',$_);
#  my @tokens = split(' ', $line[0]);
  my @tokens = &splitline($_);
  while (my $token = shift @tokens){
    if (exists $keywords{$token}){
      print "$token\n";
      my $func = $keywords{$token};
      &$func();
    } elsif($token =~ /\d+/){ 
      # assemble literal
      print "$token\n";
      if($token < 2**15){
        $mem[$pc++] = $token | 1 << 15;
      } elsif($token >= 2**15 and $token < 2**16){
        $mem[$pc++] = (~$token & 0xFFFF) | 1<<15;
        &s_invert();
      } else {
        die "number to large to handle\n";
      }
    } elsif($token =~ /%if(n?def)?/m){
      #print "$token not implemented yet\n";
      if($token eq "%ifdef"){
        my $macroname = shift @tokens;
        if(not exists $macros{$macroname}){
          my $line = "";
          while(not $line =~ /%endif|%else/){
            $line = <INPUT>;
          }
        }
      }
    } elsif($token eq "%elsif"){
      print "$token not implemented yet\n";
    } elsif($token eq "%else"){
      my $line = "";
      while(not $line =~ /%endif/){
        $line = <INPUT>;
      }
    } elsif($token eq "%endif"){
    } elsif($token =~ /jumpc?|call/m){
      my $type = $token;
      $token = shift @tokens;
      print "$type $token\n";
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
      print "$token\n";
    } elsif($token eq "%macro"){
      # adds anything in between %macro and %endmacro
      my $line = shift @tokens;
      while(not $line =~ /%endmacro/){
        $line = <INPUT>;
      }
    } elsif(exists $macros{$token}){
      # puts the macro into the input stream to be revaluated
      my $macrostr = $macros{$token};
      $macrostr =~ tr{\n}{ };
      my @tmptokens = reverse &splitline($macrostr);
      foreach my $token (@tmptokens){
        unshift @tokens, $token;
      }
    } else {
      die "\"$token\" is invalid\n";
    }
  }
  die "program to large $pc" if $pc > ($maxmem - 1);
}
close INPUT;
###############################################################################

open OUTPUT, ">", "mem.binary" or die "unabled to open output\n";
for (my $i = 0; $i < $maxmem ; $i++){
  printf OUTPUT "%016b\n", $mem[$i];
}
close OUTPUT;

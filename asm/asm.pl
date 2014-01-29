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
#   macro nesting
#   strings?
#
###############################################################################

use warnings;
use strict;

my $maxmem = 8192;
my $entryp = 4;   # offset into memory to put program into
my $filename = "cpu.asm";
my @mem;
my $i = 0;

my $pc = $entryp;
my %labels;
my %macros;

my %cpuconstants = (
## ALU instructions
"T"         => $i++,
"N"         => $i++,
"R"         => $i++,
"[T]"       => $i++,
"depth"     => $i++,
"T|N"       => $i++,
"T&N"       => $i++,
"T^N"       => $i++,
"~(T^N)"    => $i++,
"~T"        => $i++,
"T+N"       => $i++,
"N-T"       => $i++,
"N<<T"      => $i++,
"N>>T"      => $i++,
"NrolT"     => $i++,
"NrorT"     => $i++,
"L(T)*L(N)" => $i++,
"Nu<T"      => $i++,
"N<T"       => $i++,
"N=T"       => $i++,
"T<0"       => $i++,
"T=0"       => $i++,
"swapbytes" => $i++,
"togglei"   => $i++,
"T-1"       => $i++,
"clr"       => $i++,
"setcarry"  => $i++,
"flags"     => $i++,

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
  while (defined $_[$i]){
    if(exists $cpuconstants{$_[$i]}){
      $instr = $instr | $cpuconstants{$_[$i]};
    } else {
      die "$_[$i] not a key\n";
    }
    $i++;
  }
  $mem[$pc++] = $instr | 1 << 13 | 1 << 14;
}

sub unimplemented{
  print "unimplemented word\n";
}

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
);

print "initializing memory\n";
for my $i ( 0 .. $maxmem - 1 ){
  $mem[$i] = 0;
}

print "setting up interrupts\n";
$mem[0] = $entryp;

#### First Pass ###############################################################
# Get all labels
print "first pass\n";
open INPUT, "<", $filename or die "unable to open $filename for reading.\n";
while(<INPUT>){
  chomp;
  my @line = split('#',$_);
  my @tokens = split(' ', $line[0]);
  while (my $token = shift @tokens){
    # print "$token\n" if exists $keywords{$token};
    if (exists $keywords{$token}){
      $pc++;
    } elsif($token =~ /\d+/){ # print literal, special case
      $pc++;
    } elsif($token eq "jump"){
      $token = shift @tokens;
      $pc++;
    } elsif($token eq "jumpc"){
      $token = shift @tokens;
      $pc++;
    } elsif($token eq "call"){
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
      # do nothing
    } else {
      print "\"$token\" is invalid\n";
    }
  }
}
$pc = $entryp;
close INPUT;
###############################################################################

#### Second Pass ##############################################################
#

print "second pass\n";
open INPUT, "<", $filename or die "unable to open $filename for reading.\n";

while(<INPUT>){
  chomp;
  my $in = $_;
begin:
  print $in, "\n";
  my @line = split('#',$in);
  my @tokens = split(' ', $line[0]);
  while (my $token = shift @tokens){
    # print "$token\n" if exists $keywords{$token};
    if (exists $keywords{$token}){
      my $func = $keywords{$token};
      &$func();
    } elsif($token =~ /\d+/){ # print literal, special case
      if($token < 2**15){
        $mem[$pc++] = $token | 1 << 15;
      } elsif($token >= 2**15 and $token < 2**16){
        print "not handled yet\n";
      } else {
        die "number to large to handle\n";
      }
    } elsif($token eq "jump"){
      $token = shift @tokens;
      if(exists $labels{$token}){
        $mem[$pc++] = $labels{$token};
      } else {
        die "label \"$token\" does not exist\n";
      }
    } elsif($token eq "jumpc"){
      $token = shift @tokens;
      if(exists $labels{$token}){
        $mem[$pc++] = $labels{$token} | 1 << 13;
      } else {
        die "label \"$token\" does not exist\n";
      }
    } elsif($token eq "call"){
      $token = shift @tokens;
      if(exists $labels{$token}){
        $mem[$pc++] = $labels{$token} | 1 << 14;
      } else {
        die "label \"$token\" does not exist\n";
      }
    } elsif($token =~ /.*:/){ #label found
    } elsif($token eq "%macro"){
      # adds anything in between %macro and %endmacro
      my $line = shift @tokens;
      while(not $line =~ /%endmacro/){
        $line = <INPUT>;
      }
    } elsif(exists $macros{$token}){
      # puts the macro into the input stream to be revaluated
      $in = $macros{$token};
      $in =~ tr{\n}{ };
      goto begin;
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

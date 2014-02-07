#!/usr/bin/perl
# From:
# <https://en.wikipedia.org/wiki/Recursive_descent_parser>
# Accessed on 7th Feb 2014
# pl/0 language compiler for H2 processor
# Not finished
# Richard James Howe

use warnings;
use strict;

#### Global settings ##########################################################

my $inputfile = "tst.pl0";

#### Lexer ####################################################################
my $line = " ";
my @lexme = qw( ; , := - / <= >= { } ! );
my @tokens;

open INPUT, "<", $inputfile or die "open $inputfile; $!\n";

while(<INPUT>){
  $line = $_;
  for my $i (0 .. $#lexme){
    $line = join(" $lexme[$i] ", split("$lexme[$i]",$line));
  }
  $line = join(" * ", split(/\*/, $line));
#  $line = join(" ? ", split(/\?/, $line));
  $line = join(" . ", split(/\./, $line));
  $line = join(" + ", split(/\+/, $line));

#  $line = join(" < ", split("<", $line));
#  $line = join(" > ", split(">", $line));
  #print "$line";
  push @tokens, split (" ", $line);
}

close INPUT;

foreach(@tokens){print $_,"\n"}

###############################################################################


#typedef enum {ident, number, lparen, rparen, times, slash, plus,
#    minus, eql, neq, lss, leq, gtr, geq, callsym, beginsym, semicolon,
#    endsym, ifsym, whilesym, becomes, thensym, dosym, constsym, comma,
#    varsym, procsym, period, oddsym} Symbol;
# 
#Symbol sym;

#### Parser ###################################################################
my $sym;

my %constants;
my %variables;
my %functions;

sub getsym(){
  $sym = shift @tokens;
  print "> $sym\n" if defined $sym;
}

sub add_constants(){ $constants{$_[0]} = 1; &getsym; }
sub expect_constants(){
  if(exists $constants{$_[0]}){ &getsym; return 1;} else {return 0;}
}

sub add_variables(){ $variables{$_[0]} = 1; &getsym; }
sub expect_variables(){
  if(exists $variables{$_[0]}){ &getsym; return 1;} else {return 0;}
}

sub add_functions(){ $functions{$_[0]} = 1; &getsym; }
sub expect_functions(){
  if(exists $functions{$_[0]}){ &getsym; return 1;} else {return 0;}
}

sub expect_number(){
  if(not ($_[0] =~ /\d+/)){
    die "$_[0] NaN\n";
  }
  &getsym;
}

# int accept(Symbol s)
# using global $sym
sub accept($) {
    if ($sym eq $_[0]) {
        &getsym();
        return 1;
    }
    return 0;
}

# int expect(Symbol s)
sub expect($){
    if (&accept($_[0])){
        return 1;
    }
    die("expect: unexpected symbol \"$_[0]\"");
    return 0;
}
 
# using global $sym
sub factor() {
    if (&expect_variables($sym)) { # and const?
    } elsif (&expect_constants($sym)) {
    } elsif (&expect_number($sym)) {
    } elsif (&accept("(")) {
        &expression();
        &expect(")");
    } else {
        die("factor: syntax error \"$sym\"");
        &getsym();
    }
}
 
# using global $sym
sub term() {
    &factor();
    while ($sym eq "*" || $sym eq "/") {
        &getsym();
        &factor();
    }
}
 
# using global $sym
sub expression() {
    if ($sym eq "+" || $sym eq "-"){
        &getsym();
    }
    &term();
    while ($sym eq "+" || $sym eq "-") {
        &getsym();
        &term();
    }
}
 
# using global $sym
sub condition() {
    if (&accept("odd")) {
        &expression();
    } else {
        &expression();
        if ($sym eq "=" || $sym eq "#" || $sym eq "<" || $sym eq "<="
          || $sym eq ">" || $sym eq ">=") {
            &getsym();
            &expression();
        } else {
            die("condition: invalid operator");
            &getsym();
        }
    }
}
 
# using global $sym
sub statement() {
    if (&expect_variables($sym)) { # and const?
        &expect(":=");
        &expression();
    } elsif (&accept("call")) {
      # &expect(ident);
        &expect_functions($sym);
    } elsif (&accept("begin")) {
        do {
            &statement();
        } while (&accept(";"));
        &expect("end");
    } elsif (&accept("if")) {
        &condition();
        &expect("then");
        &statement();
    } elsif (&accept("while")) {
        &condition();
        &expect("do");
        &statement();
    } else {
        die("statement: syntax error on \"$sym\"");
        &getsym();
    }
}
 
# using global $sym
sub block() {
    if (&accept("const")) {
        do {
          # &expect(ident);
            &add_constants($sym);
            &expect("=");
          # &expect(number);
            &expect_number($sym);
        } while (&accept(","));
        &expect(";");
    }
    if (&accept("var")) {
        do {
          # &expect(ident);
          &add_variables($sym);
        } while (&accept(","));
        &expect(";");
    }
    while (&accept("procedure")) {
      # &expect(ident);
        &add_functions($sym);
        &expect(";");
        &block();
        &expect(";");
    }
    &statement();
}
 
# using global $sym
sub program() {
    &getsym();
    &block();
    &expect(".");
    print "parsed\n";
}

&program;
###############################################################################

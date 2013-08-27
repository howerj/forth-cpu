#!/usr/bin/perl
$| = 1; # No output buffering.

$line = " ";

@lexme = qw( ; , := - / <= >= { } ! );

while(<>){
  $line = $_;
  for $i (o .. $#lexme){
    $line = join(" @lexme[$i] ", split("@lexme[$i]",$line));
  }
  $line = join(" * ", split(/\*/, $line));
  $line = join(" ? ", split(/\?/, $line));
  $line = join(" . ", split(/\./, $line));
  $line = join(" + ", split(/\+/, $line));

#  $line = join(" < ", split("<", $line));
#  $line = join(" > ", split(">", $line));
  print "$line";
}

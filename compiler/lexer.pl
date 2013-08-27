#!/usr/bin/perl
$| = 1; # No output buffering.

$line = " ";
while(<>){
  $line = $_;
  $line = join(" ; ", split(';', $line));
  $line = join(" , ", split(",", $line));
  $line = join(" := ", split(":=", $line));

#  $line = join(" \+ ", split("\+", $line));
  $line = join(" - ", split("-", $line));
#  $line = join(" * ", split("*", $line));
  $line = join(" / ", split("/", $line));

#  $line = join(" < ", split("<", $line));
#  $line = join(" > ", split(">", $line));
  $line = join(" >= ", split(">=", $line));
  $line = join(" <= ", split("<=", $line));
  $line = join(" { ", split("{", $line));
  $line = join(" } ", split("}", $line));
  print $line;
}

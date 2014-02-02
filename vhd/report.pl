#!/usr/bin/perl
# <http://oreilly.com/catalog/perlxml/chapter/ch03.html>
#
# Template file.
# TODO:
#   * Auto generate HTML tables of resource usage in
#   a readable format
#   * Output to markdown
#   * Colorize output
#   * Integrate into Doxygen
# run file on:
# tmp/top_level_xst.xrpt
use XML::Parser;
 
# initialize parser and read the file
$parser = new XML::Parser( Style => 'Tree' );
my $tree = $parser->parsefile( shift @ARGV );
 
# serialize the structure
use Data::Dumper;
print Dumper( $tree );

#!/usr/bin/perl
use warnings;
use strict;

use WrapperGenerator;
if (!@ARGV) {
	die "Usage: $0 <class header file>\n";
}
my $class=$ARGV[0];
WrapperGenerator::generate($class);

#!/usr/bin/perl
use warnings;
use strict;
use Getopt::Std;
use Cwd;
my $wd=cwd();
my %opts=();
if (!@ARGV) {$opts{'h'}=1}
getopts( 'hvrNbicCW:', \%opts );

if ( $opts{'h'} ) {
die "$0 options:
    -c: configure
    -b: build 
    -i: install
    -C: clean, then configure
";
}
my $v=$opts{'v'}?'--verbose=2':'';
my $build=($opts{'b'} || $opts{'c'} || $opts{'C'})  ?1:0;
my $install=$opts{'i'}?1:0;
if ($install==1) {$build=0};
my $c=$opts{'c'}?1:0;
my $C=$opts{'C'}?1:0;
$c||=$C;

rename 'Gannet.cabal.OFF', 'Gannet.cabal';
rename 'Gannet-Compiler.cabal', 'Gannet-Compiler.cabal.OFF';
my $W=64;

if (-e "./dist/build/gannetc/gannetc"){
    unlink "./dist/build/gannetc/gannetc";
}

if ($C) {
    system("cabal clean");       
}	
if ($c) {
    print "* Cabal configure, $W-bit\n";
    system("cabal configure");
}
if ($build) {    
#    print "* Cabal build\n";
    print 'cabal build --ghc-options="-O " '.$v."\n";
    system('cabal build --ghc-options="-O " '.$v);
}
if ($install) {
    print "* Cabal install\n";
    print 'cabal install --ghc-options="-O " --bindir=../bin'."\n";
    system('cabal install --ghc-options="-O " --bindir=../bin');
    system("cp ../bin/gannetc ../bin/gannetc$W");
}

rename 'Gannet-Compiler.cabal.OFF', 'Gannet-Compiler.cabal';
rename 'Gannet.cabal', 'Gannet.cabal.OFF';


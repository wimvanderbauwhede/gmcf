#!/usr/bin/perl
use warnings;
use strict;

# build script for GPRM
use Config;
use Cwd;
use Getopt::Std;
use YAML::Syck;
use Data::Dumper;
use WrapperGenerator;

my $gannet_dir = $ENV{GANNET_DIR};
my $sba_dir=cwd();
my $platform = 'macosx';
my $os=`uname`;
if ($os=~/Linux/) {
    $platform='linux';
    }
    
my %opts;
getopts( 'hvwNnsHLSTPW:XdbgcCY:', \%opts );

if ( $opts{'h'} or ((scalar( keys %opts)==0)&&(scalar @ARGV==0))) {
	die "
    Gannet build script for GPRM
    $0 [opts] 
    -Y YAML-file.yml: SBA config file to use
    -w: warnings (for debugging)
    -v: verbose (for debugging)
    -N: NEW, implements 'New' features -- UNUSED
    -n: Don't generate C++ sources from Ruby code -- OBSOLETE
    -s: static allocation --OBSOLETE
    -H: model HW (default is VM) -- OBSOLETE
    -S: use POSIX sockets (no longer supported) 
    -T: DO NOT use POSIX threads
    -P: use communicating processes -- almost certainly BROKEN
    -X: cross-compile for Linux on PPC -- OBSOLETE
    -g: generate SystemConfiguration.h from YAML-file, don't build
    -b: build only, don't generate
    -c: clean
    -C: count cycles
    -d: debug
    -L: compile as library
    \n";
}

my $warn= $opts{'w'}?1:0;
my $verbose= $opts{'v'}?1:0;
my $generate= $opts{'b'}?($opts{'g'}?1:0):1;
my $build= $opts{'g'}?($opts{'b'}?1:0):1;
my $clean=$opts{'c'}?1:0;
my $scons_c=$clean?'-c':'';
my $debug=$opts{'d'}?1:0;

my $scons_ext='';#.local';

#my $scons_new=$opts{'N'} ?'new=1':'';
my $scons_new='new=1';
#my $scons_nogen=$opts{'n'}?'nogen=1':'';
my $scons_nogen='nogen=1';
my $scons_v=$verbose?'v=1':'';
my $scons_w=$warn?'w=1':'';
my $scons_d=$debug?'dbg=1':'';
my $scons_dyn=$opts{'s'}?'':'dyn=1';
my $scons_vm=$opts{'H'}?'':'vm=1';

my $scons_svm='';
my $scons_sock=$opts{'S'}?'sock=1':'sock=0';
my $scons_pthreads=$opts{'T'}?'':'pthreads=1';
#my $scons_pthreads='pthreads=1';
my $scons_distr=$opts{'P'}?'distr=1':'';
my $scons_xc=$opts{'X'}?'xc=1':'';
my $scons_cycles=$opts{'C'}?'cycles=1':'';
my $wordsz = 64;
my $scons_wordsz='wordsz='.$wordsz;
my $scons_lib = $opts{'L'}?'lib=1':'';
my $wd=cwd();
my $scons_wd='wd='.$wd;
my $ymlfile = $opts{'Y'}||"$gannet_dir/SystemConfigurations/SBA.yml";
if ((not $opts{'Y'}) and @ARGV==1) {
    $ymlfile=$ARGV[0];
    $ymlfile=~s/\.*$//;
    $ymlfile.='.yml';
}
my $ymlpath=$ymlfile;
if ($ymlfile!~/^\//) {
	$ymlpath="$wd/$ymlfile";
}
my $scons_sclib='';
my $sclib='';

my $config_href = YAML::Syck::LoadFile($ymlpath);
my %config = %{$config_href};
my @sclibs=@{ $config{'System'}{'Libraries'} };
$sclib=join(',',@sclibs);
$scons_sclib='sclib='.$sclib;

my @flibs = @{ $config{'System'}{'ModelLibPaths'} };
my $flibstr = join(',',@flibs);
my $scons_flibs='flibs='.$flibstr;

my $cxx_gen_source_path="$wd/gensrc";
my $cxx_source_path="$gannet_dir/GPRM/SBA";
#my $cxx_build_path="$gannet_dir/GPRM/build";
my $cxx_build_path="$wd";
my $gprm_lib_path="$wd/lib";

my $run_scons_str="GANNET_YML_CONFIG=$ymlpath scons $scons_c $scons_new $scons_sclib $scons_v $scons_w $scons_d $scons_cycles $scons_dyn $scons_vm $scons_sock
$scons_pthreads $scons_wordsz $scons_nogen $scons_wd $scons_lib $scons_flibs
-f $gannet_dir/GPRM/build/SConstruct$scons_ext";
$run_scons_str=~s/\s+/ /g;

$wd="$gannet_dir/GPRM/build";
if ($clean) {
# Clean GPRM build 
#	print "chdir $cxx_build_path\n";
#	chdir "$cxx_build_path";
	print "$run_scons_str\n";
	system("$run_scons_str");
	print "build.pl: Cleaning gensrc...\n";
	for my $file (qw(SystemConfiguration.h Services.h Services.cc SelectWrapper.cc)) {
		if (-e "./gensrc/$file") {
			print "rm ./gensrc/$file\n";
			unlink "./gensrc/$file";
		}
	}
	for my $file (@sclibs) {
		next if $file eq 'CoreServices';
		if (-e "./gensrc/$file.yml") {
			print "rm ./gensrc/$file.yml\n";
			unlink "./gensrc/$file.yml";
		}
	}
	if (-e './bin/gmcfCoupler') {
		print 'rm ./bin/gmcfCoupler',"\n";
		unlink  './bin/gmcfCoupler';
	}
	if (-e './lib/libgmcf.a') {
		print 'rm ./lib/libgmcf.a',"\n";
		unlink  './lib/libgmcf.a';
	}

} else {
	if ($generate or $clean) {
# 0. Generate .yml library configuration from kernel class(es)
#    and generate the wrapper function(s)		
		my $changed = `scons -s -f $gannet_dir/GPRM/build/SConstruct.CheckChange.py SRC=$ymlpath`;
		chomp $changed;
        my $is_core=0;
        my $nclasses=@sclibs;
		for my $class (@sclibs) {
            if ($class eq 'CoreServices') {$is_core=1};
# FIXME: If there are several classes, this will generate a Services.h for each, so it will overwrite!

			if ($changed eq '1' or (not -e "$cxx_gen_source_path/$class.yml" and $class ne 'CoreServices') ) {	
#                            die "$cxx_gen_source_path/$class" if $class eq 'CoreServices';

				print "build.pl: generating library configuration $class.yml and wrappers\n"; 
				WrapperGenerator::generate($class,$nclasses,$is_core);
			} elsif ( $class eq 'CoreServices' and $nclasses==1 )  {
				print "build.pl: generating library configuration $class.yml and wrappers\n";
				WrapperGenerator::generate($class,$nclasses,$is_core);                
            }
		}
		my $c=($clean)?'-c':'';
		chdir "$gannet_dir/GPRM/build";
		print "build.pl:  generating SystemConfiguration.h from YML files\n";
# 1. Generate SystemConfiguration from YAML file 
#TODO: this should go into the GannetBuilder
		my $create_config_scons_str="scons $c -f SConstruct.SystemConfiguration.py Y='$ymlpath' D='$cxx_gen_source_path' WD='$sba_dir' $scons_distr $scons_wordsz gen";
        $create_config_scons_str=~s/\s+/ /g;
		print "$create_config_scons_str\n";
		system($create_config_scons_str);
	}
	if ($build) {
# 2. Build GPRM code 
        if (!$opts{'L'}) {
			print "build.pl: building GPRM binary\n";
			print "cd $cxx_build_path\n";
			chdir "$cxx_build_path";
		} else {
			print "build.pl: building GPRM library\n";
			print "cd $gprm_lib_path\n";
			chdir "$gprm_lib_path";
		}
		print $run_scons_str,"\n";
		system($run_scons_str);
# 3. Install binary
        if (!$opts{'L'}) {
			print "build.pl: installing binary\n";
			print $run_scons_str," install\n";
			system("$run_scons_str install");
        }
	}
}

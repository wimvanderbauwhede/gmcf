#!/usr/bin/perl
use warnings;
use strict;
use v5.16;
# build script for GPRM
use Config;
use Cwd;
use Getopt::Std;
use YAML::Syck;
use Data::Dumper;
use WrapperGenerator;

my $gmfc_dir = $ENV{GMCF_DIR};
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
    Gannet build script for GMCF
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

my $scons_ext='';

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
say "Working directory: $wd";
my $scons_wd='wd='.$wd;

# Reading the configuration from the YML file
my $ymlfile = $opts{'Y'}||"$gmfc_dir/SystemConfigurations/SBA.yml";
if ((not $opts{'Y'}) and @ARGV==1) {
    $ymlfile=$ARGV[0];
    $ymlfile=~s/\.*$//;
    $ymlfile.='.yml';
}
my $ymlpath=$ymlfile;
if ($ymlfile!~/^\//) {
	$ymlpath="$wd/$ymlfile";
}
my $config_href = YAML::Syck::LoadFile($ymlpath);
my %config = %{$config_href};

my $model_source_dir =  $config{'System'}{'ModelSources'} // 'GMCF';
if (not -d  "$wd/gensrc/$model_source_dir/Models") {
    system("mkdir -p  $wd/gensrc/$model_source_dir/Models");
}
# TEMPORARY HACK: copy gmcfConfiguration from src/GMCF/Models to gensrc/GMCF/Models
# The idea is that this will become a generated file, e.g. based on the YAML config
say "cp src/$model_source_dir/Models/gmcfConfiguration.f95 gensrc/$model_source_dir/Models/gmcfConfiguration.f95"; 
system("cp src/$model_source_dir/Models/gmcfConfiguration.f95 gensrc/$model_source_dir/Models/gmcfConfiguration.f95");

# Passing config info om to Scons
my @sclibs=@{ $config{'System'}{'Libraries'} };
my $sclib=join(',',@sclibs);
my $scons_sclib='sclib='.$sclib;
my $scons_flibs='';

my @model_names =  keys %{ $config{'System'}{'Models'} };
my $models={};
for my $model_name (@model_names) {
 my $model_id = $config{'System'}{'Models'}{$model_name}{'ModelId'};
    say "Model $model_name ID $model_id";
 $models->{$model_id} = $model_name;
}
#my $nmodels = scalar @model_names; # WRONG: only works if each model is used only once
my $n_cfg=$config{'System'}{'NServiceNodes'};
my $n_actual=scalar keys %{ $config{'System'}{'ServiceNodes'} };
my $nmodels =  ($n_actual < $n_cfg)? $n_cfg-1 : $n_actual-1;
my $scons_nmodels ="nmodels=$nmodels"; #FIXME!
my $scons_md='modelsrcdir='.$model_source_dir;

my $cxx_gen_source_path="$wd/gensrc";
#my $cxx_source_path="$gmfc_dir/GPRM/SBA";
my $cxx_build_path="$wd";
my $gprm_lib_path="$wd/lib";

my $run_scons_str="GANNET_YML_CONFIG=$ymlpath scons $scons_c $scons_new $scons_sclib $scons_v $scons_w $scons_d $scons_cycles $scons_dyn $scons_vm $scons_sock $scons_pthreads $scons_wordsz $scons_nogen $scons_wd $scons_md $scons_lib $scons_flibs $scons_nmodels -f $gmfc_dir/GPRM/build/SConstruct$scons_ext";
$run_scons_str=~s/\s+/ /g;

$wd="$gmfc_dir/GPRM/build";
if ($clean) {
# Clean GPRM build 
	say "$run_scons_str";
	system("$run_scons_str");
	say '###';
    say "### build.pl: Cleaning gensrc...";
    say '###';
	for my $file (qw(SystemConfiguration.h Services.h Services.cc SelectWrapper.cc)) {
		if (-e "./gensrc/$file") {
			say "rm ./gensrc/$file";
			unlink "./gensrc/$file";
		}
	}
	for my $file (@sclibs) {
		next if $file eq 'CoreServices';
		if (-e "./gensrc/$file.yml") {
			say "rm ./gensrc/$file.yml";
			unlink "./gensrc/$file.yml";
		}
	}
	if (-e './bin/gmcfCoupler') {
		say 'rm ./bin/gmcfCoupler';
		unlink  './bin/gmcfCoupler';
	}
	if (-e './lib/libgmcf.a') {
		say 'rm ./lib/libgmcf.a';
		unlink  './lib/libgmcf.a';
	}
	if (-e './lib/libgmcfAPI.a') {
		say 'rm ./lib/libgmcfAPI.a';
		unlink  './lib/libgmcfAPI.a';
	}
	if (-e './lib/gmcfapi.mod') {
		say 'rm ./lib/gmcfapi.mod';
		unlink  './lib/gmcfapi.mod';
	}
			if (-e './lib/gmcfconfiguration.mod') {
		say 'rm ./lib/gmcfconfiguration.mod';
		unlink  './gmcfconfiguration.mod';
	}
} else { 
	if ($generate or $clean) {
# 0. Generate .yml library configuration from kernel class(es)
#    and generate the wrapper function(s)		
		my $changed = `scons -s -f $gmfc_dir/GPRM/build/SConstruct.CheckChange.py SRC=$ymlpath`;
		chomp $changed;
        my $is_core=0;
        my $nclasses=@sclibs;
        
        say '###';
		say "### build.pl: generating GMCF wrapper";
        say '###';
        WrapperGenerator::generateGMCF($models,$model_source_dir);
        
		for my $class (@sclibs) {
            if ($class eq 'CoreServices') {$is_core=1};
# FIXME: If there are several classes, this will generate a Services.h for each, so it will overwrite!
			if ( 
			($changed eq '1' or (not -e "$cxx_gen_source_path/$class.yml" and $class ne 'CoreServices') ) 
			or ( $class eq 'CoreServices' and $nclasses==1 ) 
			) {	
                say '###';
				say "### build.pl: generating library configuration $class.yml and wrappers"; 
                say '###';
				WrapperGenerator::generate($class,$nclasses,$is_core, $model_source_dir);
            }
		}
		my $c=($clean)?'-c':'';
        say "chdir $gmfc_dir/GPRM/build";
		chdir "$gmfc_dir/GPRM/build";
        say '###';
		say "### build.pl:  generating SystemConfiguration.h from YML files";
        say '###';
# 1. Generate SystemConfiguration from YAML file 
#TODO: this should go into the GannetBuilder
		my $create_config_scons_str="scons $c -f SConstruct.SystemConfiguration.py Y='$ymlpath' D='$cxx_gen_source_path' WD='$sba_dir' $scons_distr $scons_wordsz gen";
        $create_config_scons_str=~s/\s+/ /g;
		say "$create_config_scons_str";
		system($create_config_scons_str);
	}
	if ($build) { 
# 2. Build GPRM code 
        if (!$opts{'L'}) {
            say '###';
			say "### build.pl: building GPRM binary";
            say '###';
			say "cd $cxx_build_path";
			chdir "$cxx_build_path";
		} else {
            say '###';
			say "### build.pl: building GPRM library";
            say '###';
			say "chdir $gprm_lib_path";
			chdir "$gprm_lib_path";
		}
		say "$run_scons_str libgmcfAPI.a";
		system("$run_scons_str libgmcfAPI.a");
		say "$run_scons_str libgmcf.a";
		system("$run_scons_str libgmcf.a");
# 3. Install binary
        if (!$opts{'L'}) {
			say "### build.pl: installing binary";
			say $run_scons_str," install_gmcfAPI";			
#			system("$run_scons_str install_gmcfAPI");
			system("cp $gmfc_dir/GPRM/src/libgmcfAPI.a $wd/lib");
			say $run_scons_str," install_gmcf";
			system("cp $gmfc_dir/GPRM/src/libgmcf.a $wd/lib");
#			system("$run_scons_str install_gmcf");
        }
	}
}

say '###';
say '### Finished building GMCF framework';
say '###';

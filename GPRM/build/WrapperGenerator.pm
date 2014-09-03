#package GPRM::WrapperGenerator;
package WrapperGenerator;
use warnings;
use strict;

our $V=1;
our $gen_src_path='gensrc';
our $src_path='src/GMCF/Models';
our $templ_path=$ENV{GANNET_DIR}.'/GPRM/build';

our $libname;
our $nclasses;
our $is_core;
our %classes;

sub generate {
    ($libname,$nclasses,$is_core) =@_;
	if ($libname ne 'CoreServices') {
#	print $classheader;
    my @namespaces;
	my $class='';
	my %basic_types=(
			'int'=>1,
			'float'=>1,
			'double'=>1,
			'char'=>1,
			'short'=>1,
			'long'=>1,
			);
	my $classheader = "$gen_src_path/GMCF/Models/$libname.h";
    open my $CH, '<', $classheader or die "$!:$classheader" ;
    my $mode='private';
    while(my $line=<$CH>) {
#		print $line;
        chomp $line;
        $line=~s/^\s+//;
        $line=~s/\s+$//;
    
        $line=~/^namespace\s+(\w+)\s*\{/ && do {
			my $ns=$1;
            push @namespaces, $ns unless $ns=~/(:?Gannet|SBA|Base|GPRM|GMCF|Kernel)/;
            next;
        };

        $line=~/^class\s+(\w+)/ && do {
            $class=$1;
			print "CLASS: $class\n" if $V;
            next;
        };

        $line=~/(public|private):/ && do {
            $mode=$1;
			print "MODE: $mode\n" if $V;
            next;
        };

        $line=~/^([:\w\*\&<>]+)\s+(.*?)(\w+)\s*\([^\)]*\)\s*;/ && do {
			print "METHOD: $line\n" if $V;
            $line=~s/\(\s*\)/(void)/;
            $line=~s/\)\s*;.*$//;
            $line=~s/\(/ (/g;
            my @chunks = split(/\s+\(/,$line);
            my @type_method = split(/\s+/,$chunks[0]);
            my $method=pop @type_method;
			my $ct=0;
			my @templtypes=();
# FIXME: hack for templates. Works, but only if the string __TEMPLATE_TYPE_(\d+)__ does not appear in the original code!
			while ($chunks[1]=~/(<.+?>)/) {
				my $templtype=$1;
#				print "MATCHED $templtype\n";
				push @templtypes,$templtype;
				$chunks[1]=~s/$templtype/__TEMPLATE_TYPE_${ct}__/;
				$ct++;
			}
#			print @templtypes;die;
            my @arg_types = split(/\s*,\s*/,$chunks[1]);
            my $type = join(' ',@type_method);
			my $fin_arg_types=[];
			for my $arg_type (@arg_types) {
#print "ARG TYPE: $arg_type \n";
				my @chunks=split(/\s+/,$arg_type);
				if (scalar @chunks != 1) {
#					print " more than 1 chunk\n";
					if (not ( ($chunks[0] eq 'const') && (scalar @chunks==2))) {
#						print "not const ...\n";
						my $maybe_var=$chunks[-1];
						$maybe_var=~s/[\*\&]+//;
						if (not exists $basic_types{$maybe_var}){
#							print "removing last word\n";
							pop @chunks;
						}
					}
				}                
				$arg_type=join(' ',@chunks);
				if ($arg_type=~/__TEMPLATE_TYPE_(\d+)__/) {
					my $ct=$1;
#					print "FOUND __TEMPLATE_TYPE_${ct}__\n";
#					print $templtypes[$ct],"\n";
					$arg_type=~s/__TEMPLATE_TYPE_${ct}__/$templtypes[$ct]/;
				}
				push @{$fin_arg_types},$arg_type;
			}
            print $mode,': ',$type,' ',$method,' (',join(' , ', @arg_types),")\n" if $V;                    
			if ($mode eq 'public') {
				$classes{$class}{$method}=[$type,$fin_arg_types];				
           }
        };
    }
    close $CH;
	if (not -e "$src_path/$libname.yml") {
		gen_library_config();
	}
	}
	gen_kernel_wrapper();
} # END of generate()

sub gen_library_config {
	my @lines=();
	my $header =
"--- # SBA Core Library Configuration
System:
  Version: 3.0
  Library: $libname

  Services: # last value is control bit (1=control, 0=computational) 
";
	push @lines, $header;
	my $class_id=32; # FIXME: if there are several classes, this is not correct! Every class needs a unique ID
	for my $class (keys %classes) {  
		my $line ="    $class: [ $class_id, kernel_$class, 0 ]\n"; # TODO: only supports computational kernels
		push @lines, $line;
	}
	my $scline = "  ServiceClasses:\n";
	push @lines, $scline;

	for my $class (keys %classes) {  

		my $line="    $class: [ ".join(', ',sort keys %{$classes{$class}} )." ]\n";
		push @lines, $line;
	}
	open my $LC, '>', "$gen_src_path/$libname.yml" unless -e "$src_path/$libname.yml";
	for my $line (@lines) {
		print $LC $line;
	}
	close $LC;
} # END of gen_library_config()

sub gen_kernel_wrapper {
    print "GENERATING $gen_src_path/Services.h\n";
	open my $ST,'<',"$templ_path/Services.h"; 
	open my $SG,'>',"$gen_src_path/Services.h";
	while (my $line=<$ST>) {
		if ($line!~/__KERNEL_WRAPPER__/) {
			print $SG $line;
		} else {
			next if $line=~/define/;
			for my $class (keys %classes) {  
				my $header_line = "            void kernel_$class();\n";
				print $SG $header_line;
			}
		}
	}
	close $SG;
	close $ST;
    if (scalar keys %classes == 0) {
	my @lines=();	
	my $ctor_args=''; # FIXME
#		my $libname=$class; # FIXME: in principle they could be different
	my $sig_preamble_line =
"#include \"Services.h\"
#include \"SystemConfiguration.h\"

using namespace SBA;


";
  print "GENERATING $gen_src_path/Services.cc\n";

	open my $KW, '>', "$gen_src_path/Services.cc";
#		print $line;
		print $KW $sig_preamble_line;
	close $KW;
    
    } else {
    for my $class (keys %classes) {  
    print "CLASS $class $nclasses\n";
        my $comment='';
        if ($nclasses>1) {
           if( $libname eq 'CoreServices') {
            next;
           }
        } else {
            if( $libname eq 'CoreServices') {
            $comment='//';
            }
        }

	my @lines=();	
	my $ctor_args=''; # FIXME
#		my $libname=$class; # FIXME: in principle they could be different
	my $sig_preamble_line =
"#include \"Services.h\"
$comment#include \"$libname.h\" 
#include \"SystemConfiguration.h\"

using namespace SBA;

void Services::kernel_${class}() {
#ifdef VERBOSE
    std::cout << \"CALLING kernel_${class}()\" << std::endl ; 
#endif    
    ${class}* inst;
    if (init_state(SC_${libname}_${class})) {
        inst = new ${class}($ctor_args);
        store_state(SC_${libname}_${class},(void*)inst);
    } else {
        inst=(${class}*)load_state(SC_${libname}_${class});
    }

    void* res;
	Symbol_t res_symbol = NIHIL;
#ifdef VERBOSE
    std::cout << \"CALLING method \"<< method() << std::endl; 
#endif     
    switch ( method() ) {
";
#print $sig_preamble_line;
    push @lines, $sig_preamble_line;

for my $method (sort keys%{$classes{$class}} ) {
	my $ret_type = $classes{$class}->{$method}[0];
	my @arg_types = @{ $classes{$class}->{$method}[1] };
	my @args=();
	my $i=0;
	for my $arg_type (@arg_types) {
        if ($arg_type=~/SBA::(System|Tile)\*/) {
            my $ptr_t=lc($1);
            push @args,"($arg_type)sba_${ptr_t}_ptr";

        } else {
            push @args,"($arg_type)arg($i)";
            $i++;
        }
	}
	my $all_args=join(', ',@args);
	my $line = 
"        case M_${libname}_${class}_${method}:
		{
			$ret_type retval = inst->$method($all_args);
			res = (void*)retval;
			res_symbol=mkPointerSymbol(res);
			break;
		};
";
	push @lines, $line;
}
	my $postamble_line=
"
		default:
			std::cout << \"ERROR: NO SUCH METHOD: \" << method() << \"for class $class\\n\";
    };
    result(res_symbol);
}	
";
	push @lines, $postamble_line;
    print "GENERATING $gen_src_path/Services.cc\n";

	open my $KW, '>', "$gen_src_path/Services.cc";
	for my $line (@lines) {
#		print $line;
		print $KW $line;
	}
	close $KW;
}
}
} # END of gen_kernel_wrapper()

sub generateGMCF {
   (my $nmodels) =@_;
   if (not -e "$gen_src_path/GMCF/Models") {
     mkdir "$gen_src_path/GMCF";
     mkdir "$gen_src_path/GMCF/Models";
   }
    print "GENERATING $gen_src_path/GMCF/Models/GMCF.h\n";	
	open my $GWH,'>',"$gen_src_path/GMCF/Models/GMCF.h";
	print $GWH '// Generated wrapper for GMCF. Only parameter is $nmodels
#include "SBA/Types.h"
#include "SBA/System.h"
#include "SBA/Tile.h"

class GMCF {
    public:
';    
for my $i (1..$nmodels) {
print $GWH "\t\t".'int64_t run_model'.$i.'(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t);'."\n";
}
print $GWH '
};
';	
    close $GWH;
    print "GENERATING $gen_src_path/GMCF/Models/GMCFmodelF.h\n";	
	open my $GWFH,'>',"$gen_src_path/GMCF/Models/GMCFmodelF.h";
	print $GWFH '// Generated wrapper for GMCF. Only parameter is $nmodels
// GMCFmodelF.h
#ifndef _GMCFMODELF_H_
#define _GMCFMODELF_H_
';
for my $i (1..$nmodels) {
 print $GWFH 'extern "C" void main_routine'.$i.'_(int64_t*,int64_t*, const int*);'."\n";
}
print $GWFH "#endif // _GMCFMODELF_H_\n";
close $GWFH;

    print "GENERATING $gen_src_path/GMCF/Models/GMCF.cc\n";	
	open my $GWCC,'>',"$gen_src_path/GMCF/Models/GMCF.cc";
	print $GWCC '// Generated wrapper for GMCF. Only parameter is $nmodels
#include "GMCF.h"
#include "CastPointers.h"
#include "GMCFmodelF.h"

';
for my $i (1 .. $nmodels) {
print $GWCC ' 
int64_t GMCF::run_model'.$i.'(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model'.$i.'" << std::endl;
#endif
';
print $GWCC "	const int model = $i;\n";
print $GWCC '
    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers for MODEL '.$i.': .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
';
print $GWCC '	std::cout << "\n CASTING System pointer in model '.$i.'\n";'."\n";
print $GWCC '
#endif

	void* sys_vp = reinterpret_cast<void*>(sba_sysptr);
	int64_t sys_iv = (int64_t)sys_vp;
	int64_t* sba_sys_ivp = &sys_iv;
#ifdef VERBOSE
	std::cout << "\n CASTING Tile pointer\n";
#endif
	void* tile_vp = reinterpret_cast<void*>(sba_tileptr);
	int64_t tile_iv = (int64_t)tile_vp;
    int64_t* sba_tile_ivp = &tile_iv;
#ifdef VERBOSE
	std::cout << "CALLING Fortran main_routine1_" << std::endl;
#endif

    // Here we call the actual Fortran function
';    
print $GWCC "    main_routine${i}_(sba_sys_ivp,sba_tile_ivp,&model);\n";
print $GWCC '
#ifdef VERBOSE
	std::cout << "LEAVING run_model'.$i.'" << std::endl;
#endif

    return '.$i.'; // purely for GPRM compatibility!
}		
';
}
    close $GWCC;

 
} # END of generate_gmcf_wrapper()

1;

#!/usr/bin/perl
use strict;
use warnings; 
use 5.010;
use F95ArgDeclParser;

use Data::Dumper;
$Data::Dumper::Indent=1;#0;
$Data::Dumper::Terse =1;

use Getopt::Std;
our $VV= 0;
our $vv=0;
our $kernel_path='../OpenCL/Kernels';
our $wrapper_dir = 'Wrappers';
our $kstub_path='KernelStubs';

our $src_gen='';
  
our $new_name='';
our $new_name_camel='';
our $new_name_camel_lc1st='';

main();

=head1 How to use this script?

The script takes the path to the top level program unit of the model, i.e. the one with 'program' in it, and optionally the desired new name $new_name of the model.

The script performs a simple transformation: 

  program $name 
  
becomes 

  subroutine program_$new_name(sys, tile, model_id)
  
Correspondingly,

  end program $name   

becomes 

  end subroutine program_$new_name
  
Furthermore:

 - the declarations for sys, tile and model_id are inserted before any other declarations 

 - the 'use' declarations for the generic and model-specific GMCF APIs are inserted immediately above the argument declarations
 
  #ifdef USE_GMCF
    use gmcfAPI
    use gmcfAPI$new_name_camel
  #endif

    integer(8) , intent(In) :: sys
    integer(8) , intent(In) :: tile
    integer , intent(In) :: model_id

  - the initialisation call is inserted immediately after the last variable declaration
  
  #ifdef USE_GMCF
     call gmcfInit${new_name_camel}(sys,tile, model_id)
  #endif 
=cut


sub gen_OpenCL_API_calls {
    (my $ocl_args_r, my $arg_names_r, my $const_arg_names_r,my $src_lines_r) = @_;
    my %ocl_args=%{$ocl_args_r};
    my @arg_names = @{$arg_names_r};
    my @const_arg_names = @{$const_arg_names_r};
    my @src_lines=@{$src_lines_r};
    my @gen_src_lines=();
#    push @gen_src_lines,  "!!! This code was generated on ".scalar( localtime())." from $prog_src using $0";
#    push @gen_src_lines,  "!!! DON'T EDIT !!! Edit $prog_src instead, and regenerate.";
    for my $line (@src_lines) {
        push @gen_src_lines,  $line unless $line=~/^\s*\!\s*\$ACC/;
        if ($line=~/^\s*\!\s*\$ACC\s+BufDecls/i) {
            push @gen_src_lines,  '! OpenCL buffer declarations';
            for my $arg_name ( @arg_names) {
                my $str = "integer(8) :: ${arg_name}_buf";
                push @gen_src_lines,   $str;
            }
        }
        if ($line=~/^\s*\!\s*\$ACC\s+SizeDecls/i) {
            push @gen_src_lines,   '! OpenCL buffer size declarations';
            for my $arg_name ( @arg_names) {
                my $dim = $ocl_args{$arg_name}{Dim};
                my $str = "integer, dimension($dim):: ${arg_name}_sz";
                push @gen_src_lines,   $str;
            }
        }

        if ($line=~/^\s*\!\s*\$ACC\s+MakeSizes/i) {
            push @gen_src_lines,   '! OpenCL buffer sizes';
            for my $arg_name ( @arg_names) {
				my $VVstr= "print *, '${arg_name}_sz: ',${arg_name}_sz ";
                my $str = "${arg_name}_sz = shape($arg_name)";
                push @gen_src_lines,   $str;
				push @gen_src_lines,   $VVstr if $VV;
            }
        }

        if ($line=~/^\s*\!\s*\$ACC\s+MakeBuffers/i) {
            push @gen_src_lines,   '! Create OpenCL buffers';
            for my $arg_name ( @arg_names) {
                my $dim = $ocl_args{$arg_name}{Dim};
                my $argmode = $ocl_args{$arg_name}{ArgMode};
                my $ctype = ucfirst( $ocl_args{$arg_name}{CType} );
                my $VVstr = "print *, '$arg_name'";                
                my $m_arg_name = $argmode eq 'Write' ? '' : ",$arg_name";
                my $str = "call oclMake${dim}D${ctype}Array${argmode}Buffer(${arg_name}_buf,${arg_name}_sz $m_arg_name)";
#say  $str;
                push @gen_src_lines,   $VVstr if $VV;
                push @gen_src_lines,   $str;
            }
        }

        if ($line=~/^\s*\!\s*\$ACC\s+SetArgs/i) {
            push @gen_src_lines,   '! Set OpenCL argument order';
#            my $argpos=0;
            my @ordered_arg_names = ();
            for my $arg_name ( @arg_names) {
                my $argpos = $ocl_args{$arg_name}{ArgPos};
                $ordered_arg_names[$argpos] = $arg_name;
            }
            for my $arg_name ( @ordered_arg_names) {
                next unless defined $arg_name; # this skips the const args
                my $argpos = $ocl_args{$arg_name}{ArgPos};
                my $ctype = ucfirst( $ocl_args{$arg_name}{CType} );
				my $VVstr = "print *, ' ${arg_name}_buf: ', $argpos";
                my $str = "call oclSet${ctype}ArrayArg($argpos, ${arg_name}_buf )";
                push @gen_src_lines,   $VVstr if $VV;
                push @gen_src_lines,   $str;
#                $argpos++;
            }
            my @ordered_const_arg_names = ();# @const_arg_names;
            for my $arg_name ( @const_arg_names) {
                my $argpos = $ocl_args{$arg_name}{ArgPos};
                $ordered_const_arg_names[$argpos] = $arg_name;
                
            }
            
            for my $const_arg_name ( @ordered_const_arg_names) {
                                if ($const_arg_name) {
                my $argpos = $ocl_args{$const_arg_name}{ArgPos};
                my $ctype = ucfirst( $ocl_args{$const_arg_name}{CType} );
				my $VVstr = "print *, ' $const_arg_name: ', $argpos";
                my $str = "call oclSet${ctype}ConstArg($argpos, $const_arg_name )";
                push @gen_src_lines,   $VVstr if $VV;
                push @gen_src_lines,   $str;
#                $argpos++;
                                }
            }

#            $argpos=0;
            for my $arg_name ( @arg_names) {
                my $ctype = lc( $ocl_args{$arg_name}{CType} );
                my $dim = $ocl_args{$arg_name}{Dim};
                $dim = ($dim == 1) ? ''  : $dim;
#                my $str = "__global ${ctype}$dim * ${arg_name}_buf,";
#                push @gen_src_lines,  '!',  $str;
#                $argpos++;
            }
#            for my $const_arg_name ( @const_arg_names) {
#                my $ctype = lc( $ocl_args{$const_arg_name}{CType} );
#                my $str = "const ${ctype} $const_arg_name,";
#                push @gen_src_lines,  '!',  $str;
#                $argpos++;
#            }            

        }
        if ($line=~/^\s*\!\s*\$ACC\s+WriteBuffers/i) {
            push @gen_src_lines,   '! Copy all arrays required for the full run';
            for my $arg_name ( @arg_names) {
                 my $dim = $ocl_args{$arg_name}{Dim};
                 my $ctype = ucfirst( $ocl_args{$arg_name}{CType} );
                if ($ocl_args{$arg_name}{ArgMode} ne 'Write') {
                    my $str="call oclWrite${dim}D${ctype}ArrayBuffer(${arg_name}_buf,${arg_name}_sz,$arg_name)";
                    push @gen_src_lines,   $str;
                }
            }
        }
        if ($line=~/^\s*\!\s*\$ACC\s+ReadBuffers/i) {
            push @gen_src_lines,   '! Read back Read and ReadWrite arrays';
            for my $arg_name ( @arg_names) {
                 my $dim = $ocl_args{$arg_name}{Dim};
                 my $ctype = ucfirst( $ocl_args{$arg_name}{CType} );
                if ($ocl_args{$arg_name}{ArgMode} ne 'Read') {
                    my $str="call oclRead${dim}D${ctype}ArrayBuffer(${arg_name}_buf,${arg_name}_sz,$arg_name)";
                    push @gen_src_lines,   $str;
                }
            }
        }
    }
    return [@gen_src_lines];
} # END of gen_OpenCL_API_calls()

sub get_c_type {
    (my $type, my $wordsz)=@_;

    if ($wordsz==4) {
        if ($type eq 'real') {
            return 'float';
        } elsif ($type eq 'integer') {
            return 'int';
        }
    }
    elsif ($wordsz==8) {
        if ($type eq 'real') {
            return 'double';
        } elsif ($type eq 'integer') {
            return 'long';
        }
    }

} # END of get_c_type()

=head1 Datastructures

=head2 Datastructure for kernels and subroutines

For every sub we find, we must return all kernels. So we must have a record for this

  $subs = {
    $current_sub => {
        VarLines=>$var_lines,
        ParLines=>$par_lines,
        Kernels =>{
          $current_kernel =>{
            GlobalRange=>$grange;
            LocalRange=>$lrange;
            SubName => $kernel_sub
            SubArgs => $args_str
        }, 
          ...
    }, 
        ...
  };    

=cut
# This is a very simple regex-based parser for 
#   - subroutine declarations and calls, 
#   - parameter declarations 
#   - $ACC Kernel/End Kernel pragmas
# It's used to parse both the wrapper module template and the kernel stub

sub parse_F90_src {
    (my $src)=@_;
    open my $IN, '<', $src or die "$src: $!. Did you create a stub .f95 for the kernel?";
    my @orig_lines=<$IN>;
    close $IN;
    my $src_lines = normalise_F90_src(\@orig_lines);
#die Dumper($src_lines);
    my $in_sub=0;
    my $in_kernel=0;
    my $var_lines=[];
    my $par_lines=[];
    my $subs={};
    my $current_sub='';
    my $current_kernel='';

    for my $line (@{$src_lines}) {
		next if $line=~/^\s*$/;
		next if ($line=~/^\s*[*!]/ || ($line=~/^C/i)) && $line !~/^\s*!\s*\$/; # We only support !$ACC, not c,C or *

        $line=~/^\s+subroutine\s+(\w+)/ && do {
            $in_sub=1;
            $current_sub=$1;
            $subs->{$current_sub}={};
            next;
        };    

        $in_sub && $line=~/^\s+end\s+subroutine/ && do {
            $in_sub=0;
            @{$subs->{$current_sub}{VarLines}}=@{$var_lines};
            @{$subs->{$current_sub}{ParLines}}=@{$par_lines};
            $current_sub='';
            $var_lines=[];
            $par_lines=[];
            next;
        };    
        $line=~/::/ && do { 
            if($line=~/\Wparameter\W/ ) {
            push @{$par_lines}, $line;
            } else {
            push @{$var_lines}, $line;
            }
            next;
        };
        
        $line=~/^\s*\!\s*\$ACC\s+Kernel\s*\((\w+)\)/i && do {
            $in_kernel=1;
            $current_kernel=$1;
            if ($line=~/GlobalRange\s*\((.+?)\)/) {
                my $grange=$1;
                $subs->{$current_sub}{Kernels}{$current_kernel}{GlobalRange}=$grange;
            } else {
                $subs->{$current_sub}{Kernels}{$current_kernel}{GlobalRange}='1';
            }
            if ($line=~/LocalRange\s*\((.+?)\)/) {
                my $lrange=$1;
                $subs->{$current_sub}{Kernels}{$current_kernel}{LocalRange}=$lrange;
            } else {
                $subs->{$current_sub}{Kernels}{$current_kernel}{LocalRange}='1';
            }
            if ($line=~/CppMacros\s*\((.+?)\)/) {
                my $macros_defs=$1;
                $subs->{$current_sub}{Kernels}{$current_kernel}{CppMacros}=$macros_defs;
            } else {
                $subs->{$current_sub}{Kernels}{$current_kernel}{CppMacros}='';
            }
            next;
        };
        $line=~/^\s*\!\s*\$ACC\s+End\s+Kernel/i && do {
            $in_kernel=0;
            $current_kernel='';
			next;
        };
        if ($in_kernel==1 && $line=~/^\s+call\s+(\w+)\s*(.+)/) {
            my $kernel_sub = $1;
            my $args_str = $2;
            $subs->{$current_sub}{Kernels}{$current_kernel}{SubName}=$kernel_sub;
            $subs->{$current_sub}{Kernels}{$current_kernel}{SubArgs}=$args_str;
        }
    }
      if(  $in_sub ) {
            @{$subs->{$current_sub}{VarLines}}=@{$var_lines};
            @{$subs->{$current_sub}{ParLines}}=@{$par_lines};
        }      
    return ($src_lines,$subs);
}  # END of parse_F90_src()

###############################################################################
# Code for parsing the argument declarations
=head2 OpenCL Kernel Arguments

The call to `parse_arg_decls($var_lines,$kernel_args)` returns a table `%ocl_args` 
and two lists, one with the 'normal' (i.e. pointer) arguments, and one with the constant arguments.
The table has following structure

    %ocl_args =(
        $arg_name => {
            WordSize => $wordsz,
            Type => lc($type),
			CType => $ctype,          
            ArgMode => $argmode,
			Dim => $ndims,
            ArgPos => $arg_pos
        },
        ...        
    );

=cut
sub parse_arg_decls {
    (my $var_lines, my $kernel_args)=@_;
#    local $vv=1;
    say '=' x 80 if $vv;
    my $all = defined $kernel_args? 0 : 1;
    my %ocl_args=();
    my @arg_names=();
     my $ocl_arg_names=[];
     my $ocl_const_arg_names=[];

	for my $str (@{$var_lines}) {
		say "\n>>> <$str>\n" if $vv;
#		say STDERR "\n>>> <$str>\n";

		my $pt = parse_F95_arg_decl($str);
		print "ARGDECL:".Dumper($pt),"\n" if $vv;
		my $type = $pt->{TypeTup}{Type};
		my $wordsz = $pt->{TypeTup}{Kind};
		my $ndims = scalar @{ $pt->{Dim} };
        if ($ndims==1 && $pt->{Dim}[0]!~/:/ && $pt->{Dim}[0] eq '0') {
            $ndims=0;
        }
        my @var_names=@{ $pt->{Args} };
		@arg_names=(@arg_names, @var_names);
		my $argmode =  $ndims==0 ? 'Const' : $pt->{AccPragma}{AccVal};
        if ($argmode eq 'ReadWrite') {
            # check intent
            if (exists $pt->{Intent}) {
            my $intent = $pt->{Intent};
            if ($intent ne 'InOut') {
                $argmode = ($intent eq 'In') ? 'Read' : 'Write' ;
            }
            }
        } 
        
# Now I overload ArgMode to indicate if a value is const. Simply, any scalar is a const. 
die if not defined $type;
		say "$type,$wordsz,$ndims ",@var_names, " $argmode" if $vv;
       		for my $arg_name (@var_names) {
#say STDERR "<$arg_name>";                
#                if (not defined $arg_name) {say STDERR Dumper($pt); 
#                    say STDERR Dumper(@var_names);
#                }
            if ($all or exists $kernel_args->{$arg_name}) {
                   if ($argmode eq 'Const') {
                push @{$ocl_const_arg_names}, $arg_name;
                } else {
                push @{$ocl_arg_names}, $arg_name;
                }
                $ocl_args{$arg_name}{WordSize} = $wordsz;
			$ocl_args{$arg_name}{Type} = lc($type);
			my $ctype = get_c_type(lc($type),$wordsz);
			$ocl_args{$arg_name}{CType} = $ctype;
            
			$ocl_args{$arg_name}{ArgMode} = $argmode;
			$ocl_args{$arg_name}{Dim} = $ndims;
            $ocl_args{$arg_name}{ArgPos} = $kernel_args->{$arg_name};
            }
		}
	} # for
	return (\%ocl_args,$ocl_arg_names,$ocl_const_arg_names);
} # end parse_arg_decls()
#--------------------------------------------------------------------------------
sub normalise_F90_src {(my $orig_lines)=@_;

                my $in_cont              = 0;
                my $joinedline='';
                my @comments_stack       = ();
                my $joined_lines=[];
				for my $line (@{$orig_lines}) {		
                    chomp $line;
                    if (not isCommentOrBlank($line) && $line=~/;/) {

                        my $tline = $line;
                        my $nline = '';
                        my $i     = 0;
                        my %phs   = ();
                        while ( $tline =~ /(\'.+?\')/ ) {
                            $phs{"__PH${i}__"} = $1;
                            $tline =~ s/(\'.+?\')/__PH${i}__/;
                            $i++;
                        }
                        if ( $tline =~ /;/ ) {
                            my @tlines = split( /\s*;\s*/, $tline ) ;
                            my @nlines=();
                            for my $nline (@tlines) {                
                                for my $phk ( keys %phs ) {
                                    $nline =~ s/$phk/$phs{$phk}/;
                                }
                                push @nlines, $nline;
                            }
                            $line = pop @nlines;
                            for my $tline (@nlines) {
                                push @{$joined_lines}, $tline;
                            }
                        } else {
                            for my $phk ( keys %phs ) {
                                $tline =~ s/$phk/$phs{$phk}/;
                            }
                            $line = $tline;
                        }
                    }
					if ($in_cont==0) {
					    if ( isCont( $line ) ) {
#                            die "CONT:$line";
						   $in_cont=1;
					       $joinedline .= removeCont( $line );	
					    } else {
					        # emit line
					        if ( $line ne '' ) {
					            push @{$joined_lines}, $line;
					        }	
					    }
					} else { # inside continuation line
						if ( isCont( $line ) ) {
						   $joinedline .= removeCont( $line );
						} elsif ( isCommentOrBlank($line) ) {
					        push @comments_stack, $line;
						} else {
							# In cont but line is not cont => end of cont line => 
                            # I still call removeCont to clean up the line
						   $joinedline .= removeCont( $line );                            
							# emit comments;
					        for my $commentline (@comments_stack) {
                                push @{$joined_lines}, $commentline;
					        }                                        
					        @comments_stack       = ();
							# emit joined line
					        if ( $joinedline ne '' ) {
					            push @{$joined_lines}, $joinedline;
					        }   
							$joinedline='';
							$in_cont=0;
						}	
					}
				}
                return $joined_lines;
} # END of normalise_F90_src()
 # -----------------------------------------------------------------------------
	sub isCont {
		( my $line ) = @_;
		my $is_cont = 0;
			if ( $line =~ /^\s*\&/ or $line=~/&\s*$/) {
				$is_cont = 1;
			}		
		return $is_cont;
	} # END of isCont


 # -----------------------------------------------------------------------------
	sub isCommentOrBlank {
		( my $line ) = @_;

		# Detect comments & blank lines
		if ( $line =~ /^[CD\*\!]/i or $line =~ /^\ {6}\s*\!/i ) {
			return 1;
		} elsif ( $line =~ /^\s*$/ ) {
			return 1;
		}
		return 0;
	} # END of isCommentOrBlank
 # -----------------------------------------------------------------------------
	sub removeCont {
		( my $line ) = @_;
		chomp $line;
		if ( isCommentOrBlank($line) ) {
			return '';
		}
        if ( $line =~ /^\s*\&/ ) {
            $line =~ s/^\s*\&\s*/ /;
        }
        if ( $line =~ /\&\s*$/ ) {
            $line =~ s/\&\s*$//;
        }
# I think I should remove the whitespace at the start of the line
#		$line=~s/^\s+// unless $line;
		if ( $line =~ /.\!.*$/ ) {    # FIXME: trailing comments are discarded!
			my $tline = $line;
			my $nline = '';
			my $i     = 0;
			my %phs   = ();
			while ( $tline =~ /(\'.+?\')/ ) {
				$phs{"__PH${i}__"} = $1;
				$tline =~ s/(\'.+?\')/__PH${i}__/;
				$i++;
			}
			if ( $tline =~ /\!.*$/ ) {
				$nline = ( split( /\!/, $tline ) )[0];
				for my $phk ( keys %phs ) {
					$nline =~ s/$phk/$phs{$phk}/;
				}
				$line = $nline;
			} else {
				for my $phk ( keys %phs ) {
					$tline =~ s/$phk/$phs{$phk}/;
				}
				$line = $tline;
			}
		}
		return $line;
	} # END of removeCont()
#-------------------------------------------------------------------------------- 
# TODO: In general this is complicated, because the args can be expressions, array slices etc.
# So we'd need a proper parser
# We assume they are all nice simple var names for now

=head2 Kernel Arguments

The call to `get_kernel_args($argstr)` returns a map `%argtable`,

    %argtable=( $arg_name => $arg_pos, ... );
    
=cut

sub get_kernel_args { (my $argstr)=@_;
    say "ARGSTR: $argstr" if $VV;
    $argstr=~s/^\(\s*//;
    $argstr=~s/\s*\)$//;    
    my @args=split(/\s*,\s*/, $argstr);
    my $arg_pos=0;
    my %argtable = map {$_ => $arg_pos++ } @args;
    return {  %argtable };
} # END of get_kernel_args()
#-------------------------------------------------------------------------------- 
# Parse the kernel stub, and find for every argument its Intent, and update OclArgs
sub get_intent_from_kernel_sub { (my $ksub_name, my $ocl_args) =@_;
    my $ksub_path = $kstub_path.'/'.$ksub_name.'_kernel.f95';
    (my $src_lines,my $subs) = parse_F90_src($ksub_path);
#die "SUBS: ".Dumper($subs);
    (my $subname, my $record) = each %{$subs}; # TODO: Assuming there is only 1 sub in the source file! WEAK!
        my $var_lines = $record->{VarLines};
    (my $kernel_args,my $arg_names)  = parse_arg_decls($var_lines);
    my $arg_pos=0;
    for my $arg_name (@{$arg_names}) {
        if (exists $ocl_args->{$arg_name}) {
        $ocl_args->{$arg_name}{ArgMode} = $kernel_args->{$arg_name}{ArgMode};                
        } else {
            say "WARNING: $ksub_name: declared kernel argument $arg_name does not match any in called argument names.\n";
            # Try to match by position
             for my $ocl_arg_name (keys %{$ocl_args}) {
                 if ($ocl_args->{$ocl_arg_name}{ArgPos} == $arg_pos) {
                 if (not exists $kernel_args->{$ocl_arg_name}) {
                    $ocl_args->{$ocl_arg_name}{ArgMode} = $kernel_args->{$arg_name}{ArgMode};                
                 } else {
                     say "WARNING: $ksub_name: declared kernel argument $arg_name in position $arg_pos would overwrite argument $ocl_arg_name!\n";
                 }
                 }
             }
        }
        $arg_pos++;
    }    
    return $ocl_args;
} # END of get_intent_from_kernel_sub()

#-------------------------------------------------------------------------------- 
=info_FIXME

The program can contain #ifdefs and #ifs and we should not be inside one of these! Preferably, we should backtrack and insert before.
To do so, I need two passes, and mark the line numbers:
If we find a #if, and $macro_level==0, we mark the line number. If we need to insert code before macro_level is back to 0, we need to insert it at the marked position.
If we reach an insert condition while  $macro_level == 0, we mark that number.

=cut

sub generate_gmcf_subroutine_code { (my $src_lines)=@_;
    
    my $gen_src_lines=[];
    my $after_var_decls=0;
    my $var_decls = 0;
    my $no_var_decls=1;
    my $macro_level=0;
    my %code_to_insert = ();
    my %insert_or_replace = ();
    my $current_0_lev_idx = 0;
    my $current_code_to_insert=[];
    my $done_decls=0;
    my $done_init=0;
    
    for my $idx (0 .. scalar @{$src_lines} -1) {
     my $raw_line = $src_lines->[$idx];
       my $line = lc($raw_line);
        if ($macro_level==0) {
       $current_0_lev_idx=$idx;
        }
       if ($line=~/^\s*\#if/) {
        if ($macro_level==0) {
         $current_0_lev_idx=$idx;
        }
        $macro_level++;
#        push @{$gen_src_lines},$raw_line;
       } elsif ($line=~/^\s*\#endif/) {
         $macro_level--;
         if ($macro_level==0) {
         $current_0_lev_idx=$idx+1;
        }
       } elsif ($line=~/^\s*$/ or $line=~/^\s*!|^[cC]/) {
#        push @{$gen_src_lines},$raw_line;
#        print "SKIP $line\n";
        next;
       } elsif ($line=~/^\s*program\s+(\w+)/) { # WV: I ignore the possibility that the program declaration is inside a macro condition
         my $old_name = $1;
         if ($new_name eq '') { 
          $new_name = $old_name;
         }
         my $new_line="subroutine program_$new_name(sys, tile, model_id)";
         if (exists $code_to_insert{$idx}) {
          push @{$code_to_insert{$idx}}, $new_line;
         } else {
          $code_to_insert{$idx}=[$new_line];
         }          
         $insert_or_replace{$idx}=0; # 0 = replace, 1 = insert
          next;
       } elsif ($line=~/^\s*end\s+program(\s+\w+)?/) {
#         my $old_name = $1;
#         if ($new_name eq '') { 
#          $new_name = $old_name;
#         }
          my $new_line = "end subroutine program_$new_name";
                   if (exists $code_to_insert{$idx}) {
          push @{$code_to_insert{$idx}}, $new_line
         } else {
          $code_to_insert{$idx}=[$new_line];
         }     
         $insert_or_replace{$idx}=0; # 0 = replace, 1 = insert
          next;
        } elsif (
        (($line=~/=/ or $line=~/\s+call\s+/) and $no_var_decls==1) or         
       ($line=~/::/ and $no_var_decls==0)
       ) {
        
#         print "IDX at DECLS: $current_0_lev_idx\n";
         if ($line=~/::/) { 
         $no_var_decls=0;
         $var_decls = 1;         
         if ($after_var_decls==0) {
          $after_var_decls=1;
         }
         }
         if ($done_decls==0) {
          $done_decls=1;
         my $gmcf_decls=[
          '#ifdef USE_GMCF',
          '    use gmcfAPI',
          "    use gmcfAPI$new_name_camel_lc1st",
          '#endif',
          '    integer(8) , intent(In) :: sys',
          '    integer(8) , intent(In) :: tile',
          '    integer , intent(In) :: model_id'
         ];
         if (exists $code_to_insert{$current_0_lev_idx}) {
          $code_to_insert{$current_0_lev_idx} = [@{$code_to_insert{$current_0_lev_idx}},@{$gmcf_decls}] 
         } else {
          $code_to_insert{$current_0_lev_idx}=$gmcf_decls;
         }                  
        $insert_or_replace{$current_0_lev_idx}=1; # 0 = replace, 1 = insert
         }
       } elsif ($line!~/::/ && $after_var_decls==1) {
        $after_var_decls=2;        
        if ($done_init==0) {
         $done_init=1;
         my $new_line= "    call gmcfInit$new_name_camel(sys,tile, model_id)";        
            if (exists $code_to_insert{$current_0_lev_idx}) {
          push @{$code_to_insert{$current_0_lev_idx}}, $new_line;
         } else {
          $code_to_insert{$current_0_lev_idx}=[$new_line];
         }           
         $insert_or_replace{$current_0_lev_idx}=1; # 0 = replace, 1 = insert
        }  
       }             
    }
     for my $idx (0 .. scalar @{$src_lines} -1) {
      my $raw_line = $src_lines->[$idx];
#     say "IDX: $idx";
      if (exists  $code_to_insert{$idx} ) {
#      say "IDX: GOT";
       for my $new_line (@{$code_to_insert{$idx}}) {
        push @{$gen_src_lines}, $new_line;
       }
      }
      if (not exists $insert_or_replace{$idx} or
      (exists $insert_or_replace{$idx} and $insert_or_replace{$idx}==1) 
       ){
      push @{$gen_src_lines},  $raw_line;
      }
     } 
    
    return $gen_src_lines ;
   }       
sub generate_gmcf_subroutine_code_OFF { (my $src_lines, my $subs)=@_;
    my $gen_src_lines_without_ocl_decls=[];
    my $gen_src_lines=[];
    my $in_kernel=0;
    my $in_sub=0;
    my $after_var_decls=0;
    my $current_sub='';
    my $current_kernel='';
    my %gen_ocl_api_decl_lines=();
    my $gen_src_ocl_api_decl_lines=[];
    for my $raw_line (@{$src_lines}) {
       my $line = lc($raw_line);
# We can push all existing lines except the kernel part
         if ( $line=~/^\s*\!\s*\$ACC\s+Kernel\s*\((\w+)\)/i ) {
            $in_kernel=1;
            $current_kernel=$1;
            next;
        } elsif ($in_kernel) {
            
            $line=~/^\s+call\s+/ && do {
            my $cline = $line;
            $cline=~s/^\s+call/! call/;
            # here generate all that code
            my $ws='';
            $line=~/^(\s+)/ && do { $ws.=$1; };
            my $global_range = $subs->{$current_sub}{Kernels}{$current_kernel}{GlobalRange};
            my $local_range = $subs->{$current_sub}{Kernels}{$current_kernel}{LocalRange};
            
# Rather than generating this code, it makes more sense to call the corresponding code generators!
            my $ocl_kernel_api_decl_lines = [
                    '',
                    '!$ACC BufDecls',
                    '!$ACC SizeDecls',
                    '',                    
                ];
            my $ocl_kernel_api_lines = [
                    '',
                    'if ( init_ocl_local /= 1 ) then ',
                    '  init_'.$current_sub.'_local = 1',
                    
                    'if ( init_'.$current_sub.' /= 1 ) then ',
                    '  init_'.$current_sub.' = 1',
                    '!  call oclInitOpts(srcstr,kstr,koptsstr)',
                    '  call oclInit(srcstr,kstr)',
                    '',                                        
                    '  call oclGetMaxComputeUnits('.$current_sub.'_nunits)',
                    '  call oclGetNThreadsHint('.$current_sub.'_nthreads)',
                    '',
                    '!  print *, "Compute units:",'.$current_sub.'_nunits," Threads:",'.$current_sub.'_nthreads',
                    'end if',
                    '',
                    '!$ACC MakeSizes',
                    '',
                    '!$ACC MakeBuffers',
                    '',
                    '!$ACC SetArgs',
                    '',  
                    'end if',
                    '',
                    $current_sub."_globalrange = $global_range",
                    $current_sub."_localrange = $local_range",
                    'if ('.$current_sub.'_localrange == 0) then',
                    '  call padRange('.$current_sub.'_globalrange,'.$current_sub.'_nunits*'.$current_sub.'_nthreads)',                      
                    'end if',
                    '',
                    '!$ACC WriteBuffers',
                    '',
                    $cline,
                    "call runOcl($current_sub\_globalrange,$current_sub\_localrange)",
                    '',
                    '!$ACC ReadBuffers', 
                    ''
                    ];
                
                    (my $ocl_args,my $arg_names,my $const_arg_names)= @{$subs->{$current_sub}{Kernels}{$current_kernel}{OclArgs}};

                    my $gen_ocl_kernel_api_decl_lines = gen_OpenCL_API_calls ( $ocl_args,$arg_names,$const_arg_names, $ocl_kernel_api_decl_lines);
                    my $gen_ocl_kernel_api_lines = gen_OpenCL_API_calls ( $ocl_args,$arg_names,$const_arg_names, $ocl_kernel_api_lines);

                 for my $ocl_kernel_api_decl_line (@{$gen_ocl_kernel_api_decl_lines}) {
                     if (not exists $gen_ocl_api_decl_lines{$ocl_kernel_api_decl_line}) {
                        $gen_ocl_api_decl_lines{$ocl_kernel_api_decl_line}=1;
                        push @{$gen_src_ocl_api_decl_lines},$ws.$ocl_kernel_api_decl_line;

                     }                
                 }
                 for my $ocl_kernel_api_line (@{$gen_ocl_kernel_api_lines}) {
                                         push  @{$gen_src_lines_without_ocl_decls}, $ws.$ocl_kernel_api_line ;
                 }
            
            };
            $line=~/^\s*\!\s*\$ACC\s+End\s+Kernel/i && do {
                $in_kernel=0;
                $current_kernel='';
            };
            next;
        } else {        
              
# Immediately after the use declarations:
#	'integer :: init_ocl_done_$subname'  
              if ($line=~/^\s*contains\s*$/) {                  
                  my $ws='    ';
                  $line=~/^(\s+)/ && do {
                    $ws.=$1;
                  };
                  my @subnames = keys %{$subs};
                  my @init_var_names = map {'integer :: init_'.$_.' = 0'} @subnames;
                  for my $init_var (@init_var_names) {                  
                  push @{$gen_src_lines_without_ocl_decls}, $ws.$init_var;
                  }
              }
              push @{$gen_src_lines_without_ocl_decls}, $line;
# Immediately after the subroutine which wraps the subroutine call marked as Kernel:
#	'use OclWrapper'  
              if ($line=~/^\s+subroutine\s+(\w+)/) {
                  $current_sub=$1;
                  my $ws='    ';
                  $line=~/^(\s+)/ && do {
                    $ws.=$1;
                  };
                  push @{$gen_src_lines_without_ocl_decls}, $ws.'use oclWrapper';
              }

# Immediately after the existing declarations:
              if ($line=~/::/) {
#                    my $pt = parse_F95_arg_decl($line);
#                    my @var_names=@{ $pt->{Args} };
                    if ($line eq $subs->{$current_sub}{VarLines}[-1]) {
                        my $ws='';
                        $line=~/^(\s+)/ && do { $ws.=$1; };
                        my @kernel_names = keys %{$subs->{$current_sub}{Kernels}}; #FIXME: only one kernel per sub supported!
                            if (scalar @kernel_names >1) {
                                die "Sorry, only one kernel pragma per wrapper subroutine is supported.\n";
                            }
                        if (@kernel_names) {    
                            my $kernel_name = shift @kernel_names;
                            my $macro_defs_str = $subs->{$current_sub}{Kernels}{$kernel_name}{CppMacros};
                            my @macro_defs = split(/\s*\,\s*/,$macro_defs_str);
                            my $kernel_opts= join(' ',map {'-D'.$_ } @macro_defs);
                        
#FIXME: the path to the kernel is ad-hoc!
#say '#DEBUG:',$current_sub,':',$kernel_name ; # FIXME: something WEAK here!!!
                        my @ocl_decl_lines =(
                                '',
                                '! OpenCL-specific declarations',
                                'integer :: '.$current_sub.'_nunits, '.$current_sub.'_nthreads, '.$current_sub.'_globalrange, '.$current_sub.'_localrange',
                                'integer :: init_'.$current_sub.'_local',
                                '! integer :: srclen, klen',
                                'character(len=*), parameter :: srcstr="'.$kernel_path.'/'.$kernel_name.'.cl"',
                                'character(len=*), parameter :: kstr="'.$kernel_name.'"',
                                '! character(len=*), parameter :: koptsstr="'.$kernel_opts.'"',
                                '!This is a hook to insert the actual buffer declarations!',
                                '!$ACC BufDecls',
                                ''
                                );
                        for my $ocl_decl_line (@ocl_decl_lines) {
                            push  @{$gen_src_lines_without_ocl_decls}, $ws.$ocl_decl_line ;
                        }
                        }
                    }
              }
        }
#say "CURRENT SUB: $current_sub, KERNEL: $current_kernel: $line";

    }
    for my $gen_src_line (@{$gen_src_lines_without_ocl_decls}) {
        if ($gen_src_line =~/\!\$ACC\s+BufDecls/) { 
            for my $ocl_decl_line (@{$gen_src_ocl_api_decl_lines}){
                push @{$gen_src_lines}, $ocl_decl_line;
            }

# local guard around OpenCL init etc, to makes sure it happens only once per call to a subroutine
               my $init_local_decl = 'integer :: init_ocl_local';
                my $init_local_assignment = 'init_ocl_local = 0';
                push @{$gen_src_lines}, '    '.$init_local_decl;
                push @{$gen_src_lines}, '    '.$init_local_assignment;
                
        } else {
            push @{$gen_src_lines}, $gen_src_line;
        }
    }

    return $gen_src_lines;
} # END of generate_gmcf_subroutine_code()

###############################
## Read the source and get the subroutine records:
#$subname => {
#        VarLines => [String],
#        Parlines=> [String],
#
#        Kernels => {$kname => {
#                GlobalRange => String,
#                LocalRange => String,
#                SubName => String,
#                SubArgs => String,
#        }},
#}
sub main {

my %opts=();
getopts('hvop:n:', \%opts);

if ($opts{'h'}) { 
        die "
            Please specify a template module file (V2)
            To overwrite any existing modules, use -o
            For verbose output, use -v
            ";
}
    
if($opts{'v'}) {
    $VV=1;
    $vv=1;
}
my $overwrite=0;
if($opts{'o'}) {
 $overwrite=1;
}

our $prog_src= '';
if ($opts{'p'}) {
 $prog_src =$opts{'p'};
} elsif (@ARGV) {   
    $prog_src = $ARGV[0];
} else {
 die "You must provide the full path to the top level program unit with -p.\n";
}
$src_gen =  $prog_src;
my $prog_path = $prog_src;

$prog_path =~s/\w+\.\w+$//;
if ($prog_path eq '') {
 $prog_path='./';
}

if ($prog_src!~/\.[Ff](?:9[50]|0[38])?/) {
 die "Only Fortran-90 or later is supported. If your source is Fortran-77, you can convert it into Fortran-95 using 
 rf4a, https://github.com/wimvanderbauwhede/RefactorF4Acc\n";
}

if ($opts{'n'}) {
 $new_name=$opts{'n'};
 
 my @parts=split(/_/,$new_name);
 my @cparts = map {ucfirst($_)} @parts;
 $new_name_camel=join('',@cparts);
 $new_name_camel_lc1st=lcfirst($new_name_camel);
 $src_gen = $new_name.'.f95';
} else {    
    $src_gen=~s/^.+\///;
     $src_gen=~s/\.[fF]9[05]/_gmcf./
}
    $src_gen="$prog_path/$src_gen";
    
# Parse the source into a list of lines and a map of subroutine records, see above
    (my $src_lines,my $subs) = parse_F90_src($prog_src);
#die Dumper($src_lines);

## For each of the subroutine records, get the kernel arguments, and identify the corresponding variable declarations
## We need to generate OclWrapper calls for every kernel in every subroutine, separately.
## So we need to replace the !$ACC Kernel pragma with this chunk of code. 
## So we first generate that code, put it into %subs,  and then do another pass and substitute it.
#    for my $csub (keys %{$subs}) { 
#        say "SUB:$csub" if $VV;
#        my $var_lines = $subs->{$csub}{VarLines};
#        for my $ckname (keys %{$subs->{$csub}{Kernels}}) {
#            say "KERNEL:$ckname" if $VV;
#            my $kernel_args = get_kernel_args( $subs->{$csub}{Kernels}{$ckname}{SubArgs} );
#            (my $ocl_args,my $arg_names, my $const_arg_names) = parse_arg_decls($var_lines, $kernel_args);
## This is fine, at this stage we have all kernel arguments. 
## Assuming that the kernel subroutine has the Intent field present, we can use that to get the directions of the arguments
#            $ocl_args = get_intent_from_kernel_sub( $subs->{$csub}{Kernels}{$ckname}{SubName}, $ocl_args );        
#            $subs->{$csub}{Kernels}{$ckname}{OclArgs}=[$ocl_args,$arg_names,$const_arg_names];
## Now we have the intent. If the kernel sub were simple, and consisted only of a single, possible nested, loop, we could derive the oclglobalrange as well.        
## But for now, let's just use a range of 1        
#
#        }
#    }
    my $gen_src_lines = generate_gmcf_subroutine_code($src_lines);
if ($overwrite) {
    open my $GENSRC,'>',"$wrapper_dir/$src_gen" or die $!;
    map {say $GENSRC $_} @{$gen_src_lines};
    close $GENSRC;
} else {
    map {say $_} @{$gen_src_lines};
}
} # END of main()


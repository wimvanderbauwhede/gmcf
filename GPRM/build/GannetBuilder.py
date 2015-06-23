'''\file GannetBuilder.py

    \brief Gannet SBA - SCons script for building Gannet/GPRM/GMCF

'''

##   (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>

# $Id$


###############################################################################
# DON'T MODIFY ANYTHING BELOW THIS
###############################################################################

import os
import re
import commands
import sys
#sys.path+=['/usr/lib/scons/']

from SCons.Variables import Variables
from SCons.Environment import Environment
#from SCons.Help import Help

#from cxxtestgenlib import createRunner


def build(wd,sources,flibs):

    destdir='../SBA/'
    global opts

    flags=[]
    switches=[]
    boost=0

    # MACROS
    wordsz=32
    WORDSZ='WORDSZ='+str(wordsz)
    GL='GANNET_LANGUAGE'
    NEW=''
    VERBOSE='' #V='VERBOSE'
    # No SystemC by default
    SYSC=''
    SC_IDP=''
    SYSC_FIXME=''
    # Compile for VM (otherwise compiles to model HW)
    VM='VM=0'
    SEQVM='SEQVM=0'
#    OLDVM='OLDVM=1'
    USE_THREADS='USE_THREADS=0'
    DISTR='DISTR=0'
    use_pthreads = False
    # B ashkan
    USE_TILERA='' #X= 'USE_TILERA'
    use_tilera = False
    # E ashkan
    THREADED_CORE='' # 'THREADED_CORE=0'
    threaded_core = False
    # Count CPU cycles
    CYCLES='' # 'CYCLES'
    TIMINGS='' # 'TIMINGS'
    STATIC_ALLOC='STATIC_ALLOC'

    # Flags
    #Smash= '-fstack-protector '# Ashkan
    WARN='' #'-Wall '
    CXX11 = '-std=c++11'
    OPTSPEED    = '-O3 -fno-exceptions -fno-rtti '
    OPTSIZE = '-Os -fno-exceptions -fno-rtti '
    OPTTHREADS = '-O3 '
    DEBUG = ''
    ARCH=''
    NO_SOCKET=''
    OPT = '-O3 '
    CYGWIN=0
    PIC = '' #'-fPIC '
    # These are used by the build script to generate flags/switches
    OSX=0
    # Flag for cross-compilation
    XC=0
    # Use LLVM
    # LLVM=1: x86, LLVM=2: ppc
    LLVM=0
    # SystemC
    SC=0
    H=0 # Help
    LIB=False
    MACROS = ['INTERFACE_OBJ']
    yaml_config='../../SystemConfigurations/SBA.yml'

    #use options without leading '-': scons v=0 gui=QtGui
    opts = Variables()
    opts.Add('v', 'Verbose', 0)
    opts.Add('w', 'Warnings', 0)
    opts.Add('new', 'New', 0)
    opts.Add('xc', 'Crosscompile',0)
    opts.Add('llvm', 'Use LLVM',0)
    opts.Add('win','CygWin',0)
    opts.Add('vm', 'Virtual Machine',0)
    opts.Add('sock', 'Use POSIX socket interface',1)
    opts.Add('svm', 'Sequential Virtual Machine',0)
# options can't take . or / in the strings!!
#    opts.Add('yml','YAML configuration file','') #'../../SBA.yml')
    opts.Add('cycles', 'Count CPU cycles',0)
    opts.Add('timings', 'Time program execution',0)
    opts.Add('dyn', 'Dynamic memory',0)
    opts.Add('pthreads', 'Use POSIX Threads',0)
    opts.Add('lib', 'Compile as library',0)
    opts.Add('wordsz', 'Set WORDSZ',32)
    opts.Add('ptcore', 'Use POSIX Threaded Core',0)    
    opts.Add('dbg', 'Debug',0)
    opts.Add('nogen',"Don't generate C++ sources from Ruby code",0) 
    opts.Add('opt', 'Optimise','speed') # or 'size'
    opts.Add('D','Macros (add as a string: D="MACRO1:1 MACRO2 MACRO3:whatever"\nSCons is too stupid to allow "=")','') # add additional macros as a string
    opts.Add('h', 'Help',0)

    args=sys.argv[1:]
    #import getopt
    #rest = getopt.getopt(args,"hABCD")
    for arg in args:
        if re.match("(\w+)=(\w+)",arg):
            (k,v)=arg.split('=')
            opts.args[k]=v

    #exit(opts.options)

    for param in os.environ.keys():
        if param == "VERBOSE":
            VERBOSE='VERBOSE'
        if param == "GANNET_YML_CONFIG":
            yaml_config=os.environ["GANNET_YML_CONFIG"]

    for option in opts.options:
        if option.key == 'v' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            VERBOSE='VERBOSE'
        if option.key == 'w' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            WARN='-Wall '
        if option.key == 'new' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            NEW='NEW=1'
        if option.key == 'xc' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            XC=1
            # B ashkan
            use_tilera=True
            USE_TILERA='USE_TILERA'
            # E ashkan
            OPT=OPTSPEED
        if option.key == 'llvm' and opts.args.has_key(option.key): # and opts.args[option.key]!=option.default:
            if opts.args[option.key]=='1':
                LLVM=1
            elif opts.args[option.key]=='2':
                LLVM=2
            else:
                LLVM=0
            OPT=OPTSPEED
        if option.key == 'win' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            CYGWIN=1
        if option.key == 'vm' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            VM='VM=1'
        if option.key == 'svm' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            VM='VM=1'
            SEQVM='SEQVM=1'
# doesn't work if the path has dots or slashes!
#        if option.key == 'yml' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
#            print "YAML!"
#            yaml_config=opts.args[option.key]
        if option.key == 'sock' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            NO_SOCKET='NO_SOCKET'
            sockpatt=re.compile('^\.\.\/GannetSocket')
            nsources=filter(lambda s: not(sockpatt.search(s)),sources)
            sources=nsources
        if option.key == 'wordsz' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            wordsz=opts.args[option.key]
            WORDSZ='WORDSZ='+str(wordsz)
        if option.key == 'lib' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            LIB=True
        if option.key == 'pthreads' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            USE_THREADS='USE_THREADS=1'
            use_pthreads=True
            OPT=OPTTHREADS
        if option.key == 'ptcore' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            THREADED_CORE='THREADED_CORE=1'
            threaded_core=True
            OPT=OPTTHREADS
        if option.key == 'distr' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            DISTR='DISTR=1'
            OPT=OPTSPEED
        if option.key == 'cycles' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            CYCLES='CYCLES'
        if option.key == 'timings' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            TIMINGS='TIMINGS'
        if option.key == 'dyn' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            STATIC_ALLOC=''
        if option.key == 'dbg' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            DEBUG='-g -O0 -DGMCF_DEBUG ' #'-g -fno-exceptions -fno-rtti '
            OPT=''
        if option.key == 'opt' and opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            OPT=OPTSPEED
        if option.key == 'D' and  opts.args.has_key(option.key) and opts.args[option.key]!=option.default:
            macrostr=re.sub('\s*:\s*','=',opts.args[option.key])
            MACROS=macrostr.split(' ')
        if option.key == 'h' and opts.args.has_key(option.key):
            H=1

    if commands.getoutput("uname") == "Darwin":
        OSX=1
        switches+=['DARWIN']
        if SC==1:
            ARCH='-arch i386'
            
    # ashkan commented it        
    #if XC==1:
    #    switches.append('__ppc__')

    FLAGS=''
    SWITCHES=''
    flags+=[CXX11,WARN,DEBUG,ARCH,PIC,OPT]
    # Ashkan added USE_TILERA
    switches+=[SYSC,SC_IDP,SYSC_FIXME,VERBOSE,NEW,VM,SEQVM,WORDSZ,CYCLES,TIMINGS,STATIC_ALLOC,NO_SOCKET,USE_THREADS,THREADED_CORE,DISTR, USE_TILERA]+MACROS
    for flag in flags:
        if flag !='':
            FLAGS+=flag+' '
            

    for switch in switches:
        if re.search('BOOST',switch):
            boost=1
        if switch != '':
            SWITCHES+='-D'+switch+' '

    GMCF_DIR=os.environ["GMCF_DIR"]
    print "GMCF_DIR:"+GMCF_DIR
 			
    bin='gmcfCoupler'
    if LIB:
       sources.append(GMCF_DIR+'/GPRM/src/gmcf.cc')
    else:
       sources.append(GMCF_DIR+'/GPRM/src/gmcfCoupler.cc')
    #------------------------------------------------------------------------------
    CXX = os.environ['CXX']
    gcc_version = os.popen(CXX+' -dumpversion').read().rstrip("\n\r")
    gcc_version_tuple = [int(x) for x in gcc_version.split('.')]
# WV: HACK! TOO SPECIFIC!
    if (gcc_version_tuple[1]<6):
        raise Exception('You need at least g++ 4.6 for this!')
    else:
        cxx=CXX
    if XC==1:
       # B ashkan
       # cxx='powerpc-405-linux-gnu-g++'
         cxx='tile-c++'
       # E ashkan
    if LLVM==1:
        cxx='i686-pc-linux-gnu-g++'

    env = Environment(variables = opts, CXX = cxx, CXXFLAGS = FLAGS+SWITCHES)
#    env.VariantDir(wd+'build/',GMCF_DIR+'/GPRM/build/', duplicate=0)    	
#   Help(opts.GenerateHelpText(env))
    if H==1:
        print(opts.GenerateHelpText(env))
        exit(1)

    if XC==1:
        HOME=os.environ['HOME']
        env['ENV']['PATH']=os.environ["GANNET_XC_PATH"]+":"+env['ENV']['PATH']
        print env['ENV']['PATH']

    if LLVM==1:
        env['ENV']['PATH']=os.environ["GANNET_LLVM_PATH"]+":"+env['ENV']['PATH']
        print env['ENV']['PATH']
 
#FIXME: dl only needed for dynamic loading!
#libs=['m','dl'] 
# In order to link a fortran library compiled with gfortran, we must include the gfortran library. Wonder what the equivalent is for ifort, pgfortran?
#FIXME!    libs=flibs+['gfortran','m'] 
    libs = ['m']    
    if use_pthreads or threaded_core:
        libs+=['pthread']


    INCpaths=[wd+'/gensrc/',wd+'/gensrc/GMCF/Models/',wd+'/src/','.','../',GMCF_DIR+'/GPRM/src/SBA/',GMCF_DIR+'/GPRM/src/']
    LIBpaths=[wd+'/gensrc/GMCF/Models/']

    if OSX==1:
        LIBpaths=[wd+'/gensrc/GMCF/Models/','/opt/local/lib/gcc49/','/opt/local/lib/','/usr/local/lib/']

    #WV: to have multiple targets, we just need to set bin : bin is short for
    #env.Program(target=bin,...)

    if LIB:
            glib=env.Library('gmcf',sources,LIBS=libs,LIBPATH=LIBpaths,CPPPATH=INCpaths)
#            env.Alias('install_gmcf',env.Install(wd+'/lib',glib))
    else:
            prog=env.Program(bin,sources,LIBS=libs,LIBPATH=LIBpaths,CPPPATH=INCpaths)
            env.Alias('install',env.Install(wd+'/bin',prog))


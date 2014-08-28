""" \file SConstruct
   
 \brief GPRM - SCons script for building SystemConfiguration for C++ GPRM & SystemC
"""

# $Id$

import os
import re
import commands
import sys
#sys.path+=['/usr/lib/scons/']

ymlpath=ARGUMENTS.get('Y','.')
lib_path=ARGUMENTS.get('D','.')
sba_dir=ARGUMENTS.get('WD','.')

flags=''

#src_file='SBA/SystemConfiguration.rb'
target_file = lib_path+'/'+'SystemConfiguration.h'
script='./Cxx_SystemConfigurationOO.rb'
src_file=ymlpath # HACK! 

env = Environment()

cmd='GANNET_DIR='+os.environ['GANNET_DIR'] +' ruby '+script+' '+flags+' -Y '+ymlpath+' -D '+lib_path+' --cwd='+sba_dir
print cmd
gen=env.Command(target_file, src_file, cmd)
env.Alias('gen',target_file)
env.Depends(gen,script)
env.Depends(gen,ymlpath)

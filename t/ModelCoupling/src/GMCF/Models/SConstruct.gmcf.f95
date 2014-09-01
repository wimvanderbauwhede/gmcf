import os
from GannetBuilder import build

wd=ARGUMENTS.get('wd','.')


nmodels=ARGUMENTS.get('nmodels','')
NMODELS_FLAG = '-DNMODELS='+nmodels
FC=os.environ["FC"]
gmcflibsources=['./gmcf.f95']
LDFLAGS=[]
FFLAGS=['-Wall','-cpp',NMODELS_FLAG]
envF=Environment(F95=FC,LINK=FC,LINKFLAGS=LDFLAGS,F95FLAGS=FFLAGS,F95PATH=['.'])
envF.Library('gmcf',gmcflibsources) # ,LIBS=['m','netcdff'],LIBPATH=['.','/opt/local/lib','/usr/local/lib'])


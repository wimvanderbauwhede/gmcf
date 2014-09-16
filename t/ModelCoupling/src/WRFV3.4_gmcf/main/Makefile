# For GMCF, we need to compile wrf_gmcf into a static library

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES =  module_wrf_top.F

OBJS    =

LIBPATHS = 

include ../configure.wrf

#WRF_GMCF = wrf_gmcf # defined in configure.wrf
GMCF_LIB = lib$(WRF_GMCF).a

$(GMCF_LIB):     $(WRF_GMCF).o module_wrf_top.o	
	/bin/rm -f $(GMCF_LIB)
	$(AR) cr $(GMCF_LIB) $(WRF_GMCF).o module_wrf_top.o  gmcfAPIwrf.o 
	$(RANLIB) $(GMCF_LIB)
	/bin/cp $(GMCF_LIB) ../../GMCF/Models
	
libwrf_objs.a : $(OBJS_BUNDLED) 	
	/bin/rm -f libwrf_objs.a
	$(AR) cr libwrf_objs.a $(OBJS_BUNDLED)  
	$(RANLIB) libwrf_objs.a

$(WRF_GMCF)_main.o: $(WRF_GMCF)_main.cc
	$(CXXCOMP) -c -Wall -O2 -std=c++11 	$(WRF_GMCF)_main.cc
	
$(SOLVER)_$(WRF_GMCF) : $(WRF_GMCF)_main.o $(GMCF_LIB) libwrf_objs.a
	$(RANLIB) $(LIBWRFLIB)
	$(CXXCOMP) -o $(WRF_GMCF).exe $(LDFLAGS) $(WRF_GMCF)_main.o -L. -l$(WRF_GMCF) $(LIBWRFLIB) $(LIB)  -lwrf_objs -L ../../../lib  -lgmcf -lgmcfAPI -lgfortran

#$(SOLVER)_$(WRF_GMCF) : $(WRF_GMCF)_main.o $(GMCF_LIB)
#	$(RANLIB) $(LIBWRFLIB)
##	$(LD) -o $(WRF_GMCF).exe $(LDFLAGS) $(WRF_GMCF)_main.o ../main/module_wrf_top.o $(LIBWRFLIB) $(LIB) -L. -L ../../../lib -l$(WRF_GMCF) -lgmcf -lgmcfAPI -lstdc++
#	$(LD) -o $(WRF_GMCF).exe $(LDFLAGS) $(WRF_GMCF)_main.o -L. -l$(WRF_GMCF) $(LIBWRFLIB) $(LIB)  -L ../../../lib -lgmcf -lgmcfAPI -lstdc++


$(SOLVER)_wrf : wrf.o ../main/module_wrf_top.o
	$(RANLIB) $(LIBWRFLIB)
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o ../main/module_wrf_top.o $(LIBWRFLIB) $(LIB)

$(SOLVER)_wrf_SST_ESMF : wrf_ESMFMod.o wrf_SST_ESMF.o ../main/module_wrf_top.o
	$(RANLIB) $(LIBWRFLIB)
	$(LD) -o wrf_SST_ESMF.exe $(LDFLAGS) wrf_SST_ESMF.o wrf_ESMFMod.o ../main/module_wrf_top.o $(LIBWRFLIB) $(LIB)

$(SOLVER)_ideal : module_initialize ideal_$(SOLVER).o
	$(RANLIB) $(LIBWRFLIB)
	$(LD) -o ideal.exe $(LDFLAGS) ideal_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)

$(SOLVER)_real : module_initialize real_$(SOLVER).o ndown_$(SOLVER).o nup_$(SOLVER).o tc_$(SOLVER).o
	$(RANLIB) $(LIBWRFLIB)
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)
	$(LD) -o nup.exe $(LDFLAGS) nup_$(SOLVER).o  ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)
	$(LD) -o tc.exe $(LDFLAGS) tc_$(SOLVER).o  ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB)

convert_em : convert_em.o
	$(RANLIB) $(LIBWRFLIB)
	$(LD) -o convert_em.exe $(LDFLAGS) convert_em.o $(LIBWRFLIB) $(LIB)

convert_nmm : convert_nmm.o
	$(RANLIB) $(LIBWRFLIB)
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o $(LIBWRFLIB) $(LIB)

real_nmm : real_nmm.o
	( cd ../dyn_nmm ;  $(MAKE) module_initialize_real.o )
	$(RANLIB) $(LIBWRFLIB)
	$(FC) -o real_nmm.exe $(LDFLAGS) real_nmm.o $(LIBWRFLIB) $(LIB)

module_initialize : ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o
	( cd ../dyn_$(SOLVER) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o )

## prevent real being compiled for OMP -- only for regtesting
#$(SOLVER)_real : module_initialize real_$(SOLVER).o
#	$(RANLIB) $(LIBWRFLIB)
#	if [ -z "$(OMP)" ] ; then $(FC) -o real.exe $(LDFLAGS) real_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o $(LIBWRFLIB) $(LIB) ; fi
#
## prevent module_initialize being compiled for OMP --remove after IBM debugging
#module_initialize :
#	if [ -z "$(OMP)" ] ; then ( cd ../dyn_$(SOLVER) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o ) ; fi
# end of regtest changes

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

convert_nmm.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	$(ESMF_MOD_DEPENDENCE)

convert_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	$(ESMF_MOD_DEPENDENCE)

ideal_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_io_domain.o \
	../dyn_$(SOLVER)/$(CASE_MODULE) \
	$(ESMF_MOD_DEPENDENCE)

ideal_nmm.o: \
	../dyn_$(SOLVER)/$(CASE_MODULE) \
	../share/module_optional_input.o \
	../share/module_io_domain.o \
	../share/input_wrf.o 

ndown_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../frame/module_wrf_error.o \
	../frame/module_integrate.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	../share/module_get_file_names.o \
	../share/module_soil_pre.o \
	../dyn_em/module_initialize_$(IDEAL_CASE).o \
	../dyn_em/module_big_step_utilities_em.o \
	$(ESMF_MOD_DEPENDENCE)

nup_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_streams.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../frame/module_wrf_error.o \
	../frame/module_integrate.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	../share/module_get_file_names.o \
	../share/module_soil_pre.o \
	../dyn_em/module_initialize_real.o \
	../dyn_em/module_big_step_utilities_em.o \
	$(ESMF_MOD_DEPENDENCE)

# this already built above :../dyn_em/module_initialize.real.o \
real_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../dyn_em/module_initialize_$(IDEAL_CASE).o \
	../dyn_em/module_big_step_utilities_em.o \
	../share/module_io_domain.o \
	../share/module_date_time.o \
	../share/module_optional_input.o \
	../share/module_bc_time_utilities.o \
        ../dyn_em/module_wps_io_arw.o \
	$(ESMF_MOD_DEPENDENCE)
#	../chem/module_input_chem_data.o \
#	../chem/module_input_chem_bioemiss.o \


tc_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../dyn_em/module_initialize_$(IDEAL_CASE).o \
	../dyn_em/module_big_step_utilities_em.o \
	../share/module_io_domain.o \
	../share/module_date_time.o \
	../share/module_optional_input.o \
	../share/module_bc_time_utilities.o \
	$(ESMF_MOD_DEPENDENCE)

#WV GMCF
$(WRF_GMCF).o:  ../main/module_wrf_top.o gmcfAPIwrf.o
#WV GMCF
$(WRF_GMCF).o : $(WRF_GMCF).F
#WV GMCF
gmcfAPIwrf.o:  gmcfAPIwrf.F

wrf.o:  ../main/module_wrf_top.o

wrf_ESMFMod.o:  ../main/module_wrf_top.o

wrf_SST_ESMF.o:  wrf_ESMFMod.o

module_wrf_top.o: ../frame/module_machine.o \
                  ../frame/module_domain.o \
                  ../frame/module_integrate.o \
                  ../frame/module_driver_constants.o \
                  ../frame/module_configure.o \
                  ../frame/module_timing.o \
                  ../frame/module_wrf_error.o \
                  ../frame/module_state_description.o \
                  $(ESMF_MOD_DEPENDENCE)

# DO NOT DELETE

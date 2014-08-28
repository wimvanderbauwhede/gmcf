#include "GMCF.h"
#include "CastPointers.h"
#include "GMCFmodelF.h"

int GMCF::run_model1(const SBA::System* sba_sysptr,const SBA::Tile* sba_tileptr) {
    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
	const int64_t* sba_sys_ivp;
     *sba_sys_ivp=toWord<const SBA::System*>(sba_sysptr);
     const int64_t* sba_tile_ivp;
     *sba_tile_ivp=toWord<const SBA::Tile*>(sba_tileptr);
    int model = 1;
    // Here we call the actual Fortran function
    main_routine1_(sba_sys_ivp,sba_tile_ivp,&model);
    return 1; // purely for GPRM compatibility!
}


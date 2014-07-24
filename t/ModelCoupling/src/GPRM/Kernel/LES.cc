#include "LES.h"
#include "CastPointers.h"
#include "LESmodelF.h"

int LES::run_model(SBA::System* sba_sysptr,SBA::Tile* sba_tileptr) {
    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
    int64_t* sba_sys_ivp;
     *sba_sys_ivp=toWord<SBA::System*>(sba_sysptr);
    int64_t* sba_tile_ivp;
     *sba_tile_ivp=toWord<SBA::Tile*>(sba_tileptr);
    // Here we call the actual Fortran function
    lesmain_(sba_sys_ivp,sba_tile_ivp);
    return 1; // purely for GPRM compatibility!
}


#include "GMCF.h"
#include "CastPointers.h"
#include "GMCFmodelF.h"


int64_t GMCF::run_model1(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model1" << std::endl;
#endif
	const int model = 1;
    // Cast void* to int64_t
    // We need to cast to int64_t and then pass the address rather than casting to uint64_t* I think
#ifdef VERBOSE
	std::cout << "CHECKING pointers: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 1\n";
#endif

	void* sys_vp = reinterpret_cast<void*>(sba_sysptr);
	int64_t sys_iv = (int64_t)sys_vp;
	int64_t* sba_sys_ivp = &sys_iv;
//	 int64_t* sba_sys_ivp;
  //   *sba_sys_ivp=toWord<SBA::System*>(sba_sysptr);
#ifdef VERBOSE
	std::cout << "\n CASTING Tile pointer\n";
#endif
	void* tile_vp = reinterpret_cast<void*>(sba_tileptr);
	int64_t tile_iv = (int64_t)tile_vp;
     int64_t* sba_tile_ivp = &tile_iv;
//     *sba_tile_ivp=toWord<SBA::Tile*>(sba_tileptr);
#ifdef VERBOSE
	std::cout << "CALLING Fortran main_routine1_" << std::endl;
#endif

    // Here we call the actual Fortran function
    main_routine1_(sba_sys_ivp,sba_tile_ivp,&model);
#ifdef VERBOSE
	std::cout << "LEAVING run_model1" << std::endl;
#endif

    return 1; // purely for GPRM compatibility!
}


int64_t GMCF::run_model2(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t model_id) {
#ifdef VERBOSE
	std::cout << "INSIDE run_model2" << std::endl;
#endif

	const int model =2;
#ifdef VERBOSE
	std::cout << "CHECKING pointers: .tdc file name:" << sba_sysptr->task_description << "; Tile address: "<< sba_tileptr->address << std::endl;
#endif

#ifdef VERBOSE
	std::cout << "\n CASTING System pointer in model 2\n";
#endif

	void* sys_vp = reinterpret_cast<void*>(sba_sysptr);
	int64_t sys_iv = (int64_t)sys_vp;
	int64_t* sba_sys_ivp = &sys_iv;
//	 int64_t* sba_sys_ivp;
  //   *sba_sys_ivp=toWord<SBA::System*>(sba_sysptr);
#ifdef VERBOSE
	std::cout << "\n CASTING Tile pointer\n";
#endif
	void* tile_vp = reinterpret_cast<void*>(sba_tileptr);
	int64_t tile_iv = (int64_t)tile_vp;
     int64_t* sba_tile_ivp = &tile_iv;
//     *sba_tile_ivp=toWord<SBA::Tile*>(sba_tileptr);

#ifdef VERBOSE
	std::cout << "CALLING Fortran main_routine2_" << std::endl;
#endif

    // Here we call the actual Fortran function
    main_routine2_(sba_sys_ivp,sba_tile_ivp,&model);

#ifdef VERBOSE
	std::cout << "LEAVING run_model2" << std::endl;
#endif
    return 2; // purely for GPRM compatibility!
}


// This is the combined kernel (super-kernel)
/*

velnw__bondv1_init_uvw:
PARALLEL AUTO FULL RANGE. Same for CPU and GPU
	oclGlobalRange=(ip+1)*jp*kp
	oclLocalRange=0

bondv1_calc_uout: REDUCTION. Note that the GPU version is equivalent in performance but easier to code, only doesn't work on CPU
GPU_KERNEL: 1 thread per compute unit
	oclGlobalRange = jp
	oclLocalRange = jp
CPU_KERNEL: NTH threads per compute unit
	oclGlobalRange = jp*NTH
	oclLocalRange = NTH

bondv1_calc_uvw
PARALLEL AUTO "BOUNDARY RANGE". Same for CPU and GPU
	oclGlobalRange=(kp*jp)+(kp+2)*(ip+2)+(ip+3)*(jp+3)
	oclLocalRange=0

velfg__feedbf__les_calc_sm:
PARALLEL AUTO FULL RANGE. Same for CPU and GPU
	oclGlobalRange=ip*jp*kp
	oclLocalRange=0

les_bound_sm:
GPU_KERNEL: HYBRID BOUNDARY SCHEME. This does not work at all on CPU because of the "irregular" values of the ranges.
	max_range = max(ip+3,jp+3,kp+2)
	oclGlobalRange = max_range*max_range
	oclLocalRange = max_range
CPU_KERNEL: PARALLEL AUTO "BOUNDARY RANGE"
	oclGlobalRange = (jp+3)*(kp+2) + (kp+2)*(ip+2) + (jp+3)*(ip+2)
	oclLocalRange = 0

les_calc_visc__adam:
PARALLEL AUTO FULL RANGE. Same for CPU and GPU
	oclGlobalRange=ip*jp*kp
	oclLocalRange=0

press_rhsav: REDUCTION. Note that the GPU version is equivalent in performance but easier to code, only doesn't work on CPU
GPU_KERNEL
	oclGlobalRange=ip*kp
	oclLocalRange=ip
CPU_KERNEL
	oclGlobalRange = NTH*kp
	oclLocalRange = NTH

press_sor: REDUCTION+ITERATION

press_pav: REDUCTION. Note that the GPU version is equivalent in performance but easier to code, only doesn't work on CPU
GPU_KERNEL
	oclGlobalRange = ip*kp
	oclLocalRange = ip
CPU_KERNEL
	oclGlobalRange = NTH*kp
	oclLocalRange = NTH

press_adj: PARALLEL AUTO FULL RANGE. Same for CPU and GPU
	oclGlobalRange=ip*jp*kp
	oclLocalRange=0

press_boundp:
GPU_KERNEL: HYBRID BOUNDARY SCHEME. This does not work at all on CPU because of the "irregular" values of the ranges.
	max_range = max((ip+2),(jp+2),(kp+2))
	oclGlobalRange = max_range*max_range
	oclLocalRange = max_range
CPU_KERNEL: PARALLEL AUTO "BOUNDARY RANGE"
	oclGlobalRange = (jm+2)*(km+2) + (km+2)*(im+2) + (jm+2)*(im+2)
	oclLocalRange = 0

 */

#define COMBINED_KERNEL

#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

// WV: TODO: I suppose I could use an enum
// Also, this could go into a header file

#define ST_INIT 0
#define ST_VELNW__BONDV1_INIT_UVW 1
#define ST_BONDV1_CALC_UOUT 2
#define ST_BONDV1_CALC_UVW 3
#define ST_VELFG 31
#define ST_VELFG__FEEDBF__LES_CALC_SM 4
#define ST_FEEDBF__LES_CALC_SM 32
#define ST_LES_BOUND_SM 5
#define ST_LES_CALC_VISC__ADAM 6
#define ST_PRESS_RHSAV 7
#define ST_PRESS_SOR 8
#define ST_PRESS_PAV 9
#define ST_PRESS_ADJ 10
#define ST_PRESS_BOUNDP 11

// TODO: considering that im,jm,km are always identical to ip,jp,kp which are constants,
// I could simply define the constants using macros and remove these pesky arguments

// module_velnw__bondv1_init_uvw_ocl -> TODO: maybe merge with bondv1?
#include "velnw__bondv1_init_uvw_kernel.h" // CPU OK

// module_bondv1
#include "bondv1_calc_uout_kernel.h" // CPU OK
#include "bondv1_calc_uvw_kernel.h" // CPU OK
//#include "velfg_kernel.h" // CPU OK

// module_les_ocl
//#include "feedbf__les_calc_sm_kernel.h" // CPU OK
#include "velfg__feedbf__les_calc_sm_kernel.h" // CPU OK
#include "les_bound_sm_kernel.h" // CPU NEW OK
#include "les_calc_visc__adam_kernel.h" // CPU OK

// module_press_ocl
#include "press_rhsav_kernel.h" // CPU NEW OK but needs to be tested to be sure
#include "press_sor_kernel_new_2014-08-17.h" // TODO CPU!!!
#include "press_pav_kernel.h" // CPU NEW OK, but barrier HACK!
#include "press_adj_kernel.h" // CPU OK but needs to be tested to be sure
#include "press_boundp_kernel.h" // CPU NEW OK but needs to be tested to be sure

// --------------------------------------------------------------------------------
// The actual kernel
// --------------------------------------------------------------------------------

__kernel void LES_combined_kernel (
//		__global float* p,
		__global float2* p2,
		__global float4* uvw,
		__global float4* uvwsum,
		__global float4* fgh,
		__global float4* fgh_old,
		__global float* rhs,
		const __global float4* mask1,
		__global float16* diu,
		__global float* sm,
		const __global float* dxs,
		const __global float* dys,
		const __global float* dzs,
		const __global float* dx1,
		const __global float* dy1,
		const __global float* dzn,
#ifndef EXTERNAL_WIND_PROFILE
		const __global float* z2,
#else
		const __global float * wind_profile,
#endif

		const __global float* cn1,
		const __global float* cn2l,
		const __global float* cn2s,
		const __global float* cn3l,
		const __global float* cn3s,
		const __global float* cn4l,
		const __global float* cn4s,
		__global float* val_ptr, // we use this for any transfer of scalar values
		__global float* chunks_num,
		__global float* chunks_denom,
		__global unsigned int* n_ptr, // also used for nrd
		__global unsigned int* state_ptr,
		const float dt,
		const unsigned int im,
		const unsigned int jm,
		const unsigned int km

        ) {
	unsigned int gl_id = get_global_id(0);
	unsigned int gr_id = get_group_id(0);
	unsigned int l_id =  get_local_id(0);

#if KERNEL == CPU_KERNEL
	__local float loc_th_1[NTH];
	__local float loc_th_2[NTH];
#elif	KERNEL == GPU_KERNEL
	__local float loc_th_1[N_ELTS_TH];
	__local float loc_th_2[N_ELTS_TH];
#endif
    int n = *n_ptr;
    int state = *state_ptr;
    switch (state) {
        case ST_INIT:
            {
            	if (gl_id==0) {
                // do nothing
            	}
                break;
            }
        case ST_VELNW__BONDV1_INIT_UVW:
            {
            	velnw__bondv1_init_uvw_kernel(p2, uvw, fgh, dxs, dys, dzs, dzn,
#ifndef EXTERNAL_WIND_PROFILE
            			z2,
#else
            			wind_profile,
#endif

            			n_ptr, im, jm, km,dt);
                break;
            }
        case ST_BONDV1_CALC_UOUT:
            {
                bondv1_calc_uout_kernel(
                		loc_th_1, loc_th_2,
                		uvw,
                		val_ptr,
                		chunks_num,
                		chunks_denom,
                		im, jm, km
                );
                break;
            }

        case ST_BONDV1_CALC_UVW:
            {
            	bondv1_calc_uvw_kernel(
            			uvw,uvwsum,fgh,mask1,diu,dxs,dzs,dx1,dy1,dzn,sm,val_ptr,dt,im,jm,km
            	);
                break;
            }

        case ST_VELFG__FEEDBF__LES_CALC_SM:
            {
            	velfg__feedbf__les_calc_sm_kernel(
            			uvw,uvwsum,fgh,mask1,diu,dxs,dzs,dx1,dy1,dzn,sm,val_ptr,dt,im,jm,km
            	);
                break;
            }

//        case ST_VELFG:
//            {
//            	velfg_kernel(
//            			uvw,fgh,diu,dzs,dx1,dy1,dzn,val_ptr,im,jm,km
//            	);
//                break;
//            }
//
//        case ST_FEEDBF__LES_CALC_SM:
//            {
//            	feedbf__les_calc_sm_kernel(
//            			uvw,uvwsum,fgh,mask1,diu,dxs,dzs,dx1,dy1,dzn,sm,val_ptr,dt,im,jm,km
//            	);
//                break;
//            }

        case ST_LES_BOUND_SM:
            {
                les_bound_sm_kernel(fgh,  dx1,dy1,dzn,diu, sm, im, jm, km);
                break;
            }

        case ST_LES_CALC_VISC__ADAM:
            {
                les_calc_visc__adam_kernel (
                		fgh,
                		fgh_old,
                		dx1,dy1,dzn,
                		diu,
                		sm,
                		im,
                		jm,
                		km
                );
                break;
            }

        case ST_PRESS_RHSAV:
            {
                press_rhsav_kernel(
                		loc_th_1, loc_th_2,
				uvw, fgh, rhs, dx1,dy1,dzn,
		      chunks_num, chunks_denom,
		      dt,im, jm, km
		      );
            break;
            }
        case ST_PRESS_SOR:
            {

                press_sor_kernel(
                		loc_th_1,
//                		loc_th_2,
                		uvw,
//                		p,
                		p2,
                		rhs, cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s,
                		chunks_num, chunks_denom, val_ptr, n_ptr,
                		im, jm, km
                );

                break;
            }

        case ST_PRESS_PAV:
            {
                press_pav_kernel(loc_th_1, loc_th_2,p2,dx1,dy1,dzn,chunks_num, chunks_denom, im, jm, km );
                break;
            }

        case ST_PRESS_ADJ:
            {
                press_adj_kernel(p2, val_ptr, im, jm, km );
                break;
            }
        case ST_PRESS_BOUNDP:
            {
                press_boundp_kernel(p2, im, jm, km);
                break;
            }
        default:    
            n=1;
            // do nothing
    };

} // END of LES_kernel

// ====================================================== SUBKERNEL DEFINITIONS ===========================================================
#include "velnw__bondv1_init_uvw_kernel.cl"
#include "bondv1_calc_uout_kernel.cl"
#include "bondv1_calc_uvw_kernel.cl"
//#include "velfg_kernel.cl"
//#include "feedbf__les_calc_sm_kernel.cl"
#include "velfg__feedbf__les_calc_sm_kernel.cl"
#include "les_bound_sm_kernel.cl"
#include "les_calc_visc__adam_kernel.cl"
#include "press_rhsav_kernel.cl"
#include "press_sor_kernel_new_2014-08-17.cl"
#include "press_pav_kernel.cl"
#include "press_adj_kernel.cl"
#include "press_boundp_kernel.cl"

// ========= END OF SUBKERNELS ======

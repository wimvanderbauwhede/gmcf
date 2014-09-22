// This is the combined kernel (super-kernel)
/*
 * TODO: new BONDV1 split/merge + update CPU kernels
 *
velnw_bondv1_calc_uvw: PARALLEL AUTO full range

bondv1_calc_uout: REDUCTION

merged_bondv1_calc_uvw_velfg_feedbf_les_calc_sm: PARALLEL AUTO full range + boundary range

les_bound_sm: PARALLEL GROUPS/THREADS -> could this become boundary range?

merged_les_calc_visc_adam: PARALLEL AUTO

press_rhsav: REDUCTION

press_sor: REDUCTION+ITERATION

press_pav: REDUCTION

press_adj: PARALLEL AUTO

press_boundp: PARALLEL GROUPS/THREADS -> could this become boundary range?
 */

#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

#define ICAL 0
//#define IFBF 0
#define N_ELTS_TH 256

// Specifically for the SOR kernel:
//#define APPROX_PAR_SOR
//#define CALCULATED_BOUNDS
//#define P_SCRATCH
//#define OMEGA_NOT_1

// Physical property set
#define RO 1.1763
#define VN 1.583E-5
// IBM parameter set (Feedback force by Goldstein)
#define ALPHA -10.0
#define BETA -1.0

// WV: I suppose I could use an enum
// Also, this could go into a header file

#define ST_INIT 0
#define ST_VELNW__BONDV1_CALC_UVW 1
#define ST_BONDV1_CALC_UOUT 2
#define ST_BONDV1_CALC_UVW__VELFG__FEEDBF__LES_CALC_SM 3
#define ST_LES_BOUND_SM 4
#define ST_LES_CALC_VISC__ADAM 5
#define ST_PRESS_RHSAV 6
#define ST_PRESS_SOR 7
#define ST_PRESS_PAV 8
#define ST_PRESS_ADJ 9
#define ST_PRESS_BOUNDP 10

// I think it would be best to factor out all the functions, and define them in header files.
// All I need to do is define a macro __SUBKERNEL whicj will be in shared_macros
// but will be redefined in the combined kernel source.

// TODO: considering that im,jm,km are always identical to ip,jp,kp which are constants,
// I could simply define the constants using macros and remove these pesky arguments

#include "velnw__bondv1_calc_uvw_kernel.h"
#include "bondv1_calc_uout_kernel.h"
#include "bondv1_calc_uvw__velfg__feedbf__les_calc_sm_kernel.h"
#include "les_bound_sm_kernel.h"
#include "les_calc_visc__adam_kernel.h"
#include "press_rhsav_kernel.h"
#include "press_sor_kernel.h"
#include "press_pav_kernel.h"
#include "press_adj_kernel.h"
#include "press_boundp_kernel.h"

// --------------------------------------------------------------------------------
// The actual kernel
// --------------------------------------------------------------------------------

__kernel void LES_combined_kernel (
		__global float* p,
		__global float* p_scratch, // only used if P_SCRATCH is defined
		__global float4* uvw,
		__global float4* uvwsum,
		__global float4* fgh,
		__global float4* fgh_old,
		__global float* rhs,
		__global float4* mask1,
		__global float16* diu,
		__global float* sm,
		__global float* dxs,
		__global float* dys,
		__global float* dzs,
		__global float* dx1,
		__global float* dy1,
		__global float* dzn,
		__global float* z2,
		__global float* cn1,
		__global float* cn2l,
		__global float* cn2s,
		__global float* cn3l,
		__global float* cn3s,
		__global float* cn4l,
		__global float* cn4s,
		__global float* val_ptr, // we use this for any transfer of scalar values
		__global float* chunks_num,
		__global float* chunks_denom,
		__global unsigned int* n_ptr, // also used for nrd
		__global unsigned int* state_ptr,
		const float dt,
//		const unsigned int nmax, // this is actually unused
		const unsigned int im,
		const unsigned int jm,
		const unsigned int km

        ) {
    unsigned int n = *n_ptr;
    unsigned int state = *state_ptr;


    switch (state) {
        case ST_INIT: 
            {
                // do nothing
                n=0;
                break;
            }

        case ST_VELNW__BONDV1_CALC_UVW:
            {
            	velnw_bondv1_init_uvw_kernel(p, uvw, fgh, dxs, dys, dzs, dzn, z2, n_ptr, im, jm, km,dt);
                break;
            }
        case ST_BONDV1_CALC_UOUT:
            {
                bondv1_calc_uout_kernel(
                		uvw, dxs, dzn, z2,
#if ICAL == 0
                		n_ptr,
#endif
                		im, jm, km, dt
                );
                break;
            }

        case ST_BONDV1_CALC_UVW__VELFG__FEEDBF__LES_CALC_SM:
            {
            	bondv1_calc_uvw__velfg__feedbf__les_calc_sm_kernel(
            			uvw,uvwsum,fgh,mask1,diu,dzs,dx1,dy1,dzn,sm,dt,im,jm,km
            	);
                break;
            }

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
				uvw, fgh, rhs, dx1,dy1,dzn,
		      chunks_num, chunks_denom,
		      dt,im, jm, km
		      );
                break;
            }

        case ST_PRESS_SOR:
            {

                press_sor_kernel(
                		uvw, p,
#ifdef P_SCRATCH
                		p_scratch,
#endif
                		rhs, cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s,
                		chunks_num, chunks_denom, val_ptr, n_ptr,
                		im, jm, km
                );

                break;
            }

        case ST_PRESS_PAV:
            {
                press_pav_kernel(p,dx1,dy1,dzn,chunks_num, chunks_denom, im, jm, km );
                break;
            }

        case ST_PRESS_ADJ:
            {
                press_adj_kernel(p, val_ptr, im, jm, km );
                break;
            }
        case ST_PRESS_BOUNDP:
            {
                press_boundp_kernel(p, im, jm, km);
                break;
            }

        default:    
            n=1;
            // do nothing
    };


} // END of LES_kernel

// ====================================================== SUBKERNEL DEFINITIONS ===========================================================
#include "velnw__bondv1_calc_uvw_kernel.cl"
#include "bondv1_calc_uout_kernel.cl"
#include "bondv1_calc_uvw__velfg__feedbf__les_calc_sm_kernel.cl"
#include "les_bound_sm_kernel.cl"
#include "les_calc_visc__adam_kernel.cl"
#include "press_rhsav_kernel.cl"
#include "press_sor_kernel.cl"
#include "press_pav_kernel.cl"
#include "press_adj_kernel.cl"
#include "press_boundp_kernel.cl"

// ========= END OF SUBKERNELS ======




#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

/*
KERNEL = GPU_KERNEL is correct on GPU if none of the options below is switched on:

 CALCULATED_BOUNDS calculates the bounds together with the other points, rather than copying.
 This is faster but leads to races in red-black, with some loss of precision.
 P_SCRATCH uses a scratch pad for p rather than red-black. I think it is correct but the results are different. It only supports calculated bounds.
 APPROX_PAR_SOR is super fast, esp. when combined with P_SCRATCH but lets the SOR converge per i-j plane, rather than over the full cube.
 Without P_SCRATCH, it might give the best trade-off between speed and precision. But I think P_SCRATCH is correct.
*/

#define FIXED_NTH
//#define NEW_SOR
//#define APPROX_PAR_SOR
//#define CALCULATED_BOUNDS
//#define P_SCRATCH

#ifndef COMBINED_KERNEL
void boundp1c_ (__global float *p,int im,int jm,int km) ;
void boundp2c_ (__global float *p,int im,int jm,int km) ;
#endif
// ====================================================== KERNEL ===========================================================

__SUBKERNEL void press_sor_kernel (
#ifdef COMBINED_KERNEL
__local float* sor_chunks,
__local float* tmp_chunks,
#endif

		__global float4* uvw,
        __global float* p,
//#ifdef P_SCRATCH
        __global float* p_scratch,
//#endif
        __global float *rhs,
		__global float *cn1,__global float *cn2l,__global float *cn2s,__global float *cn3l,__global float *cn3s,__global float *cn4l,__global float *cn4s,
		__global float *chunks_num,
		__global float *chunks_denom,
		__global float *val_ptr,
		__global unsigned int *nrd_ptr,
		  const unsigned int im,
		  const unsigned int jm,
		  const unsigned int km
        ) {
    	const unsigned int ip=im;
        const unsigned int jp=jm;
        const unsigned int kp=km;
#if 0
#if KERNEL == ORIG_KERNEL
	//! --only used mac method
	const float pjuge=0.0001F;
	const int nmaxp=50;
#ifdef OMEGA_NOT_1
	const float omega=1.0F;
#endif

	float rhsav = *val_ptr;//rhs[0,0,0];
	float sor = 0.0F;
	if (get_global_id(0)==0) {
//! --SOR
	for (unsigned int iter=1;iter<=nmaxp;iter++) {
		sor = 0.0F;
		for (unsigned int nrd=0;nrd<=1;nrd++) {
			for (unsigned int k=1;k<=km;k++) {
				for (unsigned int j=1;j<=jm;j++) {
					for (unsigned int i=1 + (k + j + nrd) % (2);i<=im;i+=2) {
#ifdef OMEGA_NOT_1
						float reltmp_mp = omega * (cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] + rhsav
								) - p[FTNREF3D0(i,j,k,ip+3,jp+3)]);
						// reltmp_mp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for  omega==1, the p_ijk cancels out!
						p[FTNREF3D0(i,j,k,ip+3,jp+3)] += reltmp_mp;
#else
						float reltmp = cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] + rhsav
								);
						float reltmp_mp = reltmp - p[FTNREF3D0(i,j,k,ip+3,jp+3)];
						// reltmp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for the current value of omega, the p_ijk cancels out!
						p[FTNREF3D0(i,j,k,ip+3,jp+3)] = reltmp;
#endif
						sor += reltmp_mp * reltmp_mp;
					} // i
				} // j
			} // k
			boundp1c_(p,im,jm,km );
		} // nrd
		boundp2c_( p,im,jm,km );
		if ( sor < pjuge ) {
			break;
		}
	} // iter
	ksor[0]=sor;
	} // if global id == 0
#elif KERNEL == CPU_KERNEL // next kernel
#ifndef NEW_SOR
// Current CPU kernel has no parallelism!
//#define OMEGA_NOT_1
#ifdef OMEGA_NOT_1
	const float omega=1.0F;
#endif

if (get_global_id(0)==0) {
	unsigned int nrd = *nrd_ptr;
//! --SOR
		float sor =chunks_num[0];
		if (nrd<2) {
			float rhsav = rhs[0,0,0];
			float p_sum_nobounds = 0.0F;
			for (unsigned int k=1;k<=km;k++) {
				for (unsigned int j=1;j<=jm;j++) {
					for (unsigned int i=1 + (k + j + nrd) % (2);i<=im;i+=2) {
#ifdef OMEGA_NOT_1
						float reltmp_mp = omega * (cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								) - p[FTNREF3D0(i,j,k,ip+3,jp+3)]);
						// reltmp_mp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for  omega==1, the p_ijk cancels out!
						p[FTNREF3D0(i,j,k,ip+3,jp+3)] += reltmp_mp;
						p_sum_nobounds += p[FTNREF3D0(i,j,k,ip+3,jp+3)];
#else
						float reltmp = cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								);
						float reltmp_mp = reltmp - p[FTNREF3D0(i,j,k,ip+3,jp+3)];
						// reltmp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for the current value of omega, the p_ijk cancels out!
						p[FTNREF3D0(i,j,k,ip+3,jp+3)] = reltmp;
						p_sum_nobounds += reltmp;
#endif // OMEGA
						sor += reltmp_mp * reltmp_mp;
					} // i
				} // j
			} // k
			boundp1c_(p,im,jm,km );
//			chunks_num[0]+=sor;
			chunks_denom[0]+=p_sum_nobounds;
		} else { // nrd == 2
			boundp2c_( p,im,jm,km );
		}
		chunks_num[0]=sor;
} // global_id == 0
#else // NEW_SOR

#endif
#else // KERNEL == GPU_KERNEL
// ------------------------------------------------------------------------------------------------------------
#ifndef APPROX_PAR_SOR // ------------------------------------------------------------------------------------------------------------
#ifdef OMEGA_NOT_1
	const float omega=1.0F;
#endif
#ifndef COMBINED_KERNEL
#ifndef FIXED_NTH
    __local float sor_chunks[N_ELTS_TH];
    __local float tmp_chunks[N_ELTS_TH];
#else
    __local float sor_chunks[NTH];
    __local float tmp_chunks[NTH];
#endif
#endif
    int g_id = get_global_id(0);
    unsigned int grp_id = get_group_id(0);
    unsigned int l_id = get_local_id(0);

    unsigned int nrd = *nrd_ptr;

        float p_sum_nobounds = 0.0F;
        float rhsav = rhs[0,0,0];
#ifndef CALCULATED_BOUNDS
        // k = 1 .. km (range = km)
        unsigned int k = grp_id+1;
        if (nrd<2) {
        	float local_sor = 0.0F;
#ifndef FIXED_NTH
        // j == 1 .. jm (range = jm)
        unsigned int j = l_id+1;
#else
        unsigned int lj_bound = ( jm % NTH == 0) ? jm/NTH : jm/NTH+1;
        for (unsigned int lj=0;lj<lj_bound;lj++ ) {
        	unsigned int j = 1 +lj*NTH+l_id;
        	if (j<jm+1) {
#endif
        unsigned int j_lhs = j;
        unsigned int k_lhs = k;
#else // CALCULATED_BOUNDS
        // k = 0 .. km+1
        unsigned int k = grp_id;//+1;
        // j == 0 .. jm+1
        unsigned int j = l_id;//+1;
        // What we do is separate the lhs index from the rhs index
        // then we re-assign the rhs index to meet the boundary condition
        unsigned int j_lhs = j;
        unsigned int k_lhs = k;

        if (k==0) {
        	k=1;
        }
        if (k==km+1) {
        	k=km;
        }
        if (j==0) {
        	j = jm;
        }
        if (j==jm+1) {
        	j = 1;
        }
#endif


#ifndef P_SCRATCH
    	for (unsigned int i=1 + ((k + j + nrd) % 2);i<=im;i+=2) {

#ifdef OMEGA_NOT_1
						float reltmp_mp = omega * (cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								) - p[FTNREF3D0(i,j,k,ip+3,jp+3)]);
						// reltmp_mp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for  omega==1, the p_ijk cancels out!
						p[FTNREF3D0(i,j_lhs,k_lhs,ip+3,jp+3)] += reltmp_mp;
#else
						float reltmp = cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								);
						float reltmp_mp = reltmp - p[FTNREF3D0(i,j,k,ip+3,jp+3)];
						// reltmp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for the current value of omega, the p_ijk cancels out!
						p[FTNREF3D0(i,j_lhs,k_lhs,ip+3,jp+3)] = reltmp;
#endif
						local_sor += reltmp_mp * reltmp_mp;
						p_sum_nobounds += reltmp;
    	} // loop over i
#ifndef CALCULATED_BOUNDS
        //boundp1
        //--computational boundary(neumann condition)
        // adjust i=0 and i=im+1 for all j and k
        p[FTNREF3D0(0,j_lhs,k_lhs,ip+3,jp+3)] = p[FTNREF3D0(1,j_lhs,k_lhs,ip+3,jp+3)];
        p[FTNREF3D0(im + 1,j_lhs,k_lhs,ip+3,jp+3)] = p[FTNREF3D0(im,j_lhs,k_lhs,ip+3,jp+3)];
#endif

#else // P_SCRATCH
    	for (unsigned int i_lhs=0;i_lhs<im+2;i_lhs++) {
    		unsigned int i = i_lhs;
//    		unsigned int null_bound=1; // this is a way to skip the contributions of the i-boundaries
    		if (i_lhs==0) {
    			i=1;
//    			null_bound=0;
    		}
    		if (i_lhs==im+1) {
    			i=im;
//    			null_bound=0;
    		}
#ifdef OMEGA_NOT_1
						float reltmp_mp = omega * (cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								) - p[FTNREF3D0(i,j,k,ip+3,jp+3)]);
						// reltmp_mp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for  omega==1, the p_ijk cancels out!
						p[FTNREF3D0(i,j_lhs,k_lhs,ip+3,jp+3)] += reltmp_mp;
#else
						float reltmp_mp = 0.0;
						if (nrd == 0) {
						 float reltmp = cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								);
							 reltmp_mp = reltmp -  p[FTNREF3D0(i,j,k,ip+3,jp+3)];
							p_scratch[FTNREF3D0(i_lhs,j_lhs,k_lhs,ip+3,jp+3)] = reltmp;
						} else {
							float reltmp = cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
								  cn2l[FTNREF1D(i,1)] * p_scratch[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p_scratch[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p_scratch[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p_scratch[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p_scratch[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p_scratch[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								);
								reltmp_mp = reltmp -  p_scratch[FTNREF3D0(i,j,k,ip+3,jp+3)];
							p[FTNREF3D0(i_lhs,j_lhs,k_lhs,ip+3,jp+3)] = reltmp;
						}
#endif // OMEGA
						local_sor +=  reltmp_mp * reltmp_mp;
    	}
#endif // P_SCRATCH
#ifdef FIXED_NTH
        } //if j<jm
        	} // for jl
#endif
        	sor_chunks[l_id] = local_sor;
        	tmp_chunks[l_id] = p_sum_nobounds;
#if NTH > 32
            barrier(CLK_LOCAL_MEM_FENCE);
#endif
            if (l_id==0) {

#ifndef CALCULATED_BOUNDS
            // boundary conditions for j=0 and j = jm+1, for all i and k
            		for (unsigned int i = 0; i < im+2; i++) {
            			unsigned int ii = i;//l_id;
            			p[FTNREF3D0(ii,jm+1,k,ip+3,jp+3)] = p[FTNREF3D0(ii,1,k,ip+3,jp+3)];
            			p[FTNREF3D0(ii,0,k,ip+3,jp+3)] = p[FTNREF3D0(ii,jm,k,ip+3,jp+3)];
            		}
#endif
            	float local_sor_acc = chunks_num[grp_id];
                p_sum_nobounds=chunks_denom[grp_id];
            	// we skip the two outer chunks as they are for the boundary conditions
#ifndef FIXED_NTH
#ifdef CALCULATED_BOUNDS
                for(unsigned int s = 1; s < jm+1; s++)
#else
                for(unsigned int s = 0; s < jm; s++)
#endif
#else
                for(unsigned int s = 0; s < NTH; s++)
#endif
                {
                	local_sor_acc += sor_chunks[s];
                    p_sum_nobounds += tmp_chunks[s];
                }
                chunks_num[grp_id] = local_sor_acc;
                chunks_denom[grp_id] = p_sum_nobounds;
            }

#ifndef CALCULATED_BOUNDS
    }    else { // nrd==2
        //boundp2
        //--computational boundary(neumann condition)
    	unsigned int i = grp_id;
#ifndef FIXED_NTH
        // j == 0 .. jm+1
        unsigned int j = l_id;
#else
        unsigned int lj_bound = ((jm+2) % NTH == 0) ? (jm+2)/NTH : (jm+2)/NTH+1;
        for (unsigned int lj=0;lj<lj_bound;lj++ ) {
        	unsigned int j = lj*NTH+l_id;
        	if (j<jm+2) {
#endif

        p[FTNREF3D0(i,j,0,ip+3,jp+3)] = p[FTNREF3D0(i,j,1,ip+3,jp+3)];
        p[FTNREF3D0(i,j,km+1,ip+3,jp+3)] = p[FTNREF3D0(i,j,km,ip+3,jp+3)];
#ifdef FIXED_NTH
        	}
        }
#endif
    }
#endif

// ------------------------------------------------------------------------------------------------------------
#else // APPROX_PAR_SOR // ------------------------------------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------
		const float pjuge=0.0001F;
		const int nmaxp=50;
#ifdef OMEGA_NOT_1
		const float omega=1.0F;
#endif // OMEGA_NOT_1

#ifndef COMBINED_KERNEL
    __local float sor_chunks[N_ELTS_TH];
    __local float tmp_chunks[N_ELTS_TH];
#endif
    unsigned int grp_id = get_group_id(0);
    unsigned int l_id = get_local_id(0);

	float rhsav = rhs[0,0,0];
	float local_sor_acc = 0.0F;
//	ksor[0]=nmaxp;
	chunks_denom[grp_id]=nmaxp;
	for (unsigned int iter=1;iter<=nmaxp;iter++) {
#ifndef CALCULATED_BOUNDS
		for (unsigned int nrd=0;nrd<=2;nrd++)
#else
		for (unsigned int nrd=0;nrd<=1;nrd++)
#endif // CALCULATED_BOUNDS
		{
//			float p_sum_nobounds = 0.0F;
			float local_sor = 0.0F;
#ifndef CALCULATED_BOUNDS
        // k = 1 .. km
        unsigned int k = grp_id+1;
        // j == 1 .. jm
        unsigned int j = l_id+1;
        unsigned int j_lhs = j;
        unsigned int k_lhs = k;

        if (nrd<2) {

#else
        // k = 0 .. km+1
        unsigned int k = grp_id;//+1;
        // j == 0 .. jm+1
        unsigned int j = l_id;//+1;
        // What we do is separate the lhs index from the rhs index
        // then we re-assign the rhs index to meet the boundary condition
        unsigned int j_lhs = j;
        unsigned int k_lhs = k;

        if (k==0) {
        	k=1;
        }
        if (k==km+1) {
        	k=km;
        }
        if (j==0) {
        	j = jm;
        }
        if (j==jm+1) {
        	j = 1;
        }
#endif // CALCULATED_BOUNDS

#ifndef P_SCRATCH
    	for (unsigned int i=1 + ((k + j + nrd) % 2);i<=im;i+=2) {

#ifdef OMEGA_NOT_1
						float reltmp_mp = omega * (cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								) - p[FTNREF3D0(i,j,k,ip+3,jp+3)]);
						// reltmp_mp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for  omega==1, the p_ijk cancels out!
						p[FTNREF3D0(i,j_lhs,k_lhs,ip+3,jp+3)] += reltmp_mp;
#else
						float reltmp = cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								);
						float reltmp_mp = reltmp - p[FTNREF3D0(i,j,k,ip+3,jp+3)];
						// reltmp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for the current value of omega, the p_ijk cancels out!
						p[FTNREF3D0(i,j_lhs,k_lhs,ip+3,jp+3)] = reltmp;
#endif // OMEGA
						local_sor += reltmp_mp * reltmp_mp;
//						p_sum_nobounds += reltmp;

    	} // loop over i

#ifndef CALCULATED_BOUNDS
        //boundp1
        //--computational boundary(neumann condition)
        // adjust i=0 and i=im+1 for all j and k
        p[FTNREF3D0(0,j_lhs,k_lhs,ip+3,jp+3)] = p[FTNREF3D0(1,j_lhs,k_lhs,ip+3,jp+3)];
        p[FTNREF3D0(im + 1,j_lhs,k_lhs,ip+3,jp+3)] = p[FTNREF3D0(im,j_lhs,k_lhs,ip+3,jp+3)];
#endif // CALCULATED_BOUNDS

#else // P_SCRATCH
    	for (unsigned int i_lhs=0;i_lhs<im+2;i_lhs++) {
    		unsigned int i = i_lhs;
//    		unsigned int null_bound=1; // this is a way to skip the contributions of the i-boundaries
    		if (i_lhs==0) {
    			i=1;
//    			null_bound=0;
    		}
    		if (i_lhs==im+1) {
    			i=im;
//    			null_bound=0;
    		}
#ifdef OMEGA_NOT_1
						float reltmp_mp = omega * (cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								) - p[FTNREF3D0(i,j,k,ip+3,jp+3)]);
						// reltmp_mp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for  omega==1, the p_ijk cancels out!
						p[FTNREF3D0(i,j_lhs,k_lhs,ip+3,jp+3)] += reltmp_mp;
#else
						float reltmp_mp = 0.0;
						if (nrd == 0) {
						 float reltmp = cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								);
							 reltmp_mp = reltmp -  p[FTNREF3D0(i,j,k,ip+3,jp+3)];
							p_scratch[FTNREF3D0(i_lhs,j_lhs,k_lhs,ip+3,jp+3)] = reltmp;
						} else {
							float reltmp = cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
								  cn2l[FTNREF1D(i,1)] * p_scratch[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p_scratch[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p_scratch[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p_scratch[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p_scratch[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p_scratch[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								);
								reltmp_mp = reltmp -  p_scratch[FTNREF3D0(i,j,k,ip+3,jp+3)];
							p[FTNREF3D0(i_lhs,j_lhs,k_lhs,ip+3,jp+3)] = reltmp;
						}
#endif // OMEGA
						local_sor +=  reltmp_mp * reltmp_mp;
    	}
#endif // P_SCRATCH


        	sor_chunks[l_id] = local_sor;
            barrier(CLK_LOCAL_MEM_FENCE);



#ifndef CALCULATED_BOUNDS
            if (l_id==0) {
            // boundary conditions for j=0 and j = jm+1, for all i and k
            	// problem is of course that values for k won't necessarily be synced
				for (unsigned int i = 0; i < im+2; i++) {
					unsigned int ii = i;
					p[FTNREF3D0(ii,jm+1,k,ip+3,jp+3)] = p[FTNREF3D0(ii,1,k,ip+3,jp+3)];
					p[FTNREF3D0(ii,0,k,ip+3,jp+3)] = p[FTNREF3D0(ii,jm,k,ip+3,jp+3)];
				}
            }
#endif //CALCULATED_BOUNDS
            	local_sor_acc = chunks_num[grp_id];//0.0F;
            	// we skip the two outer chunks as they are for the boundary conditions
#ifndef CALCULATED_BOUNDS
                for(unsigned int s = 0; s < jm; s++)
#else
                for(unsigned int s = 1; s < jm+1; s++)
#endif //CALCULATED_BOUNDS
                {
                	local_sor_acc += sor_chunks[s];
                }


#ifndef CALCULATED_BOUNDS
    } else { // nrd==2
        //boundp2
        //--computational boundary(neumann condition)
    	// This is not quite right as j ranges from 1 to jm
    	for (unsigned int i = 0;i<im+2;i++) {
//    	unsigned int j = l_id;
        p[FTNREF3D0(i,j,0,ip+3,jp+3)] = p[FTNREF3D0(i,j,1,ip+3,jp+3)];
        p[FTNREF3D0(i,j,km+1,ip+3,jp+3)] = p[FTNREF3D0(i,j,km,ip+3,jp+3)];

        p[FTNREF3D0(i,0,0,ip+3,jp+3)] = p[FTNREF3D0(i,0,1,ip+3,jp+3)];
        p[FTNREF3D0(i,0,km+1,ip+3,jp+3)] = p[FTNREF3D0(i,0,km,ip+3,jp+3)];

        p[FTNREF3D0(i,jm+1,0,ip+3,jp+3)] = p[FTNREF3D0(i,jm+1,1,ip+3,jp+3)];
        p[FTNREF3D0(i,jm+1,km+1,ip+3,jp+3)] = p[FTNREF3D0(i,jm+1,km,ip+3,jp+3)];
    	}
    }
#endif //CALCULATED_BOUNDS
		} // nrd

		if ( local_sor_acc < pjuge ) {
			chunks_denom[grp_id]=iter;
			break;
		}
	} // iter
    chunks_num[grp_id] = local_sor_acc;

#endif // APPROX_PAR_SOR
#endif // KERNEL
#endif // 0
#ifndef COMBINED_KERNEL
    __local float sor_chunks[NTH];
    __local float tmp_chunks[NTH];
#endif
    int g_id = get_global_id(0);
    unsigned int grp_id = get_group_id(0);
    unsigned int l_id = get_local_id(0);

    unsigned int nrd = *nrd_ptr;

        float p_sum_nobounds = 0.0F;
        float rhsav = rhs[0,0,0];
        // k = 1 .. km (range = km)
        unsigned int k = grp_id+1;
        if (nrd<2) {
        	float local_sor = 0.0F;
        unsigned int lj_bound = ( jm % NTH == 0) ? jm/NTH : jm/NTH+1;
        for (unsigned int lj=0;lj<lj_bound;lj++ ) {
        	unsigned int j = 1 +lj*NTH+l_id;
        	if (j<jm+1) {
        unsigned int j_lhs = j;
        unsigned int k_lhs = k;


    	for (unsigned int i=1 + ((k + j + nrd) % 2);i<=im;i+=2) {

						float reltmp = cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
                                  cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)]
								+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)]
								+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)]
								+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)]
								+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)]
								+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)]
								- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
								);
						float reltmp_mp = reltmp - p[FTNREF3D0(i,j,k,ip+3,jp+3)];
						// reltmp = omega* (terms - p_ijk)
						// p_ijk = p_ijk + omega* (terms - p_ijk)
						// so for the current value of omega, the p_ijk cancels out!
						p[FTNREF3D0(i,j_lhs,k_lhs,ip+3,jp+3)] = reltmp;
						local_sor += reltmp_mp * reltmp_mp;
						p_sum_nobounds += reltmp;
    	} // loop over i
        //boundp1
        //--computational boundary(neumann condition)
        // adjust i=0 and i=im+1 for all j and k
        p[FTNREF3D0(0,j_lhs,k_lhs,ip+3,jp+3)] = p[FTNREF3D0(1,j_lhs,k_lhs,ip+3,jp+3)];
        p[FTNREF3D0(im + 1,j_lhs,k_lhs,ip+3,jp+3)] = p[FTNREF3D0(im,j_lhs,k_lhs,ip+3,jp+3)];

        } //if j<jm
        	} // for jl
        	sor_chunks[l_id] = local_sor;
        	tmp_chunks[l_id] = p_sum_nobounds;
#if NTH > 32
            barrier(CLK_LOCAL_MEM_FENCE);
#endif
            if (l_id==0) {

            // boundary conditions for j=0 and j = jm+1, for all i and k
            		for (unsigned int i = 0; i < im+2; i++) {
            			unsigned int ii = i;//l_id;
            			p[FTNREF3D0(ii,jm+1,k,ip+3,jp+3)] = p[FTNREF3D0(ii,1,k,ip+3,jp+3)];
            			p[FTNREF3D0(ii,0,k,ip+3,jp+3)] = p[FTNREF3D0(ii,jm,k,ip+3,jp+3)];
            		}
            	float local_sor_acc = chunks_num[grp_id];
                p_sum_nobounds=chunks_denom[grp_id];
            	// we skip the two outer chunks as they are for the boundary conditions
                for(unsigned int s = 0; s < NTH; s++)
                {
                	local_sor_acc += sor_chunks[s];
                    p_sum_nobounds += tmp_chunks[s];
                }
                chunks_num[grp_id] = local_sor_acc;
                chunks_denom[grp_id] = p_sum_nobounds;
            }

    } else { // nrd==2
        //boundp2
        //--computational boundary(neumann condition)
    	unsigned int i = grp_id;
        unsigned int lj_bound = ((jm+2) % NTH == 0) ? (jm+2)/NTH : (jm+2)/NTH+1;
        for (unsigned int lj=0;lj<lj_bound;lj++ ) {
        	unsigned int j = lj*NTH+l_id;
        	if (j<jm+2) {
        p[FTNREF3D0(i,j,0,ip+3,jp+3)] = p[FTNREF3D0(i,j,1,ip+3,jp+3)];
        p[FTNREF3D0(i,j,km+1,ip+3,jp+3)] = p[FTNREF3D0(i,j,km,ip+3,jp+3)];
        	}
        }
    }

} // END of press_sor_kernel()
// ============================================================== ==============================================================
void boundp1c_ (__global float *p,int im,int jm,int km) {

  int ip = im;
  int jp=jm;
  int kp=km;

  int k;
  int j;
  int i;
//! 
//! --computational boundary(neumann condition)
  for (k=0;k<=km + 1;k++) {
    for (j=0;j<=jm + 1;j++) {
      p[FTNREF3D0(0,j,k,ip+3,jp+3)] = p[FTNREF3D0(1,j,k,ip+3,jp+3)];
      p[FTNREF3D0(im + 1,j,k,ip+3,jp+3)] = p[FTNREF3D0(im,j,k,ip+3,jp+3)];
    }
  }
  for (k=0;k<=km + 1;k++) {
    for (i=0;i<=im + 1;i++) {
      p[FTNREF3D0(i,0,k,ip+3,jp+3)] = p[FTNREF3D0(i,jm,k,ip+3,jp+3)];
      p[FTNREF3D0(i,jm + 1,k,ip+3,jp+3)] = p[FTNREF3D0(i,1,k,ip+3,jp+3)];
    }
  }

  return;
}

void boundp2c_ (__global float *p,int im,int jm,int km) {

  int ip = im;
  int jp=jm;
  int kp=km;

  int j;
  int i;
//! 
//! --computational boundary(neumann condition)
  for (j=0;j<=jm + 1;j++) {
    for (i=0;i<=im + 1;i++) {
      p[FTNREF3D0(i,j,0,ip+3,jp+3)] = p[FTNREF3D0(i,j,1,ip+3,jp+3)];
      p[FTNREF3D0(i,j,km + 1,ip+3,jp+3)] = p[FTNREF3D0(i,j,km,ip+3,jp+3)];
    }
  }
//!

  return;
}


#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

#define NEW_PAV

// ====================================================== KERNEL ===========================================================
__SUBKERNEL void press_pav_kernel (
#ifdef COMBINED_KERNEL
		__local float* pavs_th,
		__local float* pcos_th,
#endif
        __global float2* p,
        __global float *dx1,__global float *dy1,__global float *dzn,
		__global float *chunks_num,
		__global float *chunks_denom,
        const unsigned int im,
        const unsigned int jm,
        const unsigned int km

		) {
	const int ip = im;
	const int jp = jm;
	const int kp = km;

#if KERNEL == ORIG_KERNEL

//! Accumulation, make this a separate kernel
  float pav = 0.0F;
  float pco = 0.0F;
  for (unsigned int k=1;k<=km;k++) {
    for (unsigned int j=1;j<=jm;j++) {
      for (unsigned int i=1;i<=im;i++) {
    	  float dxyz =  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)];
        pav = pav + p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0 * dxyz;
        pco = pco + dxyz;
      }
    }
  }
  pav = pav / pco;

  rhs[0,0,0]=pav;

#elif KERNEL == CPU_KERNEL

#ifndef NEW_PAV
if (get_global_id(0)==0) {
//! Accumulation, make this a separate kernel
  float pav = 0.0F;
  float pco = 0.0F;
  for (unsigned int k=1;k<=km;k++) {
    for (unsigned int j=1;j<=jm;j++) {
      for (unsigned int i=1;i<=im;i++) {
    	  float dxyz =  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)];
        pav = pav + p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0 * dxyz;
        pco = pco + dxyz;
      }
    }
  }
//  pav = pav / pco;
//
//  rhs[0,0,0]=pav;

	chunks_num[0] = pav;
	chunks_denom[0] = pco;

}
#else
//#define NTH 1
// A better approach is to sum per compute unit with a fixed number of threads


	unsigned int gl_id = get_global_id(0);
	unsigned int gr_id = get_group_id(0);
	unsigned int nunits = get_num_groups(0);
	unsigned int l_id =  get_local_id(0);


//  unsigned int i = l_id+1;
  unsigned int il_bound = ip % NTH == 0 ? ip/NTH : ip/NTH+1;
#ifndef COMBINED_KERNEL
  __local float pavs_th[NTH]; // This must be more than the largest size of im.
  __local float pcos_th[NTH];
#endif
  float pav = 0.0F;
  float pco = 0.0F;
  unsigned int chunk_sz = kp / nunits;
  unsigned int kl_start= gr_id*chunk_sz;
  unsigned int kl_stop= (gr_id < nunits-1) ? (gr_id+1)* chunk_sz : kp;
  for (unsigned int kl = kl_start; kl<kl_stop; kl++) {
	unsigned int k = 1+kl;

  for (unsigned int il=0;il<il_bound;il++) {
	  unsigned int i = il*NTH+l_id+1;
	  if (i<= ip) {
  for (unsigned int j=1; j<=jm;j++) {
	  float dxyz =  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)];
	         pav = pav + p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0 * dxyz;
	         pco = pco + dxyz;
  } // for j
	  }  // if i <= ip

  } // for il
} // for kl

	pavs_th[l_id]=pav;
	pcos_th[l_id]=pco;

// On the CPU, this barrier breaks everything. On the GPU, the barrier is essential if NTH is larger than the physical warp size I think
// So we could be very ad-hoc-ish and have a test "if NTH>32"
//#if NTH > 32
  barrier(CLK_LOCAL_MEM_FENCE );
//#endif

  float pavs_g=0.0F;
  float pcos_g=0.0F;

  for (unsigned int ii=0;ii<NTH;ii++) {
  	pavs_g+=pavs_th[ii];
  	pcos_g+=pcos_th[ii];
	}

  	  chunks_num[gr_id] = pavs_g;
  	  chunks_denom[gr_id] = pcos_g;

#endif // NEW
#elif KERNEL == GPU_KERNEL
// NDRange global = im*km, local = im, loop over jm


//	unsigned int gl_id = get_global_id(0);
	unsigned int gr_id = get_group_id(0);
	unsigned int l_id =  get_local_id(0);
    unsigned int k = gr_id+1;
    unsigned int i = l_id+1;
#ifndef COMBINED_KERNEL
    __local float pavs_th[N_ELTS_TH]; // This must be more than the largest size of im.
    __local float pcos_th[N_ELTS_TH];
#endif
    float pav=0.0F;
    float pco=0.0F;
    for (unsigned int j=1; j<=jm;j++) {
    	float dxyz =  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)];
    	pav = pav + p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0 * dxyz;
    	pco = pco + dxyz;
    }

	pavs_th[l_id]=pav;
	pcos_th[l_id]=pco;
    barrier(CLK_LOCAL_MEM_FENCE);
    float pavs_g=0.0F;
    float pcos_g=0.0F;
    for (unsigned int ii=0;ii<im;ii++) { // 1 .. im => 0 .. im-1
    	pavs_g+=pavs_th[ii];
    	pcos_g+=pcos_th[ii];
	}
    chunks_num[gr_id] =  pavs_g;
	chunks_denom[gr_id] = pcos_g;
//	ksor[0] = gr_id*1000.0+l_id*1.0;

#endif
} // END of press_pav_kernel()


// ============================================================== ==============================================================

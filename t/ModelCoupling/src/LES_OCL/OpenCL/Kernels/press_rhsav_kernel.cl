#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

#define NEW_RHSAV

#ifndef COMBINED_KERNEL
void bondfgc_ (__global float4 *fgh,const int im,const int jm,const int km,int i,int j, int k) ;
#endif
// ====================================================== KERNEL ===========================================================
// From our experiments, it is clear that the best approach is to have globalrange=km*im, localrange=im, loop over jm
__SUBKERNEL void press_rhsav_kernel (
#ifdef COMBINED_KERNEL
		  __local float* rhsavs_th,
		  __local float* areas_th,
#endif
		__global float4* uvw,
		__global float4* fgh,
        __global float *rhs,
		__global float *dx1,__global float *dy1,__global float *dzn,
        __global float* chunks_num,
        __global float* chunks_denom,
        const float dt,
		const unsigned int im,
		const unsigned int jm,
		const unsigned int km
		        ) {

		  const int ip = im;
		  const int jp = jm;
		  const int kp = km;

#if KERNEL == ORIG_KERNEL

  int k;
  int j;
  int i;
  
  float rhsav = 0.0F;
  float area = 0.0F;
  for (k=1;k<=km;k++) {
    for (j=1;j<=jm;j++) {
      for (i=1;i<=im;i++) {
    	  float4 fgh_ijk=fgh[FTNREF3D0(i,j,k,ip+1,jp+1)];
    	  float4 uvw_ijk=uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)];
    	  bondfgc_( fgh,im,jm,km,i,j,k);
    	  float rhs_tmp1 =
        		(-uvw[FTNREF3D(i - 1,j,k,ip+2,jp+3,0,-1,-1)].s0 + uvw_ijk.s0) / dx1[FTNREF1D(i,-1)]
	          + (-uvw[FTNREF3D(i,j - 1,k,ip+2,jp+3,0,-1,-1)].s1 + uvw_ijk.s1) / dy1[FTNREF1D(j,0)]
	          + (-uvw[FTNREF3D(i,j,k - 1,ip+2,jp+3,0,-1,-1)].s2 + uvw_ijk.s2) / dzn[FTNREF1D(k,-1)];
    	  //! --stretch
    	  float rhs_tmp2=
        	 (fgh_ijk.s0 - fgh[FTNREF3D0(i - 1,j,k,ip+1,jp+1)].s0) / dx1[FTNREF1D(i,-1)]
	       + (fgh_ijk.s1 - fgh[FTNREF3D0(i,j - 1,k,ip+1,jp+1)].s1) / dy1[FTNREF1D(j,0)]
           + (fgh_ijk.s2 - fgh[FTNREF3D0(i,j,k - 1,ip+1,jp+1)].s2) / dzn[FTNREF1D(k,-1)]
	       + rhs_tmp1 / dt;

		  rhsav +=  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)] * rhs_tmp2;
          area  +=  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)];
          rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] = rhs_tmp2;
      }
    }
  }
  rhsav/=area;
  // This is a trick to save a kernel argument: as far as I can tell, rhs[0,0,0] is not used anywhere.
  rhs[0,0,0]=rhsav;


#elif KERNEL == CPU_KERNEL

#ifndef NEW_RHSAV
  if (get_global_id(0)==0) {
	float rhsav = 0.0F;
	float area = 0.0F;
	float rhs_sum_nobounds = 0.0;
	for (unsigned int k=1;k<=km;k++) {
		for (unsigned int j=1;j<=jm;j++) {
			for (unsigned int i=1;i<=im;i++) {
				float4 fgh_ijk=fgh[FTNREF3D0(i,j,k,ip+1,jp+1)];
				float4 uvw_ijk=uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)];
				bondfgc_( fgh,im,jm,km,i,j,k);
				float rhs_tmp1 =
					(-uvw[FTNREF3D(i - 1,j,k,ip+2,jp+3,0,-1,-1)].s0 + uvw_ijk.s0) / dx1[FTNREF1D(i,-1)]
				  + (-uvw[FTNREF3D(i,j - 1,k,ip+2,jp+3,0,-1,-1)].s1 + uvw_ijk.s1) / dy1[FTNREF1D(j,0)]
				  + (-uvw[FTNREF3D(i,j,k - 1,ip+2,jp+3,0,-1,-1)].s2 + uvw_ijk.s2) / dzn[FTNREF1D(k,-1)];
				//! --stretch
				float rhs_tmp2=
				   (fgh_ijk.s0 - fgh[FTNREF3D0(i - 1,j,k,ip+1,jp+1)].s0) / dx1[FTNREF1D(i,-1)]
			     + (fgh_ijk.s1 - fgh[FTNREF3D0(i,j - 1,k,ip+1,jp+1)].s1) / dy1[FTNREF1D(j,0)]
			     + (fgh_ijk.s2 - fgh[FTNREF3D0(i,j,k - 1,ip+1,jp+1)].s2) / dzn[FTNREF1D(k,-1)]
			     + rhs_tmp1 / dt;
			    rhsav +=  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)] * rhs_tmp2;
			    area  +=  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)];
			    rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] = rhs_tmp2;
			    rhs_sum_nobounds += rhs[FTNREF3D0(i,j,k,ip+2,jp+2)];
			}
		}
	 }
//	ksor[0]=rhs_sum_nobounds;
	chunks_num[0] = rhsav;
	chunks_denom[0] = area;
	//  rhsav/=area;
	//  // This is a trick to save a kernel argument: as far as I can tell, rhs[0,0,0] is not used anywhere.
	//  rhsav=55.7188;
	//  rhs[0,0,0]=rhsav;
  }
#else

// A better approach is to sum per compute unit with a fixed number of threads


	unsigned int gl_id = get_global_id(0);
	unsigned int gr_id = get_group_id(0);
	unsigned int nunits = get_num_groups(0);
	unsigned int l_id =  get_local_id(0);

//  unsigned int k = gr_id+1;
	//  unsigned int i = l_id+1;
	  unsigned int il_bound = ip % NTH == 0 ? ip/NTH : ip/NTH+1;
#ifndef COMBINED_KERNEL
  __local float rhsavs_th[NTH]; // This must be more than the largest size of im.
  __local float areas_th[NTH];
#endif
  float rhsav=0.0F;
  float area=0.0F;
  unsigned int k_range = kp;
  unsigned int chunk_sz = k_range / nunits;
  unsigned int kl_start= gr_id*chunk_sz;
  unsigned int kl_stop= (gr_id < nunits-1) ? (gr_id+1)* chunk_sz : k_range;
  for (unsigned int kl = kl_start; kl<kl_stop; kl++) {
	  unsigned int k = 1+kl;
	  for (unsigned int il=0;il<il_bound;il++) {
		  unsigned int i = il*NTH+l_id+1;
		  if (i<ip+1) {
			  for (unsigned int j=1; j<=jm;j++) {
				  float4 fgh_ijk=fgh[FTNREF3D0(i,j,k,ip+1,jp+1)];
				  float4 uvw_ijk=uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)]; // uvw here has random values!
				  bondfgc_( fgh,im,jm,km,i,j,k); // this modifies fgh
				  float rhs_tmp1 =
						  (-uvw[FTNREF3D(i - 1,j,k,ip+2,jp+3,0,-1,-1)].s0 + uvw_ijk.s0) / dx1[FTNREF1D(i,-1)]
	                    + (-uvw[FTNREF3D(i,j - 1,k,ip+2,jp+3,0,-1,-1)].s1 + uvw_ijk.s1) / dy1[FTNREF1D(j,0)]
                        + (-uvw[FTNREF3D(i,j,k - 1,ip+2,jp+3,0,-1,-1)].s2 + uvw_ijk.s2) / dzn[FTNREF1D(k,-1)];
				  //! --stretch
				  float rhs_tmp2=
						  (fgh_ijk.s0 - fgh[FTNREF3D0(i - 1,j,k,ip+1,jp+1)].s0) / dx1[FTNREF1D(i,-1)]
                        + (fgh_ijk.s1 - fgh[FTNREF3D0(i,j - 1,k,ip+1,jp+1)].s1) / dy1[FTNREF1D(j,0)]
                        + (fgh_ijk.s2 - fgh[FTNREF3D0(i,j,k - 1,ip+1,jp+1)].s2) / dzn[FTNREF1D(k,-1)]
                        + rhs_tmp1 / dt;
				  float weight = dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)];
				  rhsav +=  rhs_tmp2 * weight;
				  area  +=  weight;
				  rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] = rhs_tmp2; // So, this value is fine. How come rhsav is not fine, esp. considering area is fine?
			  }
		  }
	  }
  }
	rhsavs_th[l_id]=rhsav;
	areas_th[l_id]=area;

// On the CPU, this barrier breaks everything. On the GPU, the barrier is essential if NTH is larger than the physical warp size I think
// SO we could be very ad-hoc-ish and have a test "if NTH>32"
//#if NTH > 32
  barrier(CLK_LOCAL_MEM_FENCE );
//#endif


if (l_id==0) {

  float rhsavs_g=0.0F;
  float areas_g=0.0F;

  for (unsigned int ii=0;ii<NTH;ii++) {
  	rhsavs_g+=rhsavs_th[ii];
  	areas_g+=areas_th[ii];
	}

  	  chunks_num[gr_id] = rhsavs_g;
  	  chunks_denom[gr_id] = areas_g;
}
#endif
#elif KERNEL == GPU_KERNEL
// NDRange global = im*km, local = im, loop over jm

	unsigned int gl_id = get_global_id(0);
	unsigned int gr_id = get_group_id(0);
	unsigned int l_id =  get_local_id(0);
    unsigned int k = gr_id+1;
    unsigned int i = l_id+1;
#ifndef COMBINED_KERNEL
    __local float rhsavs_th[N_ELTS_TH]; // This must be more than the largest size of im.
    __local float areas_th[N_ELTS_TH];
#endif
    float rhsav=0.0F;
    float area=0.0F;
    for (unsigned int j=1; j<=jm;j++) {
    	 float4 fgh_ijk=fgh[FTNREF3D0(i,j,k,ip+1,jp+1)];
    	    	  float4 uvw_ijk=uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)];
    	    	  bondfgc_( fgh,im,jm,km,i,j,k);
    	    	  float rhs_tmp1 =
    	        		(-uvw[FTNREF3D(i - 1,j,k,ip+2,jp+3,0,-1,-1)].s0 + uvw_ijk.s0) / dx1[FTNREF1D(i,-1)]
    		          + (-uvw[FTNREF3D(i,j - 1,k,ip+2,jp+3,0,-1,-1)].s1 + uvw_ijk.s1) / dy1[FTNREF1D(j,0)]
    		          + (-uvw[FTNREF3D(i,j,k - 1,ip+2,jp+3,0,-1,-1)].s2 + uvw_ijk.s2) / dzn[FTNREF1D(k,-1)];
    	    	  //! --stretch
    	    	  float rhs_tmp2=
    	        	 (fgh_ijk.s0 - fgh[FTNREF3D0(i - 1,j,k,ip+1,jp+1)].s0) / dx1[FTNREF1D(i,-1)]
    		       + (fgh_ijk.s1 - fgh[FTNREF3D0(i,j - 1,k,ip+1,jp+1)].s1) / dy1[FTNREF1D(j,0)]
    	           + (fgh_ijk.s2 - fgh[FTNREF3D0(i,j,k - 1,ip+1,jp+1)].s2) / dzn[FTNREF1D(k,-1)]
    		       + rhs_tmp1 / dt;

    			  rhsav +=  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)] * rhs_tmp2;
    	          area  +=  dx1[FTNREF1D(i,-1)] * dy1[FTNREF1D(j,0)] * dzn[FTNREF1D(k,-1)];
    	          rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] = rhs_tmp2;
    }

	rhsavs_th[l_id]=rhsav;
	areas_th[l_id]=area;
    barrier(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
    float rhsavs_g=0.0F;
    float areas_g=0.0F;
    for (unsigned int ii=0;ii<im;ii++) {
    	rhsavs_g+=rhsavs_th[ii];
    	areas_g+=areas_th[ii];
	}
    chunks_num[gr_id] =  rhsavs_g;
	chunks_denom[gr_id] =areas_g;

#endif
} // END of press_rhsav_kernel()
// ============================================================== ==============================================================


void bondfgc_ (__global float4 *fgh,const int im,const int jm,const int km, int i, int j, int k) {

	const int ip = im;
	const int jp = jm;
	const int kp = km;
	//! --inflow condition
	if (i==1) {
		fgh[FTNREF3D0(0,j,k,ip+1,jp+1)].s0 = fgh[FTNREF3D0(1,j,k,ip+1,jp+1)].s0;
	}
	//! --sideflow condition
	if (j==1) {
		fgh[FTNREF3D0(i,0,k,ip+1,jp+1)].s1 = fgh[FTNREF3D0(i,jm,k,ip+1,jp+1)].s1;
	}
	//! --ground and top condition
	if (k==1) {
		fgh[FTNREF3D0(i,j,0,ip+1,jp+1)].s2 = 0.0F;
		fgh[FTNREF3D0(i,j,km,ip+1,jp+1)].s2 = 0.0F;
	}
}

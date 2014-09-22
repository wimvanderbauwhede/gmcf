#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

/*
 Considering the awkward structure of this code, I just run it on one core, and use the threads within the core. So we do get_local_id().
And we parallelise e.g. just the loop over j, so no stalling
We do this by setting the local size to jm, or maybe jm+2?
Now, if im==jm it would be a lot easier.

So, either we simply run this sequentially and hope the time is very short;
Or we try a single core but parallel threads, this seems better
Or we split it into multiple kernels, but then we have the cost of going back to the host
For all runs except n=1, we only compute for i=0 and i=1, so typically 2*km*jm 
If I'd set the local range to jm+2 then I need to exclude -1 and jm+1, we can do either

if (j>-1 && j<jm+1) {
// do this
}
//always do that

but the cost will be the same as to do
// do this
// do that

Furthermore, if im!=jm then either im<jm or im>jm
if im>jm then I need to add a loop for(int i=jm+1;i<=im;i++) {}
if im<jm then I need to add a guard if (i<=im) {}


SUMMARY:
set GlobalRange= jm
set LocalRange = jm

 */


__SUBKERNEL void bondv1_calc_uout_kernel (
#ifdef COMBINED_KERNEL
		__local float* aaat,
		__local float* bbbt,
#endif
    __global float4* uvw,
    __global float* uout_ptr,
    __global float* aaa_chunks,
    __global float* bbb_chunks,
    const unsigned int im,
    const unsigned int jm,
    const unsigned int km
        ) {
	const int ip=im;
	const int jp=jm;
	const int kp=km;

#if KERNEL == GPU_KERNEL
	if (get_group_id(0)==0) {
#ifndef COMBINED_KERNEL
		__local float aaat[N_ELTS_TH]; // was jm // FIXME! Not portable!
		__local float bbbt[N_ELTS_TH]; // was jm // FIXME! Not portable!
#endif


	unsigned int j =  get_local_id(0)+1;

//! ------------- outflow condition ------------
//!      advective condition
//! 
/*
 * We're looking for the largest/smallest value in all j,k for i=im
* I can either do one loop per group or one group and loop per thread
* CPU version should be the former
*/
	  aaat[j-1]=0.0F;
	  bbbt[j-1]=0.0F; // WV: strange as it is a minimum
  for (unsigned int k=1;k<=km;k++) {      
      float u_im_j_k=uvw[FTNREF3D(im,j,k,ip+2,jp+3,0,-1,-1)].s0;
      aaat[j-1] = fmax(aaat[j-1],u_im_j_k);
      bbbt[j-1] = fmin(bbbt[j-1],u_im_j_k);
  }


// So here we need a local mem barrier, else we can not compute the following :
  	barrier(CLK_LOCAL_MEM_FENCE);


  	float aaa=0.0F;
  	float bbb=0.0F; // So I not this will be 0 unless one of the uvw[] values is smaller than 0!
    for (unsigned int jj=0;jj<jm;jj++) {
        aaa=fmax(aaat[jj],aaa);
        bbb=fmin(bbbt[jj],bbb);
    }
    uout_ptr[0] = (aaa + bbb) * 0.5f;
	}

//!
#elif KERNEL == CPU_KERNEL
// On my Mac, local mem does not work. So we simply create jp threads that loop over k and i
#define LOCAL_MEM_WORKS
#ifdef LOCAL_MEM_WORKS
#ifndef COMBINED_KERNEL
	__local float aaat[NTH];
	__local float bbbt[NTH];
#endif
	unsigned int l_id = get_local_id(0);
	unsigned int gl_id = get_global_id(0);
	unsigned int gr_id =  get_group_id(0);
	unsigned int nunits = get_num_groups(0);

	  unsigned int j_range = jm;

		unsigned int j_start =  (j_range / nunits)*gr_id;
		unsigned int j_stop = (j_range / nunits)*(gr_id+1);
		if (gr_id==nunits-1) {
			j_stop=j_range;
		}

//	unsigned int j =  get_group_id(0)+1;

		float maxa = 0.0f; //
		float minb = 0.0f; // this should of course be a large number to be correct!
		 // we could of course do a limited number of threads
		 // if km is not divisible by NTH, we need to correct
		 unsigned int kl_bound = (km % NTH) == 0 ? km/NTH : km/NTH+1;
		 for (unsigned int jl=j_start;jl<j_stop;jl++) {
			 unsigned int j = jl+1;
		  for (unsigned int kl=0;kl<kl_bound;kl++) {
			  unsigned int k = 1+kl*NTH+l_id;
			  if (k<=km) {
				  float u_im_j_k=uvw[FTNREF3D(im,j,k,ip+2,jp+3,0,-1,-1)].s0;
				  maxa = fmax(maxa,u_im_j_k);
				  minb = fmin(minb,u_im_j_k);
			  }
		  }
		 }
		 aaat[l_id] = maxa;
		 bbbt[l_id] = minb;
		  barrier(CLK_LOCAL_MEM_FENCE);

		  if (l_id==0) {
			float aaa=0.0f;
		  	float bbb=0.0f; // So I not this will be 0 unless one of the uvw[] values is smaller than 0!

		    for (unsigned int jj=0;jj<NTH;jj++) {
		        aaa=fmax(aaat[jj],aaa);
		        bbb=fmin(bbbt[jj],bbb);
		    }
		    aaa_chunks[gr_id] = aaa ;
		    bbb_chunks[gr_id] = bbb ;
		  }

#else

		  // Just a single thread per compute unit
		  unsigned int gr_id =  get_group_id(0);
		  unsigned int l_id = get_local_id(0);
		  unsigned int nunits = get_num_groups(0);
		  unsigned int n_chunks = (jm / nunits);
			unsigned int j_start =  n_chunks*gr_id;
			unsigned int j_stop = n_chunks*(gr_id+1);
			if (gr_id==nunits-1) {
				j_stop=jm-1;
			}
			// we could of course do a limited number of threads
			// if km is not divisible by NTH, we need to correct
			  float aaatt = 0.0F;
			  float bbbtt = 0.0F;
			  for (unsigned int lj=j_start;lj<j_stop;lj++) {
				  unsigned int j = lj+1;
				  for (unsigned int k=1;k<=kp;k++) {
						  float u_im_j_k=uvw[FTNREF3D(im,j,k,ip+2,jp+3,0,-1,-1)].s0;
						  aaatt = fmax(aaatt,u_im_j_k);
						  bbbtt = fmin(bbbtt,u_im_j_k);
				  }
			  }
			  	if (l_id==0) {
				  aaa_chunks[gr_id] = aaatt;
				  bbb_chunks[gr_id] = bbbtt;
			}

#endif
#endif

}

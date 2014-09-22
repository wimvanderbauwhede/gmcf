#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

#define NEW_BOUND_SM

/*
 Given that im,jm,km are about 100, then we have a loop over 3*100*100 compared to a loop over 100*100*100
 I guess that keeping this on the host is really the quickest way. So, not a kernel, just a function.
 We read back sm from calc_sm, add the bounds, and feed it to calc_visc.
 But suppose we wanted it in parallel, then I guess what we do is have the outer index from the group, the inner from local.
 Or, maybe better, use the actual cores as ngroups, and have the range so that it is a multple. But that gets complicated.
 So, let's try this:
  */

// max_range = max(max(km+2,jm+3),im+3);
// globalrange=max_range*max_range
// localrange=max_range

__SUBKERNEL void les_bound_sm_kernel (
        __global float4 *fgh,
        __global float *dx1,__global float *dy1,__global float *dzn,
        __global float16 *diu,
        __global float *sm,
        const unsigned int im,
        const unsigned int jm,
        const unsigned int km
        
        ) {

	const unsigned int ip = im;
	const unsigned int jp = jm;
	const unsigned int kp = km;
#if KERNEL == GPU_KERNEL
	int idx_o = get_group_id(0);
	int idx_i = get_local_id(0);
//! We have computed sm in the range [1:im,1:jm,1:km]
//! Now we do i=0 and i=im+1, for all j and k, including j=-1 and j=jm+1, and k=km+1
//! We use values at i=1 for values at i=0, and i=im for i=im+1, therefore we need to compute sm first.
//! =================================
//  for (k=0;k<=km + 1;k++) {
//    for (j=-1;j<=jm + 1;j++) {
    	int k = idx_o-0;
    	int j = idx_i-1;
    	if (k<km+2 && j <jm+2) {
      sm[FTNREF3D(0,j,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(1,j,k,ip+3,jp+3,-1,-1,0)];
      sm[FTNREF3D(im + 1,j,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(im,j,k,ip+3,jp+3,-1,-1,0)];
    	}
//    }
//  }
//! --side flow condition
//  for (k=0;k<=km + 1;k++) {
//    for (i=0;i<=im + 1;i++) {

      int i = idx_i-0;
      if (k<km+1 && i<im+2) {
      sm[FTNREF3D(i,jm + 1,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(i,jm,k,ip+3,jp+3,-1,-1,0)];
      sm[FTNREF3D(i,0,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(i,1,k,ip+3,jp+3,-1,-1,0)];
      }
//    }
//  }
//! --underground condition
//  for (j=-1;j<=jm + 1;j++) {
//    for (i=0;i<=im + 1;i++) {
      j = idx_o-1;
      i = idx_i-0;
      if(j<jm+2 && i<im+2) {
      sm[FTNREF3D(i,j,0,ip+3,jp+3,-1,-1,0)] = -sm[FTNREF3D(i,j,1,ip+3,jp+3,-1,-1,0)];
      sm[FTNREF3D(i,j,km + 1,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(i,j,km,ip+3,jp+3,-1,-1,0)];
      }
//    }
//  }
#elif KERNEL == CPU_KERNEL

#ifndef NEW_BOUND_SM
      if (get_global_id(0)==0) {

    	//! We have computed sm in the range [1:im,1:jm,1:km]
    	//! Now we do i=0 and i=im+1, for all j and k, including j=-1 and j=jm+1, and k=km+1
    	//! We use values at i=1 for values at i=0, and i=im for i=im+1, that's why we need to compute sm first.
    	//! =================================
    	  for (k=0;k<=km + 1;k++) {
    	    for (j=-1;j<=jm + 1;j++) {
    	      sm[FTNREF3D(0,j,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(1,j,k,ip+3,jp+3,-1,-1,0)];
    	      sm[FTNREF3D(im + 1,j,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(im,j,k,ip+3,jp+3,-1,-1,0)];
    	    }
    	  }
    	//! --side flow condition
    	  for (k=0;k<=km + 1;k++) {
    	    for (i=0;i<=im + 1;i++) {

    	      sm[FTNREF3D(i,jm + 1,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(i,jm,k,ip+3,jp+3,-1,-1,0)];
    	      sm[FTNREF3D(i,0,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(i,1,k,ip+3,jp+3,-1,-1,0)];
    	    }
    	  }
    	//! --underground condition
    	  for (j=-1;j<=jm + 1;j++) {
    	    for (i=0;i<=im + 1;i++) {
    	      sm[FTNREF3D(i,j,0,ip+3,jp+3,-1,-1,0)] = -sm[FTNREF3D(i,j,1,ip+3,jp+3,-1,-1,0)];
    	      sm[FTNREF3D(i,j,km + 1,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(i,j,km,ip+3,jp+3,-1,-1,0)];
    	    }
    	  }
      }
#else
      // New approach: global range = (jm+3)*(km+2) + (km+2)*(im+2)+(jm+3)*(im+2)
      unsigned int gl_id = get_global_id(0);
      if (gl_id < (jm+3)*(km+2) ) {
      	unsigned int k = gl_id / (jm+3) ; // 0 .. km+1
      	unsigned int j = gl_id % (jm+3) - 1; // -1 .. jm+2
	      sm[FTNREF3D(0,j,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(1,j,k,ip+3,jp+3,-1,-1,0)];
	      sm[FTNREF3D(im + 1,j,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(im,j,k,ip+3,jp+3,-1,-1,0)];

      } else if (gl_id <(jm+3)*(km+2) + (km+2)*(im+2)) {
    	  unsigned int k = (gl_id -(jm+3)*(km+2)) / (im+2) ;
    	  unsigned int i = (gl_id -(jm+3)*(km+2)) % (im+2);
	      sm[FTNREF3D(i,jm + 1,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(i,jm,k,ip+3,jp+3,-1,-1,0)];
	      sm[FTNREF3D(i,0,k,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(i,1,k,ip+3,jp+3,-1,-1,0)];

      } else if (gl_id < (jm+3)*(km+2) + (km+2)*(im+2) + (jm+3)*(im+2)) {
    	  unsigned int j = (gl_id -(jm+3)*(km+2)-(km+2)*(im+2)) / (im+2) - 1;
    	  unsigned int i = (gl_id -(jm+3)*(km+2)-(km+2)*(im+2)) % (im+2) ;
	      sm[FTNREF3D(i,j,0,ip+3,jp+3,-1,-1,0)] = -sm[FTNREF3D(i,j,1,ip+3,jp+3,-1,-1,0)];
	      sm[FTNREF3D(i,j,km + 1,ip+3,jp+3,-1,-1,0)] = sm[FTNREF3D(i,j,km,ip+3,jp+3,-1,-1,0)];
      }
#endif
#endif
} // END of les_bound_sm_kernel


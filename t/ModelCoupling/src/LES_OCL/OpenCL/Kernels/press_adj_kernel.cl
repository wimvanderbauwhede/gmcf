#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

// ====================================================== KERNEL ===========================================================
__SUBKERNEL void press_adj_kernel (
        __global float2* p,
        __global float *pav_ptr,
        const unsigned int im,
        const unsigned int jm,
        const unsigned int km
        ) {

	const int ip = im;
	const int jp = jm;
	const int kp = km;

	float pav = *pav_ptr;

#if KERNEL != ORIG_KERNEL

	  int idx = get_global_id(0);
	  if (idx<im*jm*km) {
//	  int k = idx;
	  int4 ijk = calc_loop_iters(idx,im,jm,km,1,1,1);
	  int j = ijk.s1;
	  int k = ijk.s2;
	  int i = ijk.s0;

	  p[FTNREF3D0(i, j, k, ip + 3, jp + 3)].s0 -= pav;
	  }

#else

	if(get_global_id(0)==0) {

		for (unsigned int k = 1; k <= km ; k++) {
			for (unsigned int j = 1; j <= jm; j++) {
				for (unsigned int i = 1; i <= im ; i++) {

						p[FTNREF3D0(i, j, k, ip + 3, jp + 3)].s0 -= pav;
				}
			}
		}
	}

#endif
} // END of press_adj_kernel()
// ============================================================== ==============================================================

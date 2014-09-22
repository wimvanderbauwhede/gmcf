#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"


__SUBKERNEL void velnw__bondv1_init_uvw_kernel (
    __global float2 * p,
    __global float4 * uvw,
    __global float4 * fgh, 
    __global float * dxs, 
    __global float * dys, 
    __global float * dzs, 
    __global float * dzn,
#ifndef EXTERNAL_WIND_PROFILE
    __global float * z2,
#else
    __global float * wind_profile,
#endif
    __global unsigned int* n_ptr,
    const unsigned int im, 
    const unsigned int jm, 
    const unsigned int km,
    const float dt
        ) {

  const unsigned int ip = im;
  const unsigned int jp = jm;
  const unsigned int kp = km; 

  unsigned int n = *n_ptr;

  const float inv_ro = 1/RO;
  unsigned int idx = get_global_id(0);
  if (idx<(im+1)*jm*km) {
//	  uvw[idx]=(float4)(0.0f, 0.0f, 0.0f, 0.0f);
  int4 ijk = calc_loop_iters(idx,im+1,jm,km,0,1,1); // i starts at 0
  int j = ijk.s1;
  int k = ijk.s2;
  int i = ijk.s0;

	if (i>0) {
		float mp_ijk = -p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0  ;
		float pzu = inv_ro*(mp_ijk + p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)].s0) / dxs[i];
		float pzv = inv_ro*(mp_ijk + p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)].s0) / dys[j];
		float pzw = inv_ro*(mp_ijk + p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)].s0) / dzs[k-1];
		float4 uvw_ijk = uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)];
		float4 fgh_ijk = fgh[FTNREF3D0(i,j,k,ip+1,jp+1)];
		uvw_ijk.s0 += dt * (fgh_ijk.s0 - pzu);
		uvw_ijk.s1 += dt * (fgh_ijk.s1 - pzv);
		if (k!=km) {
		uvw_ijk.s2 += dt * (fgh_ijk.s2 - pzw);
		}
		uvw_ijk.s3 = 0.0F;
		uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)]=uvw_ijk;

	}
// WV: For model coupling, and actually to make it more generic anyway, this code should be taken out
// Instead, we should transfer the wind profile from the host
// This means I need a wind profile array over all j and k
	if (i<2) { // so i = 0 or 1
#ifdef EXTERNAL_WIND_PROFILE
		float uval = wind_profile[FTNREF2D(j,k,jp,1,1)];
#else
		float uval = 5.0f * pow(((z2[k-1] + 0.5f * dzn[k+1]) / 600.0f),0.2f);
		if (k>78) {
		   uval = uvw[FTNREF3D(i,j,77,ip+2,jp+3,0,-1,-1)].s0;
		}
#endif
		uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)]=(float4)(uval, 0.0f, 0.0f, 0.0f);

	}
#if ICAL == 0
	else { // on the first time step, copy the values from i==1 to all i
		if ( n == 1 ) {
			uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)] = uvw[FTNREF3D(1,j,k,ip+2,jp+3,0,-1,-1)];
		}
	}
#endif

} // idx range

}

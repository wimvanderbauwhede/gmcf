#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

__SUBKERNEL void feedbf__les_calc_sm_kernel(
        __global float4* uvw,
        __global float4 *uvwsum,
        __global float4* fgh,
        __global float4 *mask1,
        __global float16* diu,
        __global float * dxs,
        __global float* dzs,
        __global float* dx1,
        __global float* dy1,
        __global float* dzn,
        __global float *sm,
        __global float * uout_ptr,
        const float dt,
        const unsigned int im,
        const unsigned int jm,
        const unsigned int km
        
        ) {
    const float vn = 1.583e-05F; // FIXME: taken from set.f, should maybe be a macro?
    const float alpha = -10.0F; // FIXME: should be macro
    const float beta = -1.0F; // FIXME: should be macro
    const float cs0 = 0.1F;// FIXME: should be macro
    const float csx1 = cs0;

    const unsigned int ip = im;
    const unsigned int jp = jm;
	const unsigned int kp = km;
	unsigned int idx=get_global_id(0);
//	unsigned int gl_id=get_global_id(0);
//	if (gl_id == 0) {
//	for (unsigned int idx = 0;idx<im*jm*km;idx++) {
	if (idx<im*jm*km) {
    int4 ijk=calc_loop_iters(idx,im,jm,km,1,1,1); // so all ranges 1..im, 1..jm, 1..km, and we go to ip+1 for dx1, OK

    int j = ijk.s1;
    int k = ijk.s2;
    int i = ijk.s0;

// FEEDBF
    float4 fgh_ijk;
  	  	float4 uvw_ijk = uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)]; // 0:ip+1,-1:jp+1,-1:kp+1 => ip+2, jp+3, 0,-1,-1
  	    float4 uvwsum_ijk = uvwsum[FTNREF3D0(i,j,k,ip+1,jp+1)];
  	    float4 mask1_ijk = mask1[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)];// -1:ip+1,-1:jp+1,0:kp+1 => ip+3, jp+3, -1,-1,0
//  	    float4 fgh_ijk= fgh[FTNREF3D0(i,j,k,ip+1,jp+1)];

  	    uvwsum_ijk.s0 = (uvwsum_ijk.s0 + uvw_ijk.s0)*mask1_ijk.s0;
  	    uvwsum_ijk.s1 = (uvwsum_ijk.s1 + uvw_ijk.s1)*mask1_ijk.s1;
  	    uvwsum_ijk.s2 = (uvwsum_ijk.s2 + uvw_ijk.s2)*mask1_ijk.s2;
        float f1x = alpha * uvwsum_ijk.s0 * dt;
        float f1y = alpha * uvwsum_ijk.s1  * dt;
        float f1z = alpha * uvwsum_ijk.s2  * dt;
        float f2x = beta * uvw_ijk.s0 * mask1_ijk.s0;
        float f2y = beta * uvw_ijk.s1 * mask1_ijk.s1;
        float f2z = beta * uvw_ijk.s2 * mask1_ijk.s2;
        float fx = f1x + f2x;
        float fy = f1y + f2y;
        float fz = f1z + f2z;
        fgh_ijk.s0 += fx;
        fgh_ijk.s1 += fy;
        fgh_ijk.s2 += fz;
        fgh_ijk.s3=0.0F;
        fgh[FTNREF3D0(i,j,k,ip+1,jp+1)]= fgh_ijk;
        uvwsum[FTNREF3D0(i,j,k,ip+1,jp+1)] = uvwsum_ijk;

// LES_CALC_SM

  // 9vec: -1:ip+2,0:jp+2,0:kp+2 => ip+2 - (-1) + 1 ,jp+2 - 0 + 1, -1,0,0 = ip+4,jp+3,-1,0,0
  // 8x
  //float16
  float16 diu_ijk=diu[FTNREF3D(i,j,k,ip+4,jp+3,-1,0,0)];
  // 2x
  float16 diu_im1_jk = diu[FTNREF3D(i - 1,j,k,ip+4,jp+3,-1,0,0)] ;
  float16 diu_i_jp1_k = diu[FTNREF3D(i,j + 1,k,ip+4,jp+3,-1,0,0)];
  float16 diu_i_jm1_k = diu[FTNREF3D(i,j - 1,k,ip+4,jp+3,-1,0,0)];
  float16 diu_ij_kp1 = diu[FTNREF3D(i,j,k + 1,ip+4,jp+3,-1,0,0)];
  float16 diu_ip1_jk = diu[FTNREF3D(i + 1,j,k,ip+4,jp+3,-1,0,0)];
  float16 diu_ij_km1 = diu[FTNREF3D(i,j,k - 1,ip+4,jp+3,-1,0,0)];
  // 1x
  float16 diu_im1_jp1_k = diu[FTNREF3D(i - 1,j + 1,k,ip+4,jp+3,-1,0,0)];
  float16 diu_im1_j_kp1 = diu[FTNREF3D(i - 1,j,k + 1,ip+4,jp+3,-1,0,0)];
  float16 diu_ip1_jm1_k = diu[FTNREF3D(i + 1,j - 1,k,ip+4,jp+3,-1,0,0)];
  float16 diu_ip1_j_km1 = diu[FTNREF3D(i + 1,j,k - 1,ip+4,jp+3,-1,0,0)];
  float16 diu_i_jm1_kp1 = diu[FTNREF3D(i,j - 1,k + 1,ip+4,jp+3,-1,0,0)];
  float16 diu_i_jp1_km1 = diu[FTNREF3D(i,j + 1,k - 1,ip+4,jp+3,-1,0,0)];

  //! --length scale
//! ----
  // brought inside loop for parallelization
	float delx1_ = cbrt( dx1[FTNREF1D(0,-1)] * dy1[FTNREF1D(0,0)] * dzn[FTNREF1D(k,-1)] );  // cube root
	float cs_dx = SQR((csx1 * delx1_));

//! --calculation of sgs eddy viscosity coeficient
	float dudxx1 = diu_ijk.s0;
	// i-1,j,k ; i-1,j+1,k ; i,j,k, i, j+1,k
	float dudyx1 = (diu_im1_jk.s1 + diu_im1_jp1_k.s1 + diu_ijk.s1 + diu_i_jp1_k.s1) * 0.25F;
	float dudzx1 = (diu_im1_jk.s2 + diu_im1_j_kp1.s2 + diu_ijk.s2 + diu_ij_kp1.s2) * 0.25F;
	float dvdxx1 = (diu_ijk.s3 + diu_i_jm1_k.s3 + diu_ip1_jk.s3 + diu_ip1_jm1_k.s3) * 0.25F;
	float dvdyx1 = diu_ijk.s4;
	float dvdzx1 = (diu_i_jm1_k.s5 + diu_i_jm1_kp1.s5 + diu_ijk.s5 + diu_ij_kp1.s5) * 0.25F;
	float dwdxx1 = (diu_ijk.s6 + diu_ij_km1.s6 + diu_ip1_jk.s6 + diu_ip1_j_km1.s6) * 0.25F;
	float dwdyx1 = (diu_ijk.s7 + diu_ij_km1.s7 + diu_i_jp1_k.s7 + diu_i_jp1_km1.s7) * 0.25F;
	float dwdzx1 = diu_ijk.s8;
	//! --abl or channel
	sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] = cs_dx * sqrt(2.0F * (SQR(dudxx1) + SQR(dvdyx1) + SQR(dwdzx1)) + SQR((dudyx1 + dvdxx1)) + SQR((dwdyx1 + dvdzx1)) + SQR((dudzx1 + dwdxx1)));

	}

//	} // for
//	} // if gl_id == 0
} // END of feedbf__les_calc_sm_kernel


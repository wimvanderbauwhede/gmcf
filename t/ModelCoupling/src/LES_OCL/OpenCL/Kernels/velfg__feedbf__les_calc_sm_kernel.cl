#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

// Problem is that there must be a bug as this does leads to divergence in SOR!
//#define NEW_VELFG

#ifndef NEW_VELFG
// This is the unified loop of vel2, this replaces the call to the original vel2. I guess I can wrap it in the vel2 function.
#ifndef COMBINED_KERNEL
void vel2(
    __global float4 * uvw,
    __global float16 * diu,
	float16* cov_ijk,
	float16* cov_ijk_p1,
	float16* diu_ijk,
	float16* diu_ijk_p1,
    __global float * dzs,
    __global float * dx1,
    __global float * dy1,
    __global float * dzn,
    const unsigned int im,
    const unsigned int jm,
    const unsigned int km,
    int i,int j,int k
	);
#endif
#endif

__SUBKERNEL void velfg__feedbf__les_calc_sm_kernel(
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
	unsigned int gl_id=get_global_id(0);


#ifdef NEW_VELFG

	const unsigned int u0 = 0;
    float16 diu_ijk ;//= (float16)(0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F);
	float4 fgh_ijk;// = (float4)(0.0F,0.0F,0.0F,0.0F);

	if (idx<im*jm*(km+1)) {
    int4 ijk=calc_loop_iters(idx,im,jm,km+1,1,1,1); // so all ranges 1..im, 1..jm, 1..km, and we go to ip+1 for dx1, OK

     int j = ijk.s1;
     int k = ijk.s2;
     int i = ijk.s0;
//     fgh_ijk=fgh[FTNREF3D(i,j,k,ip+1,jp+1,0,0,0)];

float cov1_i_j_k;
float cov1_im_j_k;
float cov1_imp1_j_k;
float cov2_i_0_k;
float cov2_i_1_k;
float cov2_i_j_k;
float cov2_i_jm_k;
float cov2_i_jmp1_k;
float cov3_i_j_k;
float cov4_i_j_k;
float cov4_im_j_k;
float cov4_imp1_j_k;
float cov5_i_0_k;
float cov5_i_1_k;
float cov5_i_j_k;
float cov5_i_jm_k;
float cov5_i_jmp1_k;
float cov6_i_j_k;
float cov7_i_j_k;
float cov7_im_j_k;
float cov7_imp1_j_k;
float cov8_i_0_k;
float cov8_i_1_k;
float cov8_i_j_k;
float cov8_i_jm_k;
float cov8_i_jmp1_k;
float cov9_i_j_k;

float diu1_i_j_k;
float diu1_im_j_k;
float diu1_imp1_j_k;
float diu2_i_0_k;
float diu2_i_1_k;
float diu2_i_j_k;
float diu2_i_jm_k;
float diu2_i_jmp1_k;
float diu2_im_j_k;
float diu2_imp1_j_k;
float diu3_i_j_k;
float diu3_im_j_k;
float diu3_imp1_j_k;
float diu4_i_0_k;
float diu4_i_j_k;
float diu4_i_jm_k;
float diu4_im_j_k;
float diu4_imp1_j_k;
float diu5_i_0_k;
float diu5_i_1_k;
float diu5_i_j_k;
float diu5_i_jm_k;
float diu5_i_jmp1_k;
float diu6_i_0_k;
float diu6_i_j_k;
float diu6_i_jm_k;
float diu7_i_j_k;
float diu7_im_j_k;
float diu7_imp1_j_k;
float diu8_i_0_k;
float diu8_i_1_k;
float diu8_i_j_k;
float diu8_i_jm_k;
float diu8_i_jmp1_k;
float diu9_i_j_k;
float nou1_i_j_k;
float nou1_im_j_k;
float nou1_imp1_j_k;
float nou2_i_0_k;
float nou2_i_1_k;
float nou2_i_j_k;
float nou2_i_jm_k;
float nou2_i_jmp1_k;
float nou3_i_j_k;
float nou4_i_j_k;
float nou4_im_j_k;
float nou4_imp1_j_k;
float nou5_i_0_k;
float nou5_i_1_k;
float nou5_i_j_k;
float nou5_i_jm_k;
float nou5_i_jmp1_k;
float nou6_i_j_k;
float nou7_i_j_k;
float nou7_im_j_k;
float nou7_imp1_j_k;
float nou8_i_0_k;
float nou8_i_1_k;
float nou8_i_j_k;
float nou8_i_jm_k;
float nou8_i_jmp1_k;
float nou9_i_j_k;



    if (k<km+1) {
nou1_i_j_k=(uvw[FTNREF3D(i-1,j,k,ip+2,jp+3,0,-1,-1)].s0 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s0 )/2.F;
diu1_i_j_k=(-uvw[FTNREF3D(i-1,j,k,ip+2,jp+3,0,-1,-1)].s0 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s0 )/dx1[FTNREF1D(i,-1)];
nou5_i_j_k=(uvw[FTNREF3D(i,j-1,k,ip+2,jp+3,0,-1,-1)].s1 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s1 )/2.F;
diu5_i_j_k=(-uvw[FTNREF3D(i,j-1,k,ip+2,jp+3,0,-1,-1)].s1 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s1 )/dy1[FTNREF1D(j,0)];
nou9_i_j_k=(uvw[FTNREF3D(i,j,k-1,ip+2,jp+3,0,-1,-1)].s2 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s2 )/2.F;
diu9_i_j_k=(-uvw[FTNREF3D(i,j,k-1,ip+2,jp+3,0,-1,-1)].s2 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s2 )/dzn[FTNREF1D(k,-1)];
//!
cov1_i_j_k=nou1_i_j_k*diu1_i_j_k;
cov5_i_j_k=nou5_i_j_k*diu5_i_j_k;
cov9_i_j_k=nou9_i_j_k*diu9_i_j_k;

nou2_i_j_k=(dx1[FTNREF1D(i+1,-1)]*uvw[FTNREF3D(i,j-1,k,ip+2,jp+3,0,-1,-1)].s1 +dx1[FTNREF1D(i,-1)]*uvw[FTNREF3D(i+1,j-1,k,ip+2,jp+3,0,-1,-1)].s1 )/(dx1[FTNREF1D(i,-1)]+dx1[FTNREF1D(i+1,-1)]);
diu2_i_j_k=2.F*(-uvw[FTNREF3D(i,j-1,k,ip+2,jp+3,0,-1,-1)].s0 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s0 )/(dy1[FTNREF1D(j-1,0)]+dy1[FTNREF1D(j,0)]);
cov2_i_j_k=nou2_i_j_k*diu2_i_j_k;

nou4_i_j_k=(dy1[FTNREF1D(j+1,0)]*uvw[FTNREF3D(i-1,j,k,ip+2,jp+3,0,-1,-1)].s0 +dy1[FTNREF1D(j,0)]*uvw[FTNREF3D(i-1,j+1,k,ip+2,jp+3,0,-1,-1)].s0 )/(dy1[FTNREF1D(j,0)]+dy1[FTNREF1D(j+1,0)]);
diu4_i_j_k=2.F*(-uvw[FTNREF3D(i-1,j,k,ip+2,jp+3,0,-1,-1)].s1 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s1 )/(dx1[FTNREF1D(i-1,-1)]+dx1[FTNREF1D(i,-1)]);
cov4_i_j_k=(nou4_i_j_k-u0)*diu4_i_j_k;

//nou1_imp1_j_k=nou1_im_j_k;
//diu1_imp1_j_k=diu1_im_j_k;
//cov1_imp1_j_k=cov1_im_j_k;
//
//nou2_i_0_k=nou2_i_jm_k;
//diu2_i_0_k=diu2_i_jm_k;
//cov2_i_0_k=cov2_i_jm_k;
//nou2_i_jmp1_k=nou2_i_1_k;
//diu2_i_jmp1_k=diu2_i_1_k;
//cov2_i_jmp1_k=cov2_i_1_k;
//
//nou4_imp1_j_k=nou4_im_j_k;
//diu4_imp1_j_k=diu4_im_j_k;
//cov4_imp1_j_k=cov4_im_j_k;

//nou5_i_0_k=nou5_i_jm_k;
//diu5_i_0_k=diu5_i_jm_k;
//cov5_i_0_k=cov5_i_jm_k;
//nou5_i_jmp1_k=nou5_i_1_k;
//diu5_i_jmp1_k=diu5_i_1_k;
//cov5_i_jmp1_k=cov5_i_1_k;
    }

nou3_i_j_k=(dx1[FTNREF1D(i+1,-1)]*uvw[FTNREF3D(i,j,k-1,ip+2,jp+3,0,-1,-1)].s2 +dx1[FTNREF1D(i,-1)]*uvw[FTNREF3D(i+1,j,k-1,ip+2,jp+3,0,-1,-1)].s2 )/(dx1[FTNREF1D(i,-1)]+dx1[FTNREF1D(i+1,-1)]);
diu3_i_j_k=(-uvw[FTNREF3D(i,j,k-1,ip+2,jp+3,0,-1,-1)].s0 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s0 )/dzs[FTNREF1D(k-1,-1)];
cov3_i_j_k=nou3_i_j_k*diu3_i_j_k;

nou6_i_j_k=(dy1[FTNREF1D(j+1,0)]*uvw[FTNREF3D(i,j,k-1,ip+2,jp+3,0,-1,-1)].s2 +dy1[FTNREF1D(j,0)]*uvw[FTNREF3D(i,j+1,k-1,ip+2,jp+3,0,-1,-1)].s2 )/(dy1[FTNREF1D(j,0)]+dy1[FTNREF1D(j+1,0)]);
diu6_i_j_k=(-uvw[FTNREF3D(i,j,k-1,ip+2,jp+3,0,-1,-1)].s1 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s1 )/dzs[FTNREF1D(k-1,-1)];
cov6_i_j_k=nou6_i_j_k*diu6_i_j_k;
//!
    if (k<km) {
nou7_i_j_k=(dzn[FTNREF1D(k+1,-1)]*uvw[FTNREF3D(i-1,j,k,ip+2,jp+3,0,-1,-1)].s0 +dzn[FTNREF1D(k,-1)]*uvw[FTNREF3D(i-1,j,k+1,ip+2,jp+3,0,-1,-1)].s0 )/(dzn[FTNREF1D(k,-1)]+dzn[FTNREF1D(k+1,-1)]);
diu7_i_j_k=2.F*(-uvw[FTNREF3D(i-1,j,k,ip+2,jp+3,0,-1,-1)].s2 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s2 )/(dx1[FTNREF1D(i-1,-1)]+dx1[FTNREF1D(i,-1)]);
cov7_i_j_k=(nou7_i_j_k-u0)*diu7_i_j_k;
//!
nou8_i_j_k=(dzn[FTNREF1D(k+1,-1)]*uvw[FTNREF3D(i,j-1,k,ip+2,jp+3,0,-1,-1)].s1 +dzn[FTNREF1D(k,-1)]*uvw[FTNREF3D(i,j-1,k+1,ip+2,jp+3,0,-1,-1)].s1 )/(dzn[FTNREF1D(k,-1)]+dzn[FTNREF1D(k+1,-1)]);
diu8_i_j_k=2.F*(-uvw[FTNREF3D(i,j-1,k,ip+2,jp+3,0,-1,-1)].s2 +uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)].s2 )/(dy1[FTNREF1D(j-1,0)]+dy1[FTNREF1D(j,0)]);
cov8_i_j_k=nou8_i_j_k*diu8_i_j_k;
//
//nou7_imp1_j_k=nou7_im_j_k;
//diu7_imp1_j_k=diu7_im_j_k;
//cov7_imp1_j_k=cov7_im_j_k;

//nou8_i_0_k=nou8_i_jm_k;
//diu8_i_0_k=diu8_i_jm_k;
//cov8_i_0_k=cov8_i_jm_k;
//nou8_i_jmp1_k=nou8_i_1_k;
//diu8_i_jmp1_k=diu8_i_1_k;
//cov8_i_jmp1_k=cov8_i_1_k;
}

//diu2_imp1_j_k=diu2_im_j_k;
//diu3_imp1_j_k=diu3_im_j_k;

//diu4_i_0_k=diu4_i_jm_k;
//diu6_i_0_k=diu6_i_jm_k;


diu[FTNREF3D(i,j,k,ip+4,jp+3,-1,0,0)]=(diu1_i_j_k,diu2_i_j_k,diu3_i_j_k,diu4_i_j_k,diu5_i_j_k,diu6_i_j_k,diu7_i_j_k,diu8_i_j_k,diu9_i_j_k,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F,0.0F);

if (i==im) {
    diu[im+1,j,k]=diu[i,j,k];
}
if (j == jm) {
    diu[i,0,k]=diu[i,j,k];
}

if (j==1) {
    diu[i,jm+1,k]=diu[i,j,k];
}

//!--uvelocity
// So here we have a problem because we don't have the values for i+1/j+1/k+1 !!!
// I think the best solution is to introduce a cov* array after all.
// Let's bite the bullet.
// Then we compute vel2 as a separate kernel
float cov1_ip1_j_k;
float cov2_i_jp1_k;
float cov3_i_j_kp1;
float cov4_ip1_j_k;
float cov5_i_jp1_k;
float cov6_i_j_kp1;
float cov7_ip1_j_k;
float cov8_i_jp1_k;
float cov9_i_j_kp1;

float diu1_ip1_j_k=diu[FTNREF3D(i+1,j,k,ip+4,jp+3,-1,0,0)].s0;
float diu2_i_jp1_k=diu[FTNREF3D(i,j+1,k,ip+4,jp+3,-1,0,0)].s1;
float diu3_i_j_kp1=diu[FTNREF3D(i,j,k+1,ip+4,jp+3,-1,0,0)].s2;
float diu4_ip1_j_k=diu[FTNREF3D(i+1,j,k,ip+4,jp+3,-1,0,0)].s3;
float diu5_i_jp1_k=diu[FTNREF3D(i,j+1,k,ip+4,jp+3,-1,0,0)].s4;
float diu6_i_j_kp1=diu[FTNREF3D(i,j,k+1,ip+4,jp+3,-1,0,0)].s5;
float diu7_ip1_j_k=diu[FTNREF3D(i+1,j,k,ip+4,jp+3,-1,0,0)].s6;
float diu8_i_jp1_k=diu[FTNREF3D(i,j+1,k,ip+4,jp+3,-1,0,0)].s7;
float diu9_i_j_kp1=diu[FTNREF3D(i,j,k+1,ip+4,jp+3,-1,0,0)].s8;


float covx1=(dx1[FTNREF1D(i+1,-1)]*cov1_i_j_k+dx1[FTNREF1D(i,-1)]*cov1_ip1_j_k)/(dx1[FTNREF1D(i,-1)]+dx1[FTNREF1D(i+1,-1)]);
float covy1=(cov2_i_j_k+cov2_i_jp1_k)/2.F;
float covz1=(cov3_i_j_k+cov3_i_j_kp1)/2.F;
float covc=covx1+covy1+covz1;
float dfu1_ijk=2.F*(-diu1_i_j_k+diu1_ip1_j_k)/(dx1[FTNREF1D(i,-1)]+dx1[FTNREF1D(i+1,-1)])+(-diu2_i_j_k+diu2_i_jp1_k)/dy1[FTNREF1D(j,0)]+(-diu3_i_j_k+diu3_i_j_kp1)/dzn[FTNREF1D(k,-1)];
float df=vn*dfu1_ijk;
fgh[FTNREF3D(i,j,k,ip+1,jp+1,0,0,0)].s0=(-covc+df);
//!=======================================
//!--vvelocity
covx1=(cov4_i_j_k+cov4_ip1_j_k)/2.F;
covy1=(dy1[FTNREF1D(j+1,0)]*cov5_i_j_k+dy1[FTNREF1D(j,0)]*cov5_i_jp1_k)/(dy1[FTNREF1D(j,0)]+dy1[FTNREF1D(j+1,0)]);
covz1=(cov6_i_j_k+cov6_i_j_kp1)/2.F;
covc=covx1+covy1+covz1;
float dfv1_ijk=(-diu4_i_j_k+diu4_ip1_j_k)/dx1[FTNREF1D(i,-1)]+2.F*(-diu5_i_j_k+diu5_i_jp1_k)/(dy1[FTNREF1D(j,0)]+dy1[FTNREF1D(j+1,0)])+(-diu6_i_j_k+diu6_i_j_kp1)/dzn[FTNREF1D(k,-1)];
float dg=vn*dfv1_ijk;
fgh[FTNREF3D(i,j,k,ip+1,jp+1,0,0,0)].s1=(-covc+dg);
//!
//!=======================================
//!--wvelocity
if(k<km) {
covx1=(cov7_i_j_k+cov7_ip1_j_k)/2.F;
covy1=(cov8_i_j_k+cov8_i_jp1_k)/2.F;
covz1=(dzn[FTNREF1D(k+1,-1)]*cov9_i_j_k+dzn[FTNREF1D(k,-1)]*cov9_i_j_kp1)/(dzn[FTNREF1D(k,-1)]+dzn[FTNREF1D(k+1,-1)]);
covc=covx1+covy1+covz1;
float dfw1_ijk=(-diu7_i_j_k+diu7_ip1_j_k)/dx1[FTNREF1D(i,-1)]+(-diu8_i_j_k+diu8_i_jp1_k)/dy1[FTNREF1D(j,0)]+(-diu9_i_j_k+diu9_i_j_kp1)/dzs[FTNREF1D(k,-1)];
float dh=vn*dfw1_ijk;
fgh[FTNREF3D(i,j,k,ip+1,jp+1,0,0,0)].s2=(-covc+dh);
}

} // guard

#endif
//	if (gl_id == 0) {
//	for (unsigned int idx = 0;idx<im*jm*km;idx++) {
	if (idx<im*jm*km) {
    int4 ijk=calc_loop_iters(idx,im,jm,km,1,1,1); // so all ranges 1..im, 1..jm, 1..km, and we go to ip+1 for dx1, OK

    int j = ijk.s1;
    int k = ijk.s2;
    int i = ijk.s0;

#ifndef NEW_VELFG
// VELFG

    float16 cov_ijk, cov_ijk_p1, diu_ijk, diu_ijk_p1;
    float4 fgh_ijk;
    vel2(
            uvw, // __global float4*
            diu, // __global float16*
            &cov_ijk, &cov_ijk_p1, // float16;
            &diu_ijk, &diu_ijk_p1, // float16;
            dzs, dx1, dy1, dzn, // __global float*
            im, jm, km, // const unsigned int
            i,j,k
        );

      float covx1 = (dx1[i+2]*cov_ijk.s0+dx1[i+1]*cov_ijk_p1.s0) /(dx1[i+1]+dx1[i+2]);
      float covy1 = (cov_ijk.s1+cov_ijk_p1.s1)/2.0F;
      float covz1 = (cov_ijk.s2+cov_ijk_p1.s2)/2.0F;
      float covc = covx1+covy1+covz1;
      float dfu1_ijk = 2.0F*(-diu_ijk.s0+diu_ijk_p1.s0)/(dx1[i+1]+dx1[i+2]) + (-diu_ijk.s1+diu_ijk_p1.s1)/dy1[j] +   (-diu_ijk.s2+diu_ijk_p1.s2)/dzn[k+1];
      float df = vn*dfu1_ijk;
      fgh_ijk.s0 = (-covc+df);

      covx1 = (cov_ijk.s3+cov_ijk_p1.s3)/2.0F;
      covy1 = (dx1[j+1]*cov_ijk.s4+dx1[j]*cov_ijk_p1.s4) /(dx1[j]+dx1[j+1]);
      covz1 = (cov_ijk.s5+cov_ijk_p1.s5)/2.0F;
      covc = covx1+covy1+covz1;
      float dfv1_ijk = (-diu_ijk.s3+diu_ijk_p1.s3)/dx1[i+1] +2.0F*(-diu_ijk.s4+diu_ijk_p1.s4)/(dy1[j]+dy1[j+1]) +(-diu_ijk.s5+diu_ijk_p1.s5)/dzn[k+1];
      float dg = vn*dfv1_ijk;
      fgh_ijk.s1 = (-covc+dg); // FIXME: could we use fgh[i,j,k] instead?

      // what happens if we would set this for k=km? Apparently, nothing!
//      if (k!=km) {
          covx1 = (cov_ijk.s6+cov_ijk_p1.s6)/2.0F;
          covy1 = (cov_ijk.s7+cov_ijk_p1.s7)/2.0F;
          covz1 = (dzn[k+1+1]*cov_ijk.s8+dzn[k+1]*cov_ijk_p1.s8) /(dzn[k+1]+dzn[k+1+1]);
          covc = covx1+covy1+covz1;
          float dfw1_ijk= (-diu_ijk.s6+diu_ijk_p1.s6)/dx1[i+1] +(-diu_ijk.s7+diu_ijk_p1.s7)/dy1[j] +(-diu_ijk.s8+diu_ijk_p1.s8)/dzs[k+1];
          float dh = vn*dfw1_ijk;
          fgh_ijk.s2 = (-covc+dh);
//      }
//          fgh[FTNREF3Du0(i,j,k,im,jm) ]= fgh_ijk;
#endif
// FEEDBF

  	  	float4 uvw_ijk = uvw[FTNREF3D(i,j,k,ip+2,jp+3,0,-1,-1)]; // 0:ip+1,-1:jp+1,-1:kp+1 => ip+2, jp+3, 0,-1,-1
  	    float4 uvwsum_ijk = uvwsum[FTNREF3D0(i,j,k,ip+1,jp+1)]; //  save time! RISKY!
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
  diu_ijk=diu[FTNREF3D(i,j,k,ip+4,jp+3,-1,0,0)];
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
} // END of velfg__feedbf__les_calc_sm_kernel

#ifndef NEW_VELFG
// This is the unified loop of vel2, this replaces the call to the original vel2. I guess I can wrap it in the vel2 function.
void vel2(
    __global float4 * uvw,
    __global float16 * diu,
	float16* cov_ijk,
	float16* cov_ijk_p1,
	float16* diu_ijk,
	float16* diu_ijk_p1,
    __global float * dzs,
    __global float * dx1,
    __global float * dy1,
    __global float * dzn,
    const unsigned int im,
    const unsigned int jm,
    const unsigned int km,
    int i, int j, int k
	) {
	const unsigned int u0 = 0;
// add calc of i,j,k from idx. get idx with ocl call
    const unsigned int ip = im;
    const unsigned int jp = jm;
	const unsigned int kp = km;

	float4 uvw_i_j_k     =uvw[FTNREF3D(i,j,k,ip+2     ,jp+3,0,-1,-1)];
	float4 uvw_im1_j_k   =uvw[FTNREF3D(i-1,j,k,ip+2   ,jp+3,0,-1,-1)];
	float4 uvw_i_jm1_k   =uvw[FTNREF3D(i,j-1,k,ip+2   ,jp+3,0,-1,-1)];
	float4 uvw_i_j_km1   =uvw[FTNREF3D(i,j,k-1,ip+2   ,jp+3,0,-1,-1)];
	float4 uvw_ip1_j_k   =uvw[FTNREF3D(i+1,j,k,ip+2   ,jp+3,0,-1,-1)];
	float4 uvw_i_jp1_k   =uvw[FTNREF3D(i,j+1,k,ip+2   ,jp+3,0,-1,-1)];
	float4 uvw_i_0_k     =uvw[FTNREF3D(i,0,k,ip+2     ,jp+3,0,-1,-1)];
	float4 uvw_i_1_k     =uvw[FTNREF3D(i,1,k,ip+2     ,jp+3,0,-1,-1)];
	float4 uvw_i_j_kp1   =uvw[FTNREF3D(i,j,k+1,ip+2   ,jp+3,0,-1,-1)];

	float4 uvw_i_jm_k    =uvw[FTNREF3D(i,jm,k,ip+2    ,jp+3,0,-1,-1)];
	float4 uvw_im_j_k    =uvw[FTNREF3D(im,j,k,ip+2    ,jp+3,0,-1,-1)];
	float4 uvw_imm1_j_k  =uvw[FTNREF3D(im-1,j,k,ip+2  ,jp+3,0,-1,-1)];
	float4 uvw_i_jmm1_k  =uvw[FTNREF3D(i,jm-1,k,ip+2  ,jp+3,0,-1,-1)];
	float4 uvw_ip1_jmm1_k=uvw[FTNREF3D(i+1,jm-1,k,ip+2,jp+3,0,-1,-1)];
    float4 uvw_i_jmm1_kp1=uvw[FTNREF3D(i,jm-1,k+1,ip+2,jp+3,0,-1,-1)];

    float4 uvw_im1_j_kp1 =uvw[FTNREF3D(i-1,j,k+1,ip+2 ,jp+3,0,-1,-1)];
    float4 uvw_im1_jp1_k =uvw[FTNREF3D(i-1,j+1,k,ip+2 ,jp+3,0,-1,-1)];
    float4 uvw_ip1_jm1_k =uvw[FTNREF3D(i+1,j-1,k,ip+2 ,jp+3,0,-1,-1)];
    float4 uvw_i_jm1_k1  =uvw[FTNREF3D(i,j-1,k+1,ip+2 ,jp+3,0,-1,-1)];
    float4 uvw_ip1_0_k   =uvw[FTNREF3D(i+1,0,k,ip+2   ,jp+3,0,-1,-1)];
    float4 uvw_i_jp1_km1 =uvw[FTNREF3D(i,j+1,k-1,ip+2 ,jp+3,0,-1,-1)];
    float4 uvw_ip1_j_km1 =uvw[FTNREF3D(i+1,j,k-1,ip+2 ,jp+3,0,-1,-1)];


//    float4 uvw_i_j_k=uvw[FTNREF3Du(i,j,k,ip+1,jp+1,0,-1,-1)];
    float u_i_j_k=uvw_i_j_k.s0;
    float v_i_j_k=uvw_i_j_k.s1;
    float w_i_j_k=uvw_i_j_k.s2;

//    float4 uvw_im1_j_k=uvw[FTNREF3Du(i-1,j,k,ip+1,jp+1,0,-1,-1)];
    float u_im1_j_k= uvw_im1_j_k.s0;
    float v_im1_j_k= uvw_im1_j_k.s1;
    float w_im1_j_k= uvw_im1_j_k.s2;

//    float4 uvw_i_jm1_k=uvw[FTNREF3Du(i,j-1,k,ip+1,jp+1,0,-1,-1)];
    float u_i_jm1_k = uvw_i_jm1_k.s0;
    float v_i_jm1_k = uvw_i_jm1_k.s1;
    float w_i_jm1_k = uvw_i_jm1_k.s2;

//    float4 uvw_i_j_km1=uvw[FTNREF3Du(i,j,k-1,ip+1,jp+1,0,-1,-1)];
    float u_i_j_km1= uvw_i_j_km1.s0;
    float v_i_j_km1= uvw_i_j_km1.s1;
    float w_i_j_km1= uvw_i_j_km1.s2;

//    float4 uvw_ip1_j_k=uvw[FTNREF3Du(i+1,j,k,ip+1,jp+1,0,-1,-1)];
    float u_ip1_j_k= uvw_ip1_j_k.s0;
    float v_ip1_j_k= uvw_ip1_j_k.s1;
    float w_ip1_j_k= uvw_ip1_j_k.s2;

//    float4 uvw_i_jp1_k=uvw[FTNREF3Du(i,j+1,k,ip+1,jp+1,0,-1,-1)];
    float u_i_jp1_k= uvw_i_jp1_k.s0;
    float v_i_jp1_k= uvw_i_jp1_k.s1;
    float w_i_jp1_k= uvw_i_jp1_k.s2;

//    float4 uvw_i_0_k=uvw[FTNREF3Du(i,0,k,ip+1,jp+1,0,-1,-1)];
    float u_i_0_k= uvw_i_0_k.s0;
    float v_i_0_k= uvw_i_0_k.s1;
    float w_i_0_k= uvw_i_0_k.s2;

//    float4 uvw_i_1_k=uvw[FTNREF3Du(i,1,k,ip+1,jp+1,0,-1,-1)];
    float u_i_1_k= uvw_i_1_k.s0;
    float v_i_1_k= uvw_i_1_k.s1;
    float w_i_1_k= uvw_i_1_k.s2;

//    float4 uvw_i_j_kp1=uvw[FTNREF3Du(i,j,k+1,ip+1,jp+1,0,-1,-1)];
    float u_i_j_kp1 = uvw_i_j_kp1.s0;
    float v_i_j_kp1 = uvw_i_j_kp1.s1;
    float w_i_j_kp1 = uvw_i_j_kp1.s2;

//    float4 uvw_i_jm_k=uvw[FTNREF3Du(i,jm,k,ip+1,jp+1,0,-1,-1)];
    float u_i_jm_k = uvw_i_jm_k.s0;
    float v_i_jm_k = uvw_i_jm_k.s1;
    float w_i_jm_k = uvw_i_jm_k.s2;

//    float4 uvw_im_j_k=uvw[FTNREF3Du(im,j,k,ip+1,jp+1,0,-1,-1)];
    float u_im_j_k = uvw_im_j_k.s0;
//    float4 uvw_imm1_j_k=uvw[FTNREF3Du(im-1,j,k,ip+1,jp+1,0,-1,-1)];
    float u_imm1_j_k = uvw_imm1_j_k.s0;


//    float4 uvw_i_jmm1_k=uvw[FTNREF3Du(i,jm-1,k,ip+1,jp+1,0,-1,-1)];
    float u_i_jmm1_k = uvw_i_jmm1_k.s0;
    float v_i_jmm1_k = uvw_i_jmm1_k.s1;
    float w_i_jmm1_k = uvw_i_jmm1_k.s2;

//    float4 uvw_ip1_jmm1_k=uvw[FTNREF3Du(i+1,jm-1,k,ip+1,jp+1,0,-1,-1)];
    float v_ip1_jmm1_k = uvw_ip1_jmm1_k.s1;

//    float4 uvw_i_jmm1_kp1=uvw[FTNREF3Du(i,jm-1,k+1,ip+1,jp+1,0,-1,-1)];
    float v_i_jmm1_kp1 = uvw_i_jmm1_kp1.s1;

//    float4 uvw_im1_j_kp1=uvw[FTNREF3Du(i-1,j,k+1,ip+1,jp+1,0,-1,-1)];
    float u_im1_j_kp1 = uvw_im1_j_kp1.s0;
//    float4 uvw_im1_jp1_k=uvw[FTNREF3Du(i-1,j+1,k,ip+1,jp+1,0,-1,-1)];
    float u_im1_jp1_k = uvw_im1_jp1_k.s0;

//    float4 uvw_ip1_jm1_k=uvw[FTNREF3Du(i+1,j-1,k,ip+1,jp+1,0,-1,-1)];
    float v_ip1_jm1_k = uvw_ip1_jm1_k.s1;
//    float4 uvw_i_jm1_k1=uvw[FTNREF3Du(i,j-1,k+1,ip+1,jp+1,0,-1,-1)];
    float v_i_jm1_kp1 = uvw_i_jm1_k1.s1;
//    float4 uvw_ip1_0_k=uvw[FTNREF3Du(i+1,0,k,ip+1,jp+1,0,-1,-1)];
    float v_ip1_0_k = uvw_ip1_0_k.s1;

//    float4 uvw_i_jp1_km1=uvw[FTNREF3Du(i,j+1,k-1,ip+1,jp+1,0,-1,-1)];
    float w_i_jp1_km1 = uvw_i_jp1_km1.s2;
//    float4 uvw_ip1_j_km1=uvw[FTNREF3Du(i+1,j,k-1,ip+1,jp+1,0,-1,-1)];
    float w_ip1_j_km1 = uvw_ip1_j_km1.s2;

   // dx1 has a lb of -1, so we have dx1(i) => dx1[i- (-1)] = dx1[i+1]
   // dy1 has a lb of 0, so we have dy1(i) => dy1[i]
   // dzs has a lb of -1, so we have dzs(i) => dzs[i+1]
   // dzn has a lb of -1, so we have dzn(i) => dzn[i+1]

    float nou1_i_j_k = (u_im1_j_k+u_i_j_k)/2.0F;
    float nou1_ip1_j_k = (i==im) ? (u_im1_j_k+u_i_j_k)/2.0F : (u_i_j_k+u_ip1_j_k)/2.0F;
    float diu1_i_j_k =  (-u_im1_j_k + u_i_j_k)/dx1[i+1];
    float diu1_ip1_j_k = (i==im) ? diu1_i_j_k : (-u_i_j_k+u_ip1_j_k)/dx1[i+2];
    float cov1_i_j_k = nou1_i_j_k*diu1_i_j_k;
    float cov1_ip1_j_k = nou1_ip1_j_k*diu1_ip1_j_k;

    float nou2_i_j_k = (j==0) ? (dx1[i+2]*v_i_jmm1_k+dx1[i+1]*v_ip1_jmm1_k) /(dx1[i+1]+dx1[i+2]) :  (dx1[i+2]*v_i_jm1_k+dx1[i+1]*v_ip1_jm1_k) /(dx1[i+1]+dx1[i+2]);
    float nou2_i_jp1_k = (j==jm) ? (dx1[i+2]*v_i_0_k+dx1[i+1]*v_ip1_0_k) /(dx1[i+1]+dx1[i]) : (dx1[i]*v_i_j_k+dx1[i+1]*v_ip1_j_k) /(dx1[i+1]+dx1[i+2]);
    float diu2_i_j_k = (j==0) ? 2.0F*(-u_i_jmm1_k+u_i_jm_k)/(dy1[j-1]+dy1[j]) : 2.0F*(-u_i_jm1_k+u_i_j_k)/(dy1[j-1]+dy1[j]);
    float diu2_i_jp1_k = (j==jm) ? 2.0F*(-u_i_0_k+u_i_1_k)/(dy1[j-1]+dy1[j]) :  2.0F*(-u_i_j_k+u_i_jp1_k)/(dy1[j-1]+dy1[j]);
    float cov2_i_j_k = nou2_i_j_k*diu2_i_j_k;
    float cov2_i_jp1_k = nou2_i_jp1_k*diu2_i_jp1_k;

    float nou4_i_j_k = (dy1[j+1]*u_im1_j_k+dy1[j]*u_im1_jp1_k) /(dy1[j]+dy1[j+1]);
    float nou4_ip1_j_k = (i==im) ? nou4_i_j_k : (dy1[j+1]*u_i_j_k+dy1[j]*u_i_jp1_k) /(dy1[j]+dy1[j+1]);
    float diu4_i_j_k = 2.0F*(-v_im1_j_k+v_i_j_k)/(dx1[i]+dx1[i+1]);
    float diu4_ip1_j_k = (i==im) ? diu4_i_j_k : 2.0F*(-v_i_j_k+v_ip1_j_k)/(dx1[i]+dx1[i+1]);
    float cov4_i_j_k = (nou4_i_j_k-u0)*diu4_i_j_k;
    float cov4_ip1_j_k = (nou4_ip1_j_k-u0)*diu4_ip1_j_k;

    float nou5_i_j_k = (j==0) ? (v_i_jmm1_k+v_i_jm_k)/2.0F : (v_i_jm1_k+v_i_j_k)/2.0F;
    float nou5_i_jp1_k = (j==jm) ? (v_i_0_k+v_i_1_k)/2.0F : (v_i_j_k+v_i_jp1_k)/2.0F;
    float diu5_i_j_k = (j==0) ? (-v_i_jmm1_k+v_i_jm_k)/dy1[j] : (-v_i_jm1_k+v_i_j_k)/dy1[j];
    float diu5_i_jp1_k = (j==jm) ?  (-v_i_0_k+v_i_1_k)/dy1[1] : (-v_i_j_k+v_i_jp1_k)/dy1[j+1];
    float cov5_i_j_k = nou5_i_j_k*diu5_i_j_k;
    float cov5_i_jp1_k = nou5_i_jp1_k*diu5_i_jp1_k;

    float nou9_i_j_k = (w_i_j_km1+w_i_j_k)/2.0F;
    float nou9_i_j_kp1 = (w_i_j_k+w_i_j_kp1)/2.0F;
    float diu9_i_j_k = (-w_i_j_km1+w_i_j_k)/dzn[k+1];
    float diu9_i_j_kp1 = (-w_i_j_k+w_i_j_kp1)/dzn[k+2];
    float cov9_i_j_k = nou9_i_j_k*diu9_i_j_k;
    float cov9_i_j_kp1 = nou9_i_j_kp1*diu9_i_j_kp1;

    float nou3_i_j_k = (dx1[i+2]*w_i_j_km1+dx1[i+1]*w_ip1_j_km1) /(dx1[i+1]+dx1[i+2]);
    float nou3_i_j_kp1 = (dx1[i+2]*w_i_j_k+dx1[i+1]*w_ip1_j_k) /(dx1[i+1]+dx1[i+2]);
    float diu3_i_j_k = (-u_i_j_km1+u_i_j_k)/dzs[k];
    float diu3_i_j_kp1 = (-u_i_j_k+u_i_j_kp1)/dzs[k+1];
    float cov3_i_j_k = nou3_i_j_k*diu3_i_j_k;
    float cov3_i_j_kp1 = nou3_i_j_kp1*diu3_i_j_kp1;

    float nou6_i_j_k = (dy1[j+1]*w_i_j_km1+dy1[j]*w_i_jp1_km1) /(dy1[j]+dy1[j+1]);
    float nou6_i_j_kp1 = (dy1[j+1]*w_i_j_k+dy1[j]*w_i_jp1_k) /(dy1[j]+dy1[j+1]);
    float diu6_i_j_k = (-v_i_j_km1+v_i_j_k)/dzs[k];
    float diu6_i_j_kp1 = (-v_i_j_k+v_i_j_kp1)/dzs[k+1];
    float cov6_i_j_k = nou6_i_j_k*diu6_i_j_k;
    float cov6_i_j_kp1 = nou6_i_j_kp1*diu6_i_j_kp1;

    //      if (k<km) { // need this shield? What happens if I do it for k==km?

    float nou7_i_j_k = (dzn[k+2]*u_im1_j_k+dzn[k+1]*u_im1_j_kp1) /(dzn[k+1]+dzn[k+2]);
    float nou7_ip1_j_k = (i==im) ? nou7_i_j_k : (dzn[k+2]*u_i_j_k+dzn[k+1]*u_i_j_kp1) /(dzn[k+1]+dzn[k+2]);
    float diu7_i_j_k = 2.0F*(-w_im1_j_k+w_i_j_k)/(dx1[i]+dx1[i+1]);
    float diu7_ip1_j_k = (i==im) ? diu7_i_j_k  : 2.0F*(-w_i_j_k+w_ip1_j_k)/(dx1[i+1]+dx1[i+2]);
    float cov7_i_j_k = (nou7_i_j_k-u0)*diu7_i_j_k;
    float cov7_ip1_j_k = (nou7_ip1_j_k-u0)*diu7_ip1_j_k;

    float nou8_i_j_k = (j==0) ? (dzn[k+2]*v_i_jmm1_k+dzn[k+1]*v_i_jmm1_kp1) /(dzn[k+1]+dzn[k+2]) : (dzn[k+2]*v_i_jm1_k+dzn[k+1]*v_i_jm1_kp1) /(dzn[k+1]+dzn[k+2]);
    float nou8_i_jp1_k = (j==jm) ? : (dzn[k+2]*v_i_j_k+dzn[k+1]*v_i_j_kp1) /(dzn[k+1]+dzn[k+2]);
    float diu8_i_j_k = (j==0) ? 2.0F*(-w_i_jmm1_k+w_i_jm_k)/(dy1[jm-1]+dy1[jm]) : 2.0F*(-w_i_jm1_k+w_i_j_k)/(dy1[j-1]+dy1[j]);
    float diu8_i_jp1_k =(j==jm) ?  2.0F*(-w_i_0_k+w_i_1_k)/(dy1[0]+dy1[1]): 2.0F*(-w_i_j_k+w_i_jp1_k)/(dy1[j]+dy1[j+1]);
    float cov8_i_j_k = nou8_i_j_k*diu8_i_j_k;
    float cov8_i_jp1_k = nou8_i_jp1_k*diu8_i_jp1_k;
    //      }

    diu[ FTNREF3Du(i,j,k,ip+2,jp+2,-1,0,0)] = (float16)(diu1_i_j_k,diu2_i_j_k,diu3_i_j_k,diu4_i_j_k, diu5_i_j_k, diu6_i_j_k, diu7_i_j_k, diu8_i_j_k, diu9_i_j_k, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f);

// I think I can safely say

    diu[ FTNREF3Du(im+1,j,k,      ip+2,jp+2,-1,0,0) ] = diu[ FTNREF3Du(im,j,k,     ip+2,jp+2,-1,0,0) ] ;
    diu[ FTNREF3Du(im+1,jm+1,k,   ip+2,jp+2,-1,0,0) ] = diu[ FTNREF3Du(im,jm+1,k,  ip+2,jp+2,-1,0,0) ] ;
    diu[ FTNREF3Du(im+1,j,km+1,   ip+2,jp+2,-1,0,0)] = diu[ FTNREF3Du(im,j,km+1,   ip+2,jp+2,-1,0,0)] ;
    diu[ FTNREF3Du(im+1,jm+1,km+1,ip+2,jp+2,-1,0,0)] = diu[ FTNREF3Du(im,jm+1,km+1,ip+2,jp+2,-1,0,0)] ;
// And in similar fashion
    diu[ FTNREF3Du(i,0,k,ip+2,jp+2,-1,0,0)] = diu[ FTNREF3Du(i,jm,k,ip+2,jp+2,-1,0,0)] ;
    diu[ FTNREF3Du(i,0,km+1,ip+2,jp+2,-1,0,0)] = diu[ FTNREF3Du(i,jm,km+1,ip+2,jp+2,-1,0,0)] ;
    diu[ FTNREF3Du(im+1,0,k,ip+2,jp+2,-1,0,0)] = diu[ FTNREF3Du(im+1,jm,km+1,ip+2,jp+2,-1,0,0)] ;
    diu[ FTNREF3Du(im+1,0,km+1,ip+2,jp+2,-1,0,0)] = diu[ FTNREF3Du(im+1,jm,km+1,ip+2,jp+2,-1,0,0)] ;


	*cov_ijk =(float16)(cov1_i_j_k,cov2_i_j_k,cov3_i_j_k,cov4_i_j_k,cov5_i_j_k,cov6_i_j_k,cov7_i_j_k,cov8_i_j_k,cov9_i_j_k,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f);
	*cov_ijk_p1 =(float16)(cov1_ip1_j_k,cov2_i_jp1_k,cov3_i_j_kp1,cov4_ip1_j_k,cov5_i_jp1_k,cov6_i_j_kp1,cov7_ip1_j_k,cov8_i_jp1_k,cov9_i_j_kp1,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f);
	*diu_ijk =(float16)(diu1_i_j_k,diu2_i_j_k,diu3_i_j_k,diu4_i_j_k,diu5_i_j_k,diu6_i_j_k,diu7_i_j_k,diu8_i_j_k,diu9_i_j_k,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f);
	*diu_ijk_p1 =(float16)(diu1_ip1_j_k,diu2_i_jp1_k,diu3_i_j_kp1,diu4_ip1_j_k,diu5_i_jp1_k,diu6_i_j_kp1,diu7_ip1_j_k,diu8_i_jp1_k,diu9_i_j_kp1,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f,0.0f);

}

#endif

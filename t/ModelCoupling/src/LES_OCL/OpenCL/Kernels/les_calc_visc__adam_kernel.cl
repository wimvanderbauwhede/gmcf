#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"


__SUBKERNEL void les_calc_visc__adam_kernel (
		__global float4 *fgh,
		__global float4 *fgh_old,
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

	int idx = get_global_id(0);
	if (idx<im*jm*km) {
	int4 ijk = calc_loop_iters(idx,im,jm,km,1,1,1);
	int j = ijk.s1;
	int k = ijk.s2;
	int i = ijk.s0;

	float4 fgh_ijk = fgh[FTNREF3D0(i,j,k,ip+1,jp+1)];
	float16 diu_ijk = diu[FTNREF3D(i,j,k,ip+4,jp+3,-1,0,0)];
//#define CACHE_SM
// manual caching of sm, does not improve performance
#ifndef CACHE_SM
	//! --calculation of viscosity terms in momentum eq.(x-comp.)
	//! --eddyviscosity on face
	float evsx2 = sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)];
	float evsx1 = sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)];
	float evsy2 = (dy1[FTNREF1D(j + 1,0)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dy1[FTNREF1D(j,0)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j + 1,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j + 1,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]);
	float evsy1 = (dy1[FTNREF1D(j + 1,0)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j - 1,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j - 1,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dy1[FTNREF1D(j,0)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]);
	float evsz2 = (dzn[FTNREF1D(k + 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k + 1,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k + 1,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	float evsz1 = (dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k - 1,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k - 1,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k - 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k - 1,-1)] + dzn[FTNREF1D(k,-1)]);

	float visux2 = (evsx2) * 2.F * diu[FTNREF3D(i+1,j,k,ip+4,jp+3,-1,0,0)].s0;
	float visux1 = (evsx1) * 2.F * diu[FTNREF3D(i,j,k,  ip+4,jp+3,-1,0,0)].s0;
	float visuy2 = (evsy2) * ( diu[FTNREF3D(i,j + 1,k,  ip+4,jp+3,-1,0,0)].s1 + diu[FTNREF3D(i + 1,j,k,    ip+4,jp+3,-1,0,0)].s3);
	float visuy1 = (evsy1) * ( diu[FTNREF3D(i,j,k,      ip+4,jp+3,-1,0,0)].s1 + diu[FTNREF3D(i + 1,j - 1,k,ip+4,jp+3,-1,0,0)].s3);
	float visuz2 = (evsz2) * ( diu[FTNREF3D(i,j,k + 1,  ip+4,jp+3,-1,0,0)].s2 + diu[FTNREF3D(i + 1,j,k,    ip+4,jp+3,-1,0,0)].s6);
	float visuz1 = (evsz1) * ( diu[FTNREF3D(i,j,k,      ip+4,jp+3,-1,0,0)].s2 + diu[FTNREF3D(i + 1,j,k - 1,ip+4,jp+3,-1,0,0)].s6);

	float vfu = (visux2 - visux1) / dx1[FTNREF1D(i,-1)] + (visuy2 - visuy1) / dy1[FTNREF1D(j,0)] + (visuz2 - visuz1) / dzn[FTNREF1D(k,-1)];

	fgh_ijk.s0 = (fgh_ijk.s0 + vfu);

	//! --calculation of viscosity terms in momentum eq.(y-comp.)

	//! --eddyviscosity on face
	evsy2 = sm[FTNREF3D(i,j + 1,k,ip+3,jp+3,-1,-1,0)];
	evsy1 = sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)];
	evsx2 = (dy1[FTNREF1D(j + 1,0)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dy1[FTNREF1D(j,0)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j + 1,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j + 1,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]);
	evsx1 = (dy1[FTNREF1D(j + 1,0)] * ((dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i - 1,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i - 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i - 1,-1)] + dx1[FTNREF1D(i,-1)])) + dy1[FTNREF1D(j,0)] * ((dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i - 1,j + 1,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i - 1,-1)] * sm[FTNREF3D(i,j + 1,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i - 1,-1)] + dx1[FTNREF1D(i,-1)]))) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]);
	evsz2 = (dzn[FTNREF1D(k + 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k + 1,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k + 1,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	evsz1 = (dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k - 1,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k - 1,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k - 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k - 1,-1)] + dzn[FTNREF1D(k,-1)]);

	float visvx2 = (evsx2) * (diu[FTNREF3D(i,j + 1,k,     ip+4,jp+3,-1,0,0)].s1 + diu[FTNREF3D(i + 1,j,k,ip+4,jp+3,-1,0,0)].s3);
	float visvx1 = (evsx1) * (diu[FTNREF3D(i - 1,j + 1,k, ip+4,jp+3,-1,0,0)].s1 + diu[FTNREF3D(i,j,k,    ip+4,jp+3,-1,0,0)].s3);
	float visvy2 = (evsy2) * 2.F * diu[FTNREF3D(i,j + 1,k,ip+4,jp+3,-1,0,0)].s4;
	float visvy1 = (evsy1) * 2.F * diu[FTNREF3D(i,j,k,    ip+4,jp+3,-1,0,0)].s4;
	float visvz2 = (evsz2) * (diu[FTNREF3D(i,j,k + 1,     ip+4,jp+3,-1,0,0)].s5 + diu[FTNREF3D(i,j + 1,k,    ip+4,jp+3,-1,0,0)].s7);
	float visvz1 = (evsz1) * (diu[FTNREF3D(i,j,k,         ip+4,jp+3,-1,0,0)].s5 + diu[FTNREF3D(i,j + 1,k - 1,ip+4,jp+3,-1,0,0)].s7);

	float vfv = (visvx2 - visvx1) / dx1[FTNREF1D(i,-1)] + (visvy2 - visvy1) / dy1[FTNREF1D(j,0)] + (visvz2 - visvz1) / dzn[FTNREF1D(k,-1)];

	fgh_ijk.s1 = (fgh_ijk.s1 + vfv);


	//! --calculation of viscosity terms in momentum eq.(z-comp.)

		//! --eddyviscosity on face
	evsz2 = sm[FTNREF3D(i,j,k + 1,ip+3,jp+3,-1,-1,0)];
	evsz1 = sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)];
	evsx2 = (dzn[FTNREF1D(k + 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * sm[FTNREF3D(i,j,k + 1,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i + 1,j,k + 1,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	evsx1 = (dzn[FTNREF1D(k + 1,-1)] * ((dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i - 1,j,k,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i - 1,-1)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i - 1,-1)] + dx1[FTNREF1D(i,-1)])) + dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i,-1)] * sm[FTNREF3D(i - 1,j,k + 1,ip+3,jp+3,-1,-1,0)] + dx1[FTNREF1D(i - 1,-1)] * sm[FTNREF3D(i,j,k + 1,ip+3,jp+3,-1,-1,0)]) / (dx1[FTNREF1D(i - 1,-1)] + dx1[FTNREF1D(i,-1)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	evsy2 = (dzn[FTNREF1D(k + 1,-1)] * ((dy1[FTNREF1D(j + 1,0)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)] + dy1[FTNREF1D(j,0)] * sm[FTNREF3D(i,j + 1,k,ip+3,jp+3,-1,-1,0)]) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)])) + dzn[FTNREF1D(k,-1)] * ((dy1[FTNREF1D(j + 1,0)] * sm[FTNREF3D(i,j,k + 1,ip+3,jp+3,-1,-1,0)] + dy1[FTNREF1D(j,0)] * sm[FTNREF3D(i,j + 1,k + 1,ip+3,jp+3,-1,-1,0)]) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	evsy1 = (dzn[FTNREF1D(k + 1,-1)] * ((dy1[FTNREF1D(j,0)] * sm[FTNREF3D(i,j - 1,k,ip+3,jp+3,-1,-1,0)] + dy1[FTNREF1D(j - 1,0)] * sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)]) / (dy1[FTNREF1D(j - 1,0)] + dy1[FTNREF1D(j,0)])) + dzn[FTNREF1D(k,-1)] * ((dy1[FTNREF1D(j,0)] * sm[FTNREF3D(i,j - 1,k + 1,ip+3,jp+3,-1,-1,0)] + dy1[FTNREF1D(j - 1,0)] * sm[FTNREF3D(i,j,k + 1,ip+3,jp+3,-1,-1,0)]) / (dy1[FTNREF1D(j - 1,0)] + dy1[FTNREF1D(j,0)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);

	float viswx2 = (evsx2) * (diu[FTNREF3D(i,j,k + 1,     ip+4,jp+3,-1,0,0)].s2 + diu[FTNREF3D(i + 1,j,k,ip+4,jp+3,-1,0,0)].s6);
	float viswx1 = (evsx1) * (diu[FTNREF3D(i - 1,j,k + 1, ip+4,jp+3,-1,0,0)].s2 + diu[FTNREF3D(i,j,k,    ip+4,jp+3,-1,0,0)].s6);
	float viswy2 = (evsy2) * (diu[FTNREF3D(i,j,k + 1,     ip+4,jp+3,-1,0,0)].s5 + diu[FTNREF3D(i,j + 1,k,ip+4,jp+3,-1,0,0)].s7);
	float viswy1 = (evsy1) * (diu[FTNREF3D(i,j - 1,k + 1, ip+4,jp+3,-1,0,0)].s5 + diu[FTNREF3D(i,j,k,    ip+4,jp+3,-1,0,0)].s7);
	float viswz2 = (evsz2) * 2.F * diu[FTNREF3D(i,j,k + 1,ip+4,jp+3,-1,0,0)].s8;
	float viswz1 = (evsz1) * 2.F * diu[FTNREF3D(i,j,k,    ip+4,jp+3,-1,0,0)].s8;


	float vfw = (viswx2 - viswx1) / dx1[FTNREF1D(i,-1)] + (viswy2 - viswy1) / dy1[FTNREF1D(j,0)] + (viswz2 - viswz1) / dzn[FTNREF1D(k,-1)];

	fgh_ijk.s2 = (fgh_ijk.s2 + vfw);

#else
	// manual caching of sm, does not improve performance on Mac
	float evs_ijk = sm[FTNREF3D(i,j,k,ip+3,jp+3,-1,-1,0)];
	float evs_ip1jk = sm[FTNREF3D(i + 1,j,k,ip+3,jp+3,-1,-1,0)];
	float evs_ijp1k = sm[FTNREF3D(i,j + 1,k,ip+3,jp+3,-1,-1,0)];
	float evs_ijkp1 = sm[FTNREF3D(i,j,k + 1,ip+3,jp+3,-1,-1,0)];

	float evs_im1jk = sm[FTNREF3D(i - 1,j,k,ip+3,jp+3,-1,-1,0)];
	float evs_ijm1k = sm[FTNREF3D(i,j - 1,k,ip+3,jp+3,-1,-1,0)];
	float evs_ijkm1 = sm[FTNREF3D(i,j,k - 1,ip+3,jp+3,-1,-1,0)];

	float evs_ip1jp1k = sm[FTNREF3D(i + 1,j + 1,k,ip+3,jp+3,-1,-1,0)];
	float evs_ip1jm1k = sm[FTNREF3D(i + 1,j - 1,k,ip+3,jp+3,-1,-1,0)];
	float evs_ip1jkp1 =	 sm[FTNREF3D(i + 1,j,k + 1,ip+3,jp+3,-1,-1,0)];
	float evs_ip1jkm1 =	 sm[FTNREF3D(i + 1,j,k - 1,ip+3,jp+3,-1,-1,0)];
    float evs_im1jp1k = sm[FTNREF3D(i - 1,j + 1,k,ip+3,jp+3,-1,-1,0)];

    float evs_im1jkp1 = sm[FTNREF3D(i - 1,j,k + 1,ip+3,jp+3,-1,-1,0)];
    float evs_ijp1kp1 = sm[FTNREF3D(i,j + 1,k + 1,ip+3,jp+3,-1,-1,0)];
    float evs_ijm1kp1 = sm[FTNREF3D(i,j - 1,k + 1,ip+3,jp+3,-1,-1,0)];

	//! --calculation of viscosity terms in momentum eq.(x-comp.)
	//! --eddyviscosity on face
	float evsx2 = evs_ip1jk;
	float evsx1 = evs_ijk;
	float evsy2 = (dy1[FTNREF1D(j + 1,0)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijk + dx1[FTNREF1D(i,-1)] * evs_ip1jk) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dy1[FTNREF1D(j,0)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijp1k + dx1[FTNREF1D(i,-1)] * evs_ip1jp1k) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]);
	float evsy1 = (dy1[FTNREF1D(j + 1,0)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijm1k + dx1[FTNREF1D(i,-1)] * evs_ip1jm1k) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dy1[FTNREF1D(j,0)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijk + dx1[FTNREF1D(i,-1)] * evs_ip1jk) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]);
	float evsz2 = (dzn[FTNREF1D(k + 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijk + dx1[FTNREF1D(i,-1)] * evs_ip1jk) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijkp1 + dx1[FTNREF1D(i,-1)] * evs_ip1jkp1) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	float evsz1 = (dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijkm1 + dx1[FTNREF1D(i,-1)] * evs_ip1jkm1) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k - 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijk + dx1[FTNREF1D(i,-1)] * evs_ip1jk) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k - 1,-1)] + dzn[FTNREF1D(k,-1)]);

	float visux2 = evsx2 * 2.F * diu[FTNREF3D(i+1,j,k,ip+4,jp+3,-1,0,0)].s0;
	float visux1 = evsx1 * 2.F * diu_ijk.s0;
	float visuy2 = evsy2 * ( diu[FTNREF3D(i,j + 1,k,  ip+4,jp+3,-1,0,0)].s1 + diu[FTNREF3D(i + 1,j,k,    ip+4,jp+3,-1,0,0)].s3);
	float visuy1 = evsy1 * ( diu_ijk.s1 + diu[FTNREF3D(i + 1,j - 1,k,ip+4,jp+3,-1,0,0)].s3);
	float visuz2 = evsz2 * ( diu[FTNREF3D(i,j,k + 1,  ip+4,jp+3,-1,0,0)].s2 + diu[FTNREF3D(i + 1,j,k,    ip+4,jp+3,-1,0,0)].s6);
	float visuz1 = evsz1 * ( diu_ijk.s2 + diu[FTNREF3D(i + 1,j,k - 1,ip+4,jp+3,-1,0,0)].s6);


	float vfu = (visux2 - visux1) / dx1[FTNREF1D(i,-1)] + (visuy2 - visuy1) / dy1[FTNREF1D(j,0)] + (visuz2 - visuz1) / dzn[FTNREF1D(k,-1)];

	fgh_ijk.s0 = (fgh_ijk.s0 + vfu);

	//! --calculation of viscosity terms in momentum eq.(y-comp.)

	//! --eddyviscosity on face
	evsy2 = evs_ijp1k;
	evsy1 = evs_ijk;
	evsx2 = (dy1[FTNREF1D(j + 1,0)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijk + dx1[FTNREF1D(i,-1)] * evs_ip1jk) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dy1[FTNREF1D(j,0)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijp1k + dx1[FTNREF1D(i,-1)] * evs_ip1jp1k) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]);
	evsx1 = (dy1[FTNREF1D(j + 1,0)] * ((dx1[FTNREF1D(i,-1)] * evs_im1jk + dx1[FTNREF1D(i - 1,-1)] * evs_ijk) / (dx1[FTNREF1D(i - 1,-1)] + dx1[FTNREF1D(i,-1)])) + dy1[FTNREF1D(j,0)] * ((dx1[FTNREF1D(i,-1)] * evs_im1jp1k + dx1[FTNREF1D(i - 1,-1)] * evs_ijp1k) / (dx1[FTNREF1D(i - 1,-1)] + dx1[FTNREF1D(i,-1)]))) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]);
	evsz2 = (dzn[FTNREF1D(k + 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijk + dx1[FTNREF1D(i,-1)] * evs_ip1jk) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijkp1 + dx1[FTNREF1D(i,-1)] * evs_ip1jkp1) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	evsz1 = (dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijkm1 + dx1[FTNREF1D(i,-1)] * evs_ip1jkm1) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k - 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijk + dx1[FTNREF1D(i,-1)] * evs_ip1jk) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k - 1,-1)] + dzn[FTNREF1D(k,-1)]);

	float visvx2 = (evsx2) * (diu[FTNREF3D(i,j + 1,k,     ip+4,jp+3,-1,0,0)].s1 + diu[FTNREF3D(i + 1,j,k,ip+4,jp+3,-1,0,0)].s3);
	float visvx1 = (evsx1) * (diu[FTNREF3D(i - 1,j + 1,k, ip+4,jp+3,-1,0,0)].s1 + diu_ijk.s3);
	float visvy2 = (evsy2) * 2.F * diu[FTNREF3D(i,j + 1,k,ip+4,jp+3,-1,0,0)].s4;
	float visvy1 = (evsy1) * 2.F * diu_ijk.s4;
	float visvz2 = (evsz2) * (diu[FTNREF3D(i,j,k + 1,     ip+4,jp+3,-1,0,0)].s5 + diu[FTNREF3D(i,j + 1,k,    ip+4,jp+3,-1,0,0)].s7);
	float visvz1 = (evsz1) * (diu_ijk.s5 + diu[FTNREF3D(i,j + 1,k - 1,ip+4,jp+3,-1,0,0)].s7);

	float vfv = (visvx2 - visvx1) / dx1[FTNREF1D(i,-1)] + (visvy2 - visvy1) / dy1[FTNREF1D(j,0)] + (visvz2 - visvz1) / dzn[FTNREF1D(k,-1)];

	fgh_ijk.s1 = (fgh_ijk.s1 + vfv);

	//! --calculation of viscosity terms in momentum eq.(z-comp.)

		//! --eddyviscosity on face
	evsz2 = evs_ijkp1;
	evsz1 = evs_ijk;
	evsx2 = (dzn[FTNREF1D(k + 1,-1)] * ((dx1[FTNREF1D(i + 1,-1)] *evs_ijk + dx1[FTNREF1D(i,-1)] * evs_ip1jk) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)])) + dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i + 1,-1)] * evs_ijkp1 + dx1[FTNREF1D(i,-1)] * evs_ip1jkp1) / (dx1[FTNREF1D(i,-1)] + dx1[FTNREF1D(i + 1,-1)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	evsx1 = (dzn[FTNREF1D(k + 1,-1)] * ((dx1[FTNREF1D(i,-1)] * evs_im1jk + dx1[FTNREF1D(i - 1,-1)] * evs_ijk) / (dx1[FTNREF1D(i - 1,-1)] + dx1[FTNREF1D(i,-1)])) + dzn[FTNREF1D(k,-1)] * ((dx1[FTNREF1D(i,-1)] * evs_im1jkp1 + dx1[FTNREF1D(i - 1,-1)] * evs_ijkp1) / (dx1[FTNREF1D(i - 1,-1)] + dx1[FTNREF1D(i,-1)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	evsy2 = (dzn[FTNREF1D(k + 1,-1)] * ((dy1[FTNREF1D(j + 1,0)] * evs_ijk + dy1[FTNREF1D(j,0)] * evs_ijp1k) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)])) + dzn[FTNREF1D(k,-1)] * ((dy1[FTNREF1D(j + 1,0)] * evs_ijkp1 + dy1[FTNREF1D(j,0)] * evs_ijp1kp1) / (dy1[FTNREF1D(j,0)] + dy1[FTNREF1D(j + 1,0)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);
	evsy1 = (dzn[FTNREF1D(k + 1,-1)] * ((dy1[FTNREF1D(j,0)] * evs_ijm1k + dy1[FTNREF1D(j - 1,0)] * evs_ijk) / (dy1[FTNREF1D(j - 1,0)] + dy1[FTNREF1D(j,0)])) + dzn[FTNREF1D(k,-1)] * ((dy1[FTNREF1D(j,0)] * evs_ijm1kp1 + dy1[FTNREF1D(j - 1,0)] * evs_ijkp1) / (dy1[FTNREF1D(j - 1,0)] + dy1[FTNREF1D(j,0)]))) / (dzn[FTNREF1D(k,-1)] + dzn[FTNREF1D(k + 1,-1)]);

	float viswx2 = (evsx2) * (diu[FTNREF3D(i,j,k + 1,     ip+4,jp+3,-1,0,0)].s2 + diu[FTNREF3D(i + 1,j,k,ip+4,jp+3,-1,0,0)].s6);
	float viswx1 = (evsx1) * (diu[FTNREF3D(i - 1,j,k + 1, ip+4,jp+3,-1,0,0)].s2 + diu_ijk.s6);
	float viswy2 = (evsy2) * (diu[FTNREF3D(i,j,k + 1,     ip+4,jp+3,-1,0,0)].s5 + diu[FTNREF3D(i,j + 1,k,ip+4,jp+3,-1,0,0)].s7);
	float viswy1 = (evsy1) * (diu[FTNREF3D(i,j - 1,k + 1, ip+4,jp+3,-1,0,0)].s5 + diu_ijk.s7);
	float viswz2 = (evsz2) * 2.F * diu[FTNREF3D(i,j,k + 1,ip+4,jp+3,-1,0,0)].s8;
	float viswz1 = (evsz1) * 2.F * diu_ijk.s8;

	float vfw = (viswx2 - viswx1) / dx1[FTNREF1D(i,-1)] + (viswy2 - viswy1) / dy1[FTNREF1D(j,0)] + (viswz2 - viswz1) / dzn[FTNREF1D(k,-1)];
	//    if (i == 1 && j == 1 && k == 78) {
		//    printf(" vis C:  %e\t%e\t%e\t%e\t%e\t%e\t%e\n",viswx2,viswx1,viswy2,viswy1,viswz2,viswz1,vfw);
	//    }

	fgh_ijk.s2 = (fgh_ijk.s2 + vfw);
//	fgh[FTNREF3D0(i,j,k,ip+1,jp+1)] = fgh_ijk; // redundant I think
#endif
	// Adam-Bashforth integration

	float4 fgh_old_ijk = fgh_old[FTNREF3D(i,j,k,ip,jp,1,1,1)];
	fgh_old[FTNREF3D(i,j,k,ip,jp,1,1,1)] = fgh_ijk;
//	float4 fgh_tmp_ijk = fgh_ijk;
//	float fd = fgh_ijk.s0;
//	float gd = fgh_ijk.s1;
//	float hd = fgh_ijk.s2;
	fgh_ijk.s0 = 1.5F * fgh_ijk.s0 - 0.5F * fgh_old_ijk.s0;
	fgh_ijk.s1 = 1.5F * fgh_ijk.s1 - 0.5F * fgh_old_ijk.s1;
	fgh_ijk.s2 = 1.5F * fgh_ijk.s2 - 0.5F * fgh_old_ijk.s2;
	fgh[FTNREF3D0(i,j,k,ip+1,jp+1)] = fgh_ijk;
//    fgh_old_ijk.s0 = fd;
//    fgh_old_ijk.s1 = gd;
//    fgh_old_ijk.s2 = hd;
//    fgh_old[FTNREF3D(i,j,k,ip,jp,1,1,1)] = fgh_tmp_ijk;
	}
} // END of les_calc_visc__adam_kernel

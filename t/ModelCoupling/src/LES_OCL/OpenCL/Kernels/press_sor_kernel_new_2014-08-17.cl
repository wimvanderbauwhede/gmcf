#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

//#define OMEGA_NOT_1
//#define FIXED_NTH
//#define APPROX_PAR_SOR
#define TWINNED_DOUBLE_BUFFER
#define BARRIER_OK
//#define ALL_NEW
#define NEW_ITER_ORDER
#ifndef COMBINED_KERNEL

float calc_reltmp0(
        __global float2* p2,
        __global float* rhs,
        float rhsav,
        const __global float *cn1,
        const __global float *cn2l,const __global float *cn2s,
        const __global float *cn3l,
        const __global float *cn3s,
        const __global float *cn4l,
        const __global float *cn4s,
        unsigned int i, unsigned int j, unsigned int k,
        const unsigned int ip,
        const unsigned int jp,
        const unsigned int kp
        ) ;
float calc_reltmp1(
        __global float2* p2,
        __global float* rhs,
        float rhsav,
        const __global float *cn1,
        const __global float *cn2l,const __global float *cn2s,
        const __global float *cn3l,
        const __global float *cn3s,
        const __global float *cn4l,
        const __global float *cn4s,
        unsigned int i, unsigned int j, unsigned int k,
        const unsigned int ip,
        const unsigned int jp,
        const unsigned int kp
        ) ;
float calc_reltmp_mp_rb(
		__global float2* p2,
        __global float* rhs,
        float rhsav,
        const __global float *cn1,const __global float *cn2l,const __global float *cn2s,const __global float *cn3l,const __global float *cn3s,const __global float *cn4l,const __global float *cn4s,
        unsigned int i, unsigned int j, unsigned int k,
        unsigned int j_lhs,unsigned int k_lhs,
        unsigned int nrd,
        const unsigned int ip,
        const unsigned int jp,
        const unsigned int kp
        ) ;
float calc_reltmp_mp_db(
		__global float2* p2,
        __global float* rhs,
        float rhsav,
        const __global float *cn1,const __global float *cn2l,const __global float *cn2s,const __global float *cn3l,const __global float *cn3s,const __global float *cn4l,const __global float *cn4s,
        unsigned int i, unsigned int j, unsigned int k,
        unsigned int j_lhs,unsigned int k_lhs,
        unsigned int nrd,
        const unsigned int ip,
        const unsigned int jp,
        const unsigned int kp
        ) ;
#endif
// ====================================================== KERNEL ===========================================================

__SUBKERNEL void press_sor_kernel(
#ifdef COMBINED_KERNEL
		__local float* sor_chunks,
//		__local float* tmp_chunks,
#endif
		__global float4* uvw,
//		__global float* p,
		__global float2* p2,
		__global float *rhs,
		const __global float *cn1,
		const __global float *cn2l, const __global float *cn2s,
		const __global float *cn3l, const __global float *cn3s,
		const __global float *cn4l, const __global float *cn4s,
		__global float *chunks_num,
		__global float *chunks_denom, __global float *val_ptr,
		__global unsigned int *nrd_ptr,
		const unsigned int im,
		const unsigned int jm,
		const unsigned int km
		) {
	const unsigned int ip = im;
	const unsigned int jp = jm;
	const unsigned int kp = km;




#ifdef ALL_NEW
/*
 So in this version of the kernel, I want to try and have better cache use by computing the reltmp expression in stages
 A lot of work for no gain!
 */
	unsigned int gr_id = get_group_id(0);
	unsigned int l_id = get_local_id(0);
	float rhsav = *val_ptr;

	__local float cn2l_l[256];
	__local float cn2s_l[256];
	__local float cn3l_l[256];
	__local float cn3s_l[256];
	__local float cn4l_l[256];
	__local float cn4s_l[256];
	__local float cn1_jk[256];
	float reltmp_mp[256];
	for (unsigned int ii = 0; ii < ip; ii++) {
		cn2l_l[ii] = cn2l[ii];
		cn2s_l[ii] = cn2s[ii];
	}
	for (unsigned int jj = 0; jj < jp; jj++) {
		cn3l_l[jj] = cn3l[jj];
		cn3s_l[jj] = cn3s[jj];
	}
	for (unsigned int kk = 0; kk < kp; kk++) {
		cn4l_l[kk] = cn4l[kk];
		cn4s_l[kk] = cn4s[kk];
	}

	unsigned int nrd = *nrd_ptr;
	// k = 0 .. km+1
	unsigned int k = gr_id;
	// j = 0 .. jm+1
	unsigned int j = l_id;
	unsigned int k_lhs = k;
	bool calc_sor_k = true;
	if (k == 0) {
		k = 1;
		calc_sor_k = false;
	}
	if (k == km + 1) {
		k = km;
		calc_sor_k = false;
	}
	// What we do is separate the lhs index from the rhs index
	// then we re-assign the rhs index to meet the boundary condition
	unsigned int j_lhs = j;
	bool calc_sor_j = true;
	if (j == 0) {
		j = jm;
		calc_sor_j = false;
	}
	if (j == jm + 1) {
		j = 1;
		calc_sor_j = false;
	}
	for (unsigned int ii = 1; ii <= ip; ii++) {
		cn1_jk[ii-1] = cn1[FTNREF3D(ii, j, k, ip, jp, 1, 1, 1)];
	}
    for (unsigned int ii = 0; ii <= ip+1; ii++) {
		reltmp_mp[ii]=0.0f;
	}

	float local_sor = 0.0F;
//	float reltmp_mp = 0.0f;

	for (unsigned int phase = 1; phase <= 8; phase++) {

		for (unsigned int i_lhs = 0; i_lhs <= im + 1; i_lhs++) {
			bool calc_sor_i = true;
			unsigned int i = i_lhs;
			if (i_lhs == 0) {
				i = 1;
				calc_sor_i = false;
			}
			if (i_lhs == im + 1) {
				i = im;
				calc_sor_i = false;
			}

			if (nrd == 0) {

				if (phase == 1) {
					reltmp_mp[i_lhs] += (rhsav - rhs[FTNREF3D0(i, j, k, ip + 2, jp + 2)]) ;
				} else if (phase == 2) {
					reltmp_mp[i_lhs] += cn2l_l[FTNREF1D(i, 1)]
							* p2[FTNREF3D0(i + 1, j, k, ip + 3, jp + 3)].s0 ;
				} else if (phase == 3) {
					reltmp_mp[i_lhs] += cn2s_l[i-1]
							* p2[FTNREF3D0(i - 1, j, k, ip + 3, jp + 3)].s0 ;
				} else if (phase == 4) {
					reltmp_mp[i_lhs] += cn3l_l[j-1]
							* p2[FTNREF3D0(i, j + 1, k, ip + 3, jp + 3)].s0 ;
				} else if (phase == 5) {
					reltmp_mp[i_lhs] += cn3s_l[j-1]
							* p2[FTNREF3D0(i, j - 1, k, ip + 3, jp + 3)].s0 ;
				} else if (phase == 6) {
					reltmp_mp[i_lhs] += cn4l_l[k-1]
							* p2[FTNREF3D0(i, j, k + 1, ip + 3, jp + 3)].s0 ;
				} else if (phase == 7) {
					reltmp_mp[i_lhs] += cn4s_l[k-1]
							* p2[FTNREF3D0(i, j, k - 1, ip + 3, jp + 3)].s0 ;
				} else if (phase == 8) {
					reltmp_mp[i_lhs] *= cn1_jk[i-1];
					float reltmp = reltmp_mp[i_lhs]
							- p2[FTNREF3D0(i, j, k, ip + 3, jp + 3)].s0;
					p2[FTNREF3D0(i, j_lhs, k_lhs, ip + 3, jp + 3)].s1 =
							reltmp_mp[i_lhs];
					if (calc_sor_i && calc_sor_j && calc_sor_k) {
						local_sor += reltmp * reltmp;
					}
				}

			} else { // nrd == 1

				if (phase == 1) {
					reltmp_mp[i_lhs] += (rhsav - rhs[FTNREF3D0(i, j, k, ip + 2, jp + 2)]) ;
				} else if (phase == 2) {
					reltmp_mp[i_lhs] += cn2l_l[i-1]
							* p2[FTNREF3D0(i + 1, j, k, ip + 3, jp + 3)].s1 ;
				} else if (phase == 3) {
					reltmp_mp[i_lhs] += cn2s_l[i-1]
							* p2[FTNREF3D0(i - 1, j, k, ip + 3, jp + 3)].s1 ;
				} else if (phase == 4) {
					reltmp_mp[i_lhs] += cn3l_l[j-1]
							* p2[FTNREF3D0(i, j + 1, k, ip + 3, jp + 3)].s1 ;
				} else if (phase == 5) {
					reltmp_mp[i_lhs] += cn3s_l[j-1]
							* p2[FTNREF3D0(i, j - 1, k, ip + 3, jp + 3)].s1 ;
				} else if (phase == 6) {
					reltmp_mp[i_lhs] += cn4l_l[k-1]
							* p2[FTNREF3D0(i, j, k + 1, ip + 3, jp + 3)].s1 ;
				} else if (phase == 7) {
					reltmp_mp[i_lhs] += cn4s_l[k-1]
							* p2[FTNREF3D0(i, j, k - 1, ip + 3, jp + 3)].s1 ;
				} else if (phase == 8) {
					reltmp_mp[i_lhs] *= cn1_jk[i-1];
					float reltmp = reltmp_mp[i_lhs]
							- p2[FTNREF3D0(i, j, k, ip + 3, jp + 3)].s1;
					//reltmp_mp = 0.0f;//DEBUG
					//reltmp = 1.0f;//DEBUG
					p2[FTNREF3D0(i, j_lhs, k_lhs, ip + 3, jp + 3)].s0 =
							reltmp_mp[i_lhs];
					if (calc_sor_i && calc_sor_j && calc_sor_k) {
						local_sor += reltmp * reltmp;
					}
				}

			} // nrd

		} // i

	} // phase
	sor_chunks[l_id] = local_sor;
	barrier(CLK_LOCAL_MEM_FENCE);
//if (l_id == 0) {
	float local_sor_acc = 0.0F; // chunks_num[gr_id];
// we should skip the two outer chunks as they are for the boundary conditions
	for (unsigned int s = 1; s < jm + 1; s++) {
		local_sor_acc += sor_chunks[s];
	}

	chunks_num[gr_id] = local_sor_acc;
//}

#else

	// ------------------------------------------------------------------------------------------------------------
#ifndef COMBINED_KERNEL
#ifdef FIXED_NTH
		__local float sor_chunks[NTH];
//		__local float tmp_chunks[NTH];
#else
		__local float sor_chunks[N_ELTS_TH];
//		__local float tmp_chunks[N_ELTS_TH];
#endif
#endif
//		unsigned int gl_id = get_global_id(0);
		unsigned int gr_id = get_group_id(0);
#ifdef FIXED_NTH
		unsigned int nunits = get_num_groups(0);
#endif
		unsigned int l_id = get_local_id(0);

		float rhsav = *val_ptr;

#ifdef APPROX_PAR_SOR
		const float pjuge=0.0001F;
#ifndef P_SCRATCH
		const int nmaxp=50;
#else
		const int nmaxp=25;
#endif
#endif

#ifdef TWINNED_DOUBLE_BUFFER

		float local_sor_acc = 0.0F;
#ifndef APPROX_PAR_SOR
		unsigned int nrd = *nrd_ptr;
#else
		chunks_denom[gr_id]=nmaxp;
		for (unsigned int iter=1;iter<=nmaxp;iter++) {
//			local_sor_acc = 0.0F;
			for (unsigned int nrd=0;nrd<=1;nrd++) {
#endif        
				float local_sor = 0.0F;
#ifndef FIXED_NTH
				// k = 0 .. km+1
			   unsigned int k = gr_id;
			   // j = 0 .. jm+1
#ifndef NEW_ITER_ORDER
			   unsigned int j = l_id;
#else
			   unsigned int i = l_id;
#endif
#else
				unsigned int lj_bound =
						((jm + 2) % NTH == 0) ? (jm + 2) / NTH : (jm + 2) / NTH + 1;

				unsigned int chunk_sz = (kp + 2) / nunits;
				unsigned int kl_start = gr_id * chunk_sz;
				unsigned int kl_stop =
						(gr_id < nunits - 1) ? (gr_id + 1) * chunk_sz : (kp + 2);

				for (unsigned int kl = kl_start; kl < kl_stop; kl++) {
					unsigned int k = kl;
#endif
					unsigned int k_lhs = k;

					bool calc_sor_k = true;
					if (k == 0) {
						k = 1;
						calc_sor_k = false;
					}
					if (k == km + 1) {
						k = km;
						calc_sor_k = false;
					}

#ifdef FIXED_NTH
					for (unsigned int lj = 0; lj < lj_bound; lj++) {
						unsigned int j = 0 + lj * NTH + l_id;
						if (j <= jm + 1) {
#endif
							// What we do is separate the lhs index from the rhs index
							// then we re-assign the rhs index to meet the boundary condition
#ifndef NEW_ITER_ORDER
							unsigned int j_lhs = j;
							bool calc_sor_j = true;
							if (j == 0) {
								j = jm;
								calc_sor_j = false;
							}
							if (j == jm + 1) {
								j = 1;
								calc_sor_j = false;
							}

							for (unsigned int i_lhs = 0; i_lhs <= im + 1; i_lhs++) {
								bool calc_sor_i = true ;
								unsigned int i = i_lhs;
								if (i_lhs == 0) {
									i = 1;
									calc_sor_i = false;
								}
								if (i_lhs == im + 1) {
									i = im;
									calc_sor_i = false;
								}
#else
								unsigned int i_lhs = i;
								bool calc_sor_i = true;
								if (i == 0) {
									i = 1;
									calc_sor_i = false;
								}
								if (i == im + 1) {
									i = im;
									calc_sor_i = false;
								}

								for (unsigned int j_lhs = 0; j_lhs <= jm + 1; j_lhs++) {
									bool calc_sor_j = true ;
									unsigned int j = j_lhs;
									if (j_lhs == 0) {
										j = jm;
										calc_sor_j = false;
									}
									if (j_lhs == jm + 1) {
										j = 1;
										calc_sor_j = false;
									}
#endif
								// So the idea is that this call will calculate the new boundary conditions for p or p2
								float reltmp_mp = calc_reltmp_mp_db(p2, rhs, rhsav,
										cn1, cn2l, cn2s, cn3l, cn3s, cn4l, cn4s, i, j, k,
										j_lhs, k_lhs, nrd, ip, jp, kp);
								if (calc_sor_i && calc_sor_j && calc_sor_k) {
									local_sor += reltmp_mp * reltmp_mp;
								}
							}
#ifdef FIXED_NTH
						} // if j<=jm+1
					} // for lj
				} // for lk
#endif
				sor_chunks[l_id] = local_sor;
#ifdef BARRIER_OK        
		barrier(CLK_LOCAL_MEM_FENCE);
#endif        
			if (l_id == 0) {
				local_sor_acc = 0.0F; // chunks_num[gr_id];
				// we should skip the two outer chunks as they are for the boundary conditions
#ifdef FIXED_NTH
				for (unsigned int s = 0; s < NTH; s++) {
					local_sor_acc += sor_chunks[s];
				}
#else
				for (unsigned int s = 1; s < jm+1; s++) {
					local_sor_acc += sor_chunks[s];
				}

#endif
			}
#ifdef APPROX_PAR_SOR
		} // nrd
//		if ( local_sor_acc < pjuge ) {
//			chunks_denom[gr_id]=iter;
//			break;
//		}
	} // iter
#endif // APPROX_PAR_SOR
	if (l_id == 0) {
		chunks_num[gr_id] =  local_sor_acc;
	}

#endif // TWINNED_DOUBLE_BUFFER

// ------------------------------------------------------------------------------------------------------------
// Below is the more conventional kernel, with red-black and copied bounds

#ifndef TWINNED_DOUBLE_BUFFER

#ifndef APPROX_PAR_SOR
	unsigned int nrd = *nrd_ptr;
#else
		chunks_denom[gr_id]=nmaxp;
		for (unsigned int iter=1;iter<=nmaxp;iter++) {
			for (unsigned int nrd=0;nrd<=2;nrd++) {
#endif


	if (nrd<2) {
		float local_sor = 0.0F;
#ifndef FIXED_NTH
		// k = 1 .. km (range = km)
		unsigned int k = gr_id+1;
		unsigned int j = l_id+1;
		unsigned int nth = jm;

#else

		unsigned int range = kp;
		unsigned int chunk_sz = range / nunits;
		unsigned int kl_start= gr_id*chunk_sz;
		unsigned int kl_stop= (gr_id < nunits-1) ? (gr_id+1)* chunk_sz : range;
		unsigned int lj_bound = ( jm % NTH == 0) ? jm/NTH : jm/NTH+1;
#endif

#if LOOP_ORDER == 6
		 // kji
#ifdef FIXED_NTH
		for (unsigned int kl = kl_start; kl<kl_stop; kl++) {
			unsigned int k = kl+1;
#endif
#ifdef FIXED_NTH
			for (unsigned int lj=0;lj<lj_bound;lj++ ) {
				unsigned int j = 1 +lj*NTH+l_id;
				if (j<jm+1) {
#endif

					for (unsigned int i=1 + ((k + j + nrd) % 2);i<=im;i+=2) {
						float reltmp_mp = calc_reltmp_mp_rb(p2,rhs,rhsav,cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s,i,j,k,j,k,nrd,ip,jp,kp);
						local_sor += reltmp_mp * reltmp_mp;
					} // loop over i
					  //boundp1
					  //--computational boundary(neumann condition)
					  // adjust i=0 and i=im+1 for all j and k
					p2[FTNREF3D0(0,j,k,ip+3,jp+3)].s0 = p2[FTNREF3D0(1,j,k,ip+3,jp+3)].s0;
					p2[FTNREF3D0(im + 1,j,k,ip+3,jp+3)].s0 = p2[FTNREF3D0(im,j,k,ip+3,jp+3)].s0;
#ifdef FIXED_NTH
				} //if j<jm
			} // for jl

		} // for kl
#endif
#elif LOOP_ORDER == 1
		  // jki
#ifdef FIXED_NTH
			for (unsigned int lj=0;lj<lj_bound;lj++ ) {
				unsigned int j = 1 +lj*NTH+l_id;
				if (j<jm+1) {
					for (unsigned int kl = kl_start; kl<kl_stop; kl++) {
						unsigned int k = kl+1;

#endif
						for (unsigned int i=1 + ((k + j + nrd) % 2);i<=im;i+=2) {
							float reltmp_mp = calc_reltmp_mp_rb(p2,rhs,rhsav,cn1,cn2l,cn2s,cn3l,cn3s,cn4l,cn4s,i,j,k,j,k,nrd,ip,jp,kp);
							local_sor += reltmp_mp * reltmp_mp;
						} // loop over i
					  //boundp1
					  //--computational boundary(neumann condition)
					  // adjust i=0 and i=im+1 for all j and k
						p2[FTNREF3D0(0,j,k,ip+3,jp+3)].s0 = p2[FTNREF3D0(1,j,k,ip+3,jp+3)].s0;
						p2[FTNREF3D0(im + 1,j,k,ip+3,jp+3)].s0 = p2[FTNREF3D0(im,j,k,ip+3,jp+3)].s0;
#ifdef FIXED_NTH
					} // for kl

				} //if j<jm
			} // for jl
#endif

#endif // LOOP_ORDER

		sor_chunks[l_id] = local_sor;
#ifdef BARRIER_OK
		barrier(CLK_LOCAL_MEM_FENCE);
#endif

#ifdef FIXED_NTH
			// boundary conditions for j=0 and j = jm+1, for all i and k
			for (unsigned int kl = kl_start; kl<kl_stop; kl++) {
				unsigned int k = kl+1;
#endif
				for (unsigned int ii = 0; ii <= im+1; ii++) {
					p2[FTNREF3D0(ii,jm+1,k,ip+3,jp+3)].s0 = p2[FTNREF3D0(ii,1,k,ip+3,jp+3)].s0;
					p2[FTNREF3D0(ii,0,k,ip+3,jp+3)].s0 = p2[FTNREF3D0(ii,jm,k,ip+3,jp+3)].s0;
				}
#ifdef FIXED_NTH
			} // kl
#endif
//			if (l_id==0) {
			float local_sor_acc = 0.0F;// chunks_num[gr_id];

#ifdef FIXED_NTH
			for(unsigned int s = 0; s < NTH; s++)
#else
				// changed iteration => km ipv jm
				for(unsigned int s = 0; s < nth; s++)
					// we skip the two outer chunks as they are for the boundary conditions
#endif
			{
				local_sor_acc += sor_chunks[s];
			}
			chunks_num[gr_id] = local_sor_acc;
//		}
	} else { // nrd==2
		//boundp2
		//--computational boundary(neumann condition)
#ifndef FIXED_NTH
    	unsigned int i = gr_id;
    	unsigned int j = l_id;
#else
		unsigned int chunk_sz = (ip+2) / nunits;
		unsigned int il_start= gr_id*chunk_sz;
		unsigned int il_stop= (gr_id < nunits-1) ? (gr_id+1)* chunk_sz : (ip+2);
		for (unsigned int il = il_start; il<il_stop; il++) {
			unsigned int i = il;
			unsigned int lj_bound = ((jm+2) % NTH == 0) ? (jm+2)/NTH : (jm+2)/NTH+1;
			for (unsigned int lj=0;lj<lj_bound;lj++ ) {
				unsigned int j = lj*NTH+l_id;
				if (j<=jm+1) {
#endif

					p2[FTNREF3D0(i,j,0,ip+3,jp+3)].s0 = p2[FTNREF3D0(i,j,1,ip+3,jp+3)].s0;
					p2[FTNREF3D0(i,j,km+1,ip+3,jp+3)].s0 = p2[FTNREF3D0(i,j,km,ip+3,jp+3)].s0;
#ifdef FIXED_NTH
				}
			}
		}
#endif
	} // nrd

#ifdef APPROX_PAR_SOR
		} // nrd
//		if ( local_sor_acc < pjuge ) {
//			chunks_denom[gr_id]=iter;
//			break;
//		}
	} // iter
#endif // APPROX_PAR_SOR

#endif
#endif

} // END of press_sor_kernel()
//================================================================================================================================================================ 
float calc_reltmp0(
		__global float2* p, __global float* rhs, float rhsav,
		const __global float *cn1, const __global float *cn2l, const __global float *cn2s,
		const __global float *cn3l, const __global float *cn3s, const __global float *cn4l,
		const __global float *cn4s,
		unsigned int i, unsigned int j, unsigned int k,
		const unsigned int ip, const unsigned int jp, const unsigned int kp) {


#ifdef OMEGA_NOT_1
	const float omega=1.0F; //1.0F;//

	float reltmp = omega * (cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
					cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)].s0
					+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)].s0
					+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)].s0
					+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)].s0
					+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)].s0
					+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)].s0
					- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
			) - p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0);

#else


//	float reltmp = cn1[FTNREF3D(i, j, k, ip, jp, 1, 1, 1)]
//		   * (
//			  cn2l[FTNREF1D(i, 1)] * p[FTNREF3D0(i + 1, j, k, ip + 3, jp + 3)].s0
//			+ cn2s[FTNREF1D(i, 1)] * p[FTNREF3D0(i - 1, j, k, ip + 3, jp + 3)].s0
//			+ cn3l[FTNREF1D(j, 1)] * p[FTNREF3D0(i, j + 1, k, ip + 3, jp + 3)].s0
//			+ cn3s[FTNREF1D(j, 1)] * p[FTNREF3D0(i, j - 1, k, ip + 3, jp + 3)].s0
//			+ cn4l[FTNREF1D(k, 1)] * p[FTNREF3D0(i, j, k + 1, ip + 3, jp + 3)].s0
//			+ cn4s[FTNREF1D(k, 1)] * p[FTNREF3D0(i, j, k - 1, ip + 3, jp + 3)].s0
//					- (rhs[FTNREF3D0(i, j, k, ip + 2, jp + 2)] - rhsav)
//					);

// This could be better for using mad, if the compiler is too stupid
	float reltmp = rhsav - rhs[FTNREF3D0(i, j, k, ip + 2, jp + 2)];
	reltmp += cn2l[FTNREF1D(i, 1)] * p[FTNREF3D0(i + 1, j, k, ip + 3, jp + 3)].s0;
	reltmp += cn2s[FTNREF1D(i, 1)] * p[FTNREF3D0(i - 1, j, k, ip + 3, jp + 3)].s0;
	reltmp += cn3l[FTNREF1D(j, 1)] * p[FTNREF3D0(i, j + 1, k, ip + 3, jp + 3)].s0;
	reltmp += cn3s[FTNREF1D(j, 1)] * p[FTNREF3D0(i, j - 1, k, ip + 3, jp + 3)].s0;
	reltmp += cn4l[FTNREF1D(k, 1)] * p[FTNREF3D0(i, j, k + 1, ip + 3, jp + 3)].s0;
	reltmp += cn4s[FTNREF1D(k, 1)] * p[FTNREF3D0(i, j, k - 1, ip + 3, jp + 3)].s0;
	reltmp *=  cn1[FTNREF3D(i, j, k, ip, jp, 1, 1, 1)];

#endif
	return reltmp;
}

// ------------------------------------------------------------------------------------------------------------
float calc_reltmp1(
		__global float2* p, __global float* rhs, float rhsav,
		const __global float *cn1, const __global float *cn2l, const __global float *cn2s,
		const __global float *cn3l, const __global float *cn3s, const __global float *cn4l,
		const __global float *cn4s,
		unsigned int i, unsigned int j, unsigned int k,
		const unsigned int ip, const unsigned int jp, const unsigned int kp) {


#ifdef OMEGA_NOT_1
	const float omega=1.0F; //1.0F;//

	float reltmp = omega * (cn1[FTNREF3D(i,j,k,ip,jp,1,1,1)] * (
					cn2l[FTNREF1D(i,1)] * p[FTNREF3D0(i + 1,j,k,ip+3,jp+3)].s1
					+ cn2s[FTNREF1D(i,1)] * p[FTNREF3D0(i - 1,j,k,ip+3,jp+3)].s1
					+ cn3l[FTNREF1D(j,1)] * p[FTNREF3D0(i,j + 1,k,ip+3,jp+3)].s1
					+ cn3s[FTNREF1D(j,1)] * p[FTNREF3D0(i,j - 1,k,ip+3,jp+3)].s1
					+ cn4l[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k + 1,ip+3,jp+3)].s1
					+ cn4s[FTNREF1D(k,1)] * p[FTNREF3D0(i,j,k - 1,ip+3,jp+3)].s1
					- (rhs[FTNREF3D0(i,j,k,ip+2,jp+2)] - rhsav)
			) - p[FTNREF3D0(i,j,k,ip+3,jp+3)].s1);

#else


//	float reltmp = cn1[FTNREF3D(i, j, k, ip, jp, 1, 1, 1)]
//		   * (
//			  cn2l[FTNREF1D(i, 1)] * p[FTNREF3D0(i + 1, j, k, ip + 3, jp + 3)].s1
//			+ cn2s[FTNREF1D(i, 1)] * p[FTNREF3D0(i - 1, j, k, ip + 3, jp + 3)].s1
//			+ cn3l[FTNREF1D(j, 1)] * p[FTNREF3D0(i, j + 1, k, ip + 3, jp + 3)].s1
//			+ cn3s[FTNREF1D(j, 1)] * p[FTNREF3D0(i, j - 1, k, ip + 3, jp + 3)].s1
//			+ cn4l[FTNREF1D(k, 1)] * p[FTNREF3D0(i, j, k + 1, ip + 3, jp + 3)].s1
//			+ cn4s[FTNREF1D(k, 1)] * p[FTNREF3D0(i, j, k - 1, ip + 3, jp + 3)].s1
//					- (rhs[FTNREF3D0(i, j, k, ip + 2, jp + 2)] - rhsav)
//					);


	// This could be better for using mad, if the compiler is too stupid

		float reltmp = rhsav - rhs[FTNREF3D0(i, j, k, ip + 2, jp + 2)];

		reltmp += cn2l[FTNREF1D(i, 1)] * p[FTNREF3D0(i + 1, j, k, ip + 3, jp + 3)].s1;
		reltmp += cn2s[FTNREF1D(i, 1)] * p[FTNREF3D0(i - 1, j, k, ip + 3, jp + 3)].s1;

		reltmp += cn3l[FTNREF1D(j, 1)] * p[FTNREF3D0(i, j + 1, k, ip + 3, jp + 3)].s1;

		reltmp += cn3s[FTNREF1D(j, 1)] * p[FTNREF3D0(i, j - 1, k, ip + 3, jp + 3)].s1;

		reltmp += cn4l[FTNREF1D(k, 1)] * p[FTNREF3D0(i, j, k + 1, ip + 3, jp + 3)].s1;

		reltmp += cn4s[FTNREF1D(k, 1)] * p[FTNREF3D0(i, j, k - 1, ip + 3, jp + 3)].s1;

		reltmp *=  cn1[FTNREF3D(i, j, k, ip, jp, 1, 1, 1)];
#endif                         
	return reltmp;
}

// ------------------------------------------------------------------------------------------------------------
// red-black
float calc_reltmp_mp_rb(
		__global float2* p,
		__global float* rhs, float rhsav, const __global float *cn1,
		const __global float *cn2l, const __global float *cn2s, const __global float *cn3l,
		const __global float *cn3s, const __global float *cn4l, const __global float *cn4s,
		unsigned int i, unsigned int j, unsigned int k, unsigned int j_lhs,
		unsigned int k_lhs, unsigned int nrd, const unsigned int ip,
		const unsigned int jp, const unsigned int kp) {

	float reltmp = 0.0f;
#ifdef OMEGA_NOT_1
	if (nrd == 0) {
		 reltmp = calc_reltmp0(p, rhs, rhsav, cn1, cn2l, cn2s, cn3l, cn3s,
				cn4l, cn4s, i, j, k, ip, jp, kp);
		p[FTNREF3D0(i,j_lhs,k_lhs,ip+3,jp+3)].s0 = p[FTNREF3D0(i, j, k, ip+3,jp+3)].s0 + reltmp;
	} else {
		reltmp = calc_reltmp0(p,nrd, rhs, rhsav, cn1, cn2l, cn2s, cn3l,
				cn3s, cn4l, cn4s, i, j, k, ip, jp, kp);
		p[FTNREF3D0(i, j_lhs, k_lhs, ip+3,jp+3)].s0 = p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0 + reltmp;
	}
#else
	if (nrd == 0) {
		float reltmp_mp = calc_reltmp0( p, rhs, rhsav, cn1, cn2l, cn2s, cn3l, cn3s,
				cn4l, cn4s,

				i, j, k, ip, jp, kp);
		reltmp = reltmp_mp - p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0;
		p[FTNREF3D0(i, j_lhs, k_lhs, ip + 3, jp + 3)].s0 = reltmp_mp;
	} else {
		float reltmp_mp = calc_reltmp0(p, rhs, rhsav, cn1, cn2l, cn2s, cn3l,
				cn3s, cn4l, cn4s,

				i, j, k, ip, jp, kp);
	    reltmp = reltmp_mp - p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0;
		p[FTNREF3D0(i, j_lhs, k_lhs, ip + 3, jp + 3)].s0 = reltmp_mp;
	}
#endif
	return reltmp;
}
// ------------------------------------------------------------------------------------------------------------
// double buffer
float calc_reltmp_mp_db(
		__global float2* p,
		__global float* rhs, float rhsav, const __global float *cn1,
		const __global float *cn2l, const __global float *cn2s, const __global float *cn3l,
		const __global float *cn3s, const __global float *cn4l, const __global float *cn4s,
		unsigned int i, unsigned int j, unsigned int k, unsigned int j_lhs,
		unsigned int k_lhs, unsigned int nrd, const unsigned int ip,
		const unsigned int jp, const unsigned int kp) {

	float reltmp = 0.0f;
#ifdef OMEGA_NOT_1
	if (nrd == 0) {
		 reltmp = calc_reltmp0(p, rhs, rhsav, cn1, cn2l, cn2s, cn3l, cn3s,
				cn4l, cn4s, i, j, k, ip, jp, kp);
		p[FTNREF3D0(i,j_lhs,k_lhs,ip+3,jp+3)].s1 = p[FTNREF3D0(i, j, k, ip+3,jp+3)].s0 + reltmp;
	} else {
		reltmp = calc_reltmp1(p,nrd, rhs, rhsav, cn1, cn2l, cn2s, cn3l,
				cn3s, cn4l, cn4s, i, j, k, ip, jp, kp);
		p[FTNREF3D0(i, j_lhs, k_lhs, ip+3,jp+3)].s0 = p[FTNREF3D0(i,j,k,ip+3,jp+3)].s1 + reltmp;
	}
#else
	if (nrd == 0) {
		float reltmp_mp = calc_reltmp0(p, rhs, rhsav, cn1, cn2l, cn2s, cn3l, cn3s,
				cn4l, cn4s, i, j, k, ip, jp, kp);
		reltmp = reltmp_mp - p[FTNREF3D0(i,j,k,ip+3,jp+3)].s0;
		p[FTNREF3D0(i, j_lhs, k_lhs, ip + 3, jp + 3)].s1 = reltmp_mp;
	} else {
		float reltmp_mp = calc_reltmp1(p, rhs, rhsav, cn1, cn2l, cn2s, cn3l,
				cn3s, cn4l, cn4s, i, j, k, ip, jp, kp);
	    reltmp = reltmp_mp - p[FTNREF3D0(i,j,k,ip+3,jp+3)].s1;
		p[FTNREF3D0(i, j_lhs, k_lhs, ip + 3, jp + 3)].s0 = reltmp_mp;
	}
#endif
	return reltmp;
}


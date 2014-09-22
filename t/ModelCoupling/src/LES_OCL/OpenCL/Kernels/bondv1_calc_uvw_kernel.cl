#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"


__SUBKERNEL void bondv1_calc_uvw_kernel(
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
    const float vn = 15.83F*1E-6F; // FIXME: taken from set.f, should maybe be a macro?
    const float alpha = -10.0F; // FIXME: should be macro
    const float beta = -1.0F; // FIXME: should be macro
    const float cs0 = 0.1F;// FIXME: should be macro
    const float csx1 = cs0;

    const unsigned int ip = im;
    const unsigned int jp = jm;
	const unsigned int kp = km;
	unsigned int idx=get_global_id(0);
// BONDV1_CALC_UVW
	unsigned int gl_id=idx;
    float uout = *uout_ptr;
// 0:ip+1,=> ip+2; -1:jp+1 => jp+3; -1:kp+1 => kp+3  :: uvw
    if (gl_id < (km*jm) ) {
    	unsigned int k = (gl_id / jm) + 1;
    	unsigned int j = (gl_id % jm) + 1;

    		  float4 uvw_im_j_k =  uvw[FTNREF3D(im,j,k,ip+2,jp+3,0,-1,-1)];
    	      float4 uvw_imp1_j_k = uvw[FTNREF3D(im + 1,j,k,ip+2,jp+3,0,-1,-1)] ;
    	      float4 uvw_imm1_j_k = uvw[FTNREF3D(im - 1,j,k,ip+2,jp+3,0,-1,-1)];
    	      float u_im_j_k = uvw_im_j_k.s0;
    	      float v_im_j_k = uvw_im_j_k.s1;
    	      float w_im_j_k = uvw_im_j_k.s2;
    	      float u_imm1_j_k = uvw_imm1_j_k.s0;
    	      float v_imp1_j_k = uvw_imp1_j_k.s1 ;
    	      float w_imp1_j_k = uvw_imp1_j_k.s2;
    	      u_im_j_k   = u_im_j_k   - dt * uout * (u_im_j_k   - u_imm1_j_k) / dxs[im]; // NaN!
    	      v_imp1_j_k = v_imp1_j_k - dt * uout * (v_imp1_j_k - v_im_j_k )  / dxs[im];
    	      w_imp1_j_k = w_imp1_j_k - dt * uout * (w_imp1_j_k - w_im_j_k )  / dxs[im];
    	      // Now reassign to memory
    	      uvw_im_j_k = (float4)(u_im_j_k,v_im_j_k,w_im_j_k,0.0f );
    	      uvw[FTNREF3D(im,j,k,ip+2,jp+3,0,-1,-1)]=uvw_im_j_k; // causes error in uvwsum
    	      uvw_imp1_j_k = (float4)(uvw_imp1_j_k.s0,v_imp1_j_k,w_imp1_j_k,0.0f );
    	      uvw[FTNREF3D(im + 1,j,k,ip+2,jp+3,0,-1,-1)]=uvw_imp1_j_k;

    } else if (gl_id < (km*jm)+ (km+2)*(im+2) ) {
    	//! --side flow condition; periodic
    	unsigned int k = (gl_id - (km*jm)) / (im+2);
    	unsigned int i = (gl_id - (km*jm)) % (im+2);
    	          float4 uvw_i_0_k = uvw[FTNREF3D(i,0,k,ip+2,jp+3,0,-1,-1)];
    	          float4 uvw_i_1_k = uvw[FTNREF3D(i,1,k,ip+2,jp+3,0,-1,-1)];
    	          float4 uvw_i_jm_k = uvw[FTNREF3D(i,jm,k,ip+2,jp+3,0,-1,-1)];
    	          float4 uvw_i_jmp1_k = uvw[FTNREF3D(i,jm+1,k,ip+2,jp+3,0,-1,-1)];
    	          uvw_i_0_k.s0 = uvw_i_jm_k.s0; //
    	          uvw_i_jmp1_k.s0 = uvw_i_1_k.s0; //
    	          uvw_i_0_k.s1 = uvw_i_jm_k.s1; //
    	          uvw_i_jmp1_k.s1 = uvw_i_1_k.s1;//
    	          if (k<km+1) {
    	          uvw_i_0_k.s2 = uvw_i_jm_k.s2; //
    	          uvw_i_jmp1_k.s2 = uvw_i_1_k.s2; //
    	          }
    	          // Now reassign to memory
    	          uvw[FTNREF3D(i,0,k,ip+2,jp+3,0,-1,-1)]=uvw_i_0_k;  // causes main error in uvwsum
    	          uvw[FTNREF3D(i,jm+1,k,ip+2,jp+3,0,-1,-1)]=uvw_i_jmp1_k;  // causes error in uvwsum

    } else if (gl_id <  (km*jm)+ (km+2)*(im+2)+(im+3)*(jm+3)) {
    	// the reason for this condition is that it allows us to pad the range to be a multiple of 32/64 etc
    	//! -------top and underground condition
    	unsigned int j = (gl_id - (km*jm)- (km+2)*(im+2)) / (im+3) - 1;
    	unsigned int i = (gl_id - (km*jm) - (km+2)*(im+2)) % (im+3) - 1;
        float4 uvw_i_j_0 =  uvw[FTNREF3D(i,j,0,ip+2,jp+3,0,-1,-1)];
        float4 uvw_i_j_1 =  uvw[FTNREF3D(i,j,1,ip+2,jp+3,0,-1,-1)];
        float4 uvw_i_j_km = uvw[FTNREF3D(i,j,km,ip+2,jp+3,0,-1,-1)];
        float4 uvw_i_j_kmp1 = uvw[FTNREF3D(i,j,km+1,ip+2,jp+3,0,-1,-1)];
        if ( (j>-1 && j< jm+1) && (i>-1) ) {
        uvw_i_j_0.s0 = -uvw_i_j_1.s0;
        uvw_i_j_kmp1.s0 = uvw_i_j_km.s0;
        uvw_i_j_0.s1 = -uvw_i_j_1.s1;
        uvw_i_j_kmp1.s1 = uvw_i_j_1.s1;
        }
        uvw_i_j_0.s2 = 0.0F;
        uvw_i_j_km.s2 = 0.0F;

        uvw[FTNREF3D(i,j,0,ip+2,jp+3,0,-1,-1)] = uvw_i_j_0;
        uvw[FTNREF3D(i,j,km + 1,ip+2,jp+3,0,-1,-1)] = uvw_i_j_kmp1;
        uvw[FTNREF3D(i,j,km,ip+2,jp+3,0,-1,-1)] = uvw_i_j_km;
    }

} // END of bondv1_calc_uvw_kernel

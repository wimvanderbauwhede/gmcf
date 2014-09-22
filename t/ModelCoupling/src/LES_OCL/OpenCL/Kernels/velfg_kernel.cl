#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"

__SUBKERNEL void velfg_kernel (
	    __global float4 * uvw,
	    __global float4 * fgh,
	    __global float16 * diu,
	    __global float * dzs,
	    __global float * dx1,
	    __global float * dy1,
	    __global float * dzn,
	    __global float * val_ptr,
	    const unsigned int im,
	    const unsigned int jm,
	    const unsigned int km
		) {

		const unsigned int u0 = 0;
	    const float vn = 1.583e-05F; // FIXME: taken from set.f, should maybe be a macro?
	    const float alpha = -10.0F; // FIXME: should be macro
	    const float beta = -1.0F; // FIXME: should be macro
	    const float cs0 = 0.1F;// FIXME: should be macro
	    const float csx1 = cs0;

    const unsigned int ip = im;
    const unsigned int jp = jm;
	const unsigned int kp = km;

		unsigned int idx=get_global_id(0);
		if (idx<(im)*(jm)*(km+1)) {
	    int4 ijk=calc_loop_iters(idx,im,jm,km+1,1,1,1); // so all ranges 1..im, 1..jm, 1..km, and we go to ip+1 for dx1, OK

	     int j = ijk.s1;
	     int k = ijk.s2;
	     int i = ijk.s0;

      float diu1_i_j_k =0.0f ;
      float diu1_ip1_j_k=0.0f;
      float cov1_i_j_k =0.0f;
      float cov1_ip1_j_k=0.0f;

      float diu5_i_j_k=0.0f;
      float diu5_i_jp1_k=0.0f;
      float cov5_i_j_k=0.0f;
      float cov5_i_jp1_k=0.0f;

      float diu9_i_j_k=0.0f;
      float diu9_i_j_kp1=0.0f;
      float cov9_i_j_k=0.0f;
      float cov9_i_j_kp1=0.0f;

      float diu2_i_j_k =0.0f;
      float diu2_i_jp1_k=0.0f;
      float cov2_i_j_k=0.0f;
      float cov2_i_jp1_k=0.0f;

      float diu4_i_j_k=0.0f;
      float diu4_ip1_j_k=0.0f;
      float cov4_i_j_k=0.0f;
      float cov4_ip1_j_k=0.0f;

      float diu7_i_j_k=0.0f;
      float diu7_ip1_j_k=0.0f;
      float cov7_i_j_k=0.0f;
      float cov7_ip1_j_k=0.0f;

      float diu8_i_j_k=0.0f;
      float diu8_i_jp1_k=0.0f;
      float cov8_i_j_k=0.0f;
      float cov8_i_jp1_k=0.0f;

  	    float diu3_i_j_k=0.0f;
  	    float cov3_i_j_k=0.0f;

  	    float diu6_i_j_k=0.0f;
  	    float cov6_i_j_k=0.0f;

      	    float diu3_i_j_kp1=0.0f;
      	    float cov3_i_j_kp1=0.0f;

  	    	float diu6_i_j_kp1=0.0f;
  	        float cov6_i_j_kp1=0.0f;
//  	        if (j>-1 && k>-1) {
//  	      diu[FTNREF3D(i,j,k,ip+4,jp+3,-1,0,0)] = (float16)(0.0f,0.0f,0.0f,0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f);
//  	        }
//	if (i>0 && j>0 && k>0 ) {
  	        // real(kind=4), dimension(0:3,0:ip+1,-1:jp+1,-1:kp+1)  :: uvw
		float4 uvw_i_j_k     =uvw[FTNREF3D(i  ,j  ,k  ,ip+2   ,jp+3,0,-1,-1)];
		float4 uvw_im1_j_k   =uvw[FTNREF3D(i-1,j  ,k  ,ip+2   ,jp+3,0,-1,-1)];
		float4 uvw_i_jm1_k   =uvw[FTNREF3D(i  ,j-1,k  ,ip+2   ,jp+3,0,-1,-1)];
		float4 uvw_i_j_km1   =uvw[FTNREF3D(i  ,j  ,k-1,ip+2   ,jp+3,0,-1,-1)];
		float4 uvw_ip1_j_k   =uvw[FTNREF3D(i+1,j  ,k  ,ip+2   ,jp+3,0,-1,-1)];
		float4 uvw_i_jp1_k   =uvw[FTNREF3D(i  ,j+1,k  ,ip+2   ,jp+3,0,-1,-1)];
		float4 uvw_i_0_k     =uvw[FTNREF3D(i  ,0  ,k  ,ip+2   ,jp+3,0,-1,-1)];
		float4 uvw_i_1_k     =uvw[FTNREF3D(i  ,1  ,k  ,ip+2   ,jp+3,0,-1,-1)];
		float4 uvw_i_j_kp1   =uvw[FTNREF3D(i  ,j  ,k+1,ip+2   ,jp+3,0,-1,-1)];

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


	    float u_i_j_k=uvw_i_j_k.s0;
	    float v_i_j_k=uvw_i_j_k.s1;
	    float w_i_j_k=uvw_i_j_k.s2;

	    float u_im1_j_k= uvw_im1_j_k.s0;
	    float v_im1_j_k= uvw_im1_j_k.s1;
	    float w_im1_j_k= uvw_im1_j_k.s2;

	    float u_i_jm1_k = uvw_i_jm1_k.s0;
	    float v_i_jm1_k = uvw_i_jm1_k.s1;
	    float w_i_jm1_k = uvw_i_jm1_k.s2;

	    float u_i_j_km1= uvw_i_j_km1.s0;
	    float v_i_j_km1= uvw_i_j_km1.s1;
	    float w_i_j_km1= uvw_i_j_km1.s2;

	    float u_ip1_j_k= uvw_ip1_j_k.s0;
	    float v_ip1_j_k= uvw_ip1_j_k.s1;
	    float w_ip1_j_k= uvw_ip1_j_k.s2;

	    float u_i_jp1_k= uvw_i_jp1_k.s0;
	    float v_i_jp1_k= uvw_i_jp1_k.s1;
	    float w_i_jp1_k= uvw_i_jp1_k.s2;

	    float u_i_0_k= uvw_i_0_k.s0;
	    float v_i_0_k= uvw_i_0_k.s1;
	    float w_i_0_k= uvw_i_0_k.s2;

	    float u_i_1_k= uvw_i_1_k.s0;
	    float v_i_1_k= uvw_i_1_k.s1;
	    float w_i_1_k= uvw_i_1_k.s2;

	    float u_i_j_kp1 = uvw_i_j_kp1.s0;
	    float v_i_j_kp1 = uvw_i_j_kp1.s1;
	    float w_i_j_kp1 = uvw_i_j_kp1.s2;

	    float u_i_jm_k = uvw_i_jm_k.s0;
	    float v_i_jm_k = uvw_i_jm_k.s1;
	    float w_i_jm_k = uvw_i_jm_k.s2;

	    float u_im_j_k = uvw_im_j_k.s0;
	    float u_imm1_j_k = uvw_imm1_j_k.s0;

	    float u_i_jmm1_k = uvw_i_jmm1_k.s0;
	    float v_i_jmm1_k = uvw_i_jmm1_k.s1;
	    float w_i_jmm1_k = uvw_i_jmm1_k.s2;

	    float v_ip1_jmm1_k = uvw_ip1_jmm1_k.s1;

	    float v_i_jmm1_kp1 = uvw_i_jmm1_kp1.s1;

	    float u_im1_j_kp1 = uvw_im1_j_kp1.s0;
	    float u_im1_jp1_k = uvw_im1_jp1_k.s0;

	    float v_ip1_jm1_k = uvw_ip1_jm1_k.s1;
	    float v_i_jm1_kp1 = uvw_i_jm1_k1.s1;
	    float v_ip1_0_k = uvw_ip1_0_k.s1;

	    float w_i_jp1_km1 = uvw_i_jp1_km1.s2;
	    float w_ip1_j_km1 = uvw_ip1_j_km1.s2;

    	  if ( j < jm+1 && i < im+1) {

    	  if (k< km+1 ) {
// So in here, i in [1..ip]; dx1 is -1:ip+1 , or 0:ip+2 in C, so access -1 is i+1
    		  float nou1_i_j_k = (u_im1_j_k + u_i_j_k) / 2.F;
    		  float nou1_ip1_j_k = (u_i_j_k + u_im1_j_k) / 2.F;

         diu1_i_j_k =  (-u_im1_j_k + u_i_j_k)/dx1[i+1];
         diu1_ip1_j_k = (-u_i_j_k+u_ip1_j_k)/dx1[i+2];

         cov1_i_j_k = nou1_i_j_k*diu1_i_j_k;
         cov1_ip1_j_k = nou1_ip1_j_k*diu1_ip1_j_k;

        float nou5_i_j_k =  (v_i_jm1_k+v_i_j_k)/2.0F;
        float nou5_i_jp1_k =(v_i_j_k+v_i_jp1_k)/2.0F;
         diu5_i_j_k =  (-v_i_jm1_k+v_i_j_k)/dy1[j];
         diu5_i_jp1_k =  (-v_i_j_k+v_i_jp1_k)/dy1[j+1];

         cov5_i_j_k = nou5_i_j_k*diu5_i_j_k;
         cov5_i_jp1_k = nou5_i_jp1_k*diu5_i_jp1_k;

        float nou9_i_j_k = (w_i_j_km1+w_i_j_k)/2.0F;
        float nou9_i_j_kp1 = (w_i_j_k+w_i_j_kp1)/2.0F;
         diu9_i_j_k = (-w_i_j_km1+w_i_j_k)/dzn[k+1];
         diu9_i_j_kp1 = (-w_i_j_k+w_i_j_kp1)/dzn[k+2];
         cov9_i_j_k = nou9_i_j_k*diu9_i_j_k;
         cov9_i_j_kp1 = nou9_i_j_kp1*diu9_i_j_kp1;

        float nou2_i_j_k =  (dx1[i+2]*v_i_jm1_k+dx1[i+1]*v_ip1_jm1_k) /(dx1[i+1]+dx1[i+2]);
        float nou2_i_jp1_k =  (dx1[i]*v_i_j_k+dx1[i+1]*v_ip1_j_k) /(dx1[i+1]+dx1[i+2]);

         diu2_i_j_k = 2.0F*(-u_i_jm1_k+u_i_j_k)/(dy1[j-1]+dy1[j]);
         diu2_i_jp1_k =  2.0F*(-u_i_j_k+u_i_jp1_k)/(dy1[j-1]+dy1[j]);
         cov2_i_j_k = nou2_i_j_k*diu2_i_j_k;
         cov2_i_jp1_k = nou2_i_jp1_k*diu2_i_jp1_k;


        float nou4_i_j_k = (dy1[j+1]*u_im1_j_k+dy1[j]*u_im1_jp1_k) /(dy1[j]+dy1[j+1]);
        float nou4_ip1_j_k =(dy1[j+1]*u_i_j_k+dy1[j]*u_i_jp1_k) /(dy1[j]+dy1[j+1]);
         diu4_i_j_k = 2.0F*(-v_im1_j_k+v_i_j_k)/(dx1[i]+dx1[i+1]);
         diu4_ip1_j_k = 2.0F*(-v_i_j_k+v_ip1_j_k)/(dx1[i]+dx1[i+1]);
         cov4_i_j_k = (nou4_i_j_k-u0)*diu4_i_j_k;
         cov4_ip1_j_k = (nou4_ip1_j_k-u0)*diu4_ip1_j_k;

        float nou7_i_j_k = (dzn[k+2]*u_im1_j_k+dzn[k+1]*u_im1_j_kp1) /(dzn[k+1]+dzn[k+2]);
        float nou7_ip1_j_k = (dzn[k+2]*u_i_j_k+dzn[k+1]*u_i_j_kp1) /(dzn[k+1]+dzn[k+2]);
         diu7_i_j_k = 2.0F*(-w_im1_j_k+w_i_j_k)/(dx1[i]+dx1[i+1]);
         diu7_ip1_j_k = 2.0F*(-w_i_j_k+w_ip1_j_k)/(dx1[i+1]+dx1[i+2]);
         cov7_i_j_k = (nou7_i_j_k-u0)*diu7_i_j_k;
         cov7_ip1_j_k = (nou7_ip1_j_k-u0)*diu7_ip1_j_k;

        float nou8_i_j_k = (dzn[k+2]*v_i_jm1_k+dzn[k+1]*v_i_jm1_kp1) /(dzn[k+1]+dzn[k+2]);
        float nou8_i_jp1_k =  (dzn[k+2]*v_i_j_k+dzn[k+1]*v_i_j_kp1) /(dzn[k+1]+dzn[k+2]);
         diu8_i_j_k = 2.0F*(-w_i_jm1_k+w_i_j_k)/(dy1[j-1]+dy1[j]);
         diu8_i_jp1_k = 2.0F*(-w_i_j_k+w_i_jp1_k)/(dy1[j]+dy1[j+1]);
         cov8_i_j_k = nou8_i_j_k*diu8_i_j_k;
         cov8_i_jp1_k = nou8_i_jp1_k*diu8_i_jp1_k;

    	  }

    	    float nou3_i_j_k = (dx1[i+2]*w_i_j_km1+dx1[i+1]*w_ip1_j_km1) /(dx1[i+1]+dx1[i+2]);
    	    float nou3_i_j_kp1 = (dx1[i+2]*w_i_j_k+dx1[i+1]*w_ip1_j_k) /(dx1[i+1]+dx1[i+2]);
    	     diu3_i_j_k = (-u_i_j_km1+u_i_j_k)/dzs[k];
    	     cov3_i_j_k = nou3_i_j_k*diu3_i_j_k;

    	    float nou6_i_j_k = (dy1[j+1]*w_i_j_km1+dy1[j]*w_i_jp1_km1) /(dy1[j]+dy1[j+1]);
    	    float nou6_i_j_kp1 = (dy1[j+1]*w_i_j_k+dy1[j]*w_i_jp1_k) /(dy1[j]+dy1[j+1]);
    	     diu6_i_j_k = (-v_i_j_km1+v_i_j_k)/dzs[k];
    	     cov6_i_j_k = nou6_i_j_k*diu6_i_j_k;
    	    if (k<km+1) {
        	     diu3_i_j_kp1 = (-u_i_j_k+u_i_j_kp1)/dzs[k+1]; // will overflow
        	     cov3_i_j_kp1 = nou3_i_j_kp1*diu3_i_j_kp1; // will overflow

    	    	 diu6_i_j_kp1 = (-v_i_j_k+v_i_j_kp1)/dzs[k+1]; // will overflow: v(i,j,kp+2) does not exist
    	         cov6_i_j_kp1 = nou6_i_j_kp1*diu6_i_j_kp1; // will overflow
    	    }

    	  }

    	  diu[FTNREF3D(i,j,k,ip+4,jp+3,-1,0,0)] = (float16)(diu1_i_j_k,diu2_i_j_k,diu3_i_j_k,diu4_i_j_k, diu5_i_j_k, diu6_i_j_k, diu7_i_j_k, diu8_i_j_k, diu9_i_j_k, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f);
    	  //diu[FTNREF3D(i,j,k,ip+4,jp+3,-1,0,0)] = (float16)(diu1_ip1_j_k,diu2_i_jp1_k,diu3_i_j_kp1,diu4_ip1_j_k, diu5_i_jp1_k, diu6_i_j_kp1, diu7_ip1_j_k, diu8_i_jp1_k, diu9_i_j_kp1, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f);
//! ====================================
    	if (k< km+1) {
    		if( j < jm+1 && i==im) {
    			diu[FTNREF3D(im+1,j,k,ip+4,jp+3,-1,0,0)].s0 = diu1_i_j_k;
    	}

    	if (j==jm) {
    		diu[FTNREF3D(i,0,k,ip+4,jp+3,-1,0,0)].s1 = diu2_i_j_k;
    		diu[FTNREF3D(i,0,k,ip+4,jp+3,-1,0,0)].s4 = diu5_i_j_k;
    	   	if (k< km) {
    	    			diu[FTNREF3D(i,0,k,ip+4,jp+3,-1,0,0)].s7 = diu8_i_j_k;
    	    		}
    	}
    	if  (j==1) {
    		diu[FTNREF3D(i,jm+1,k,ip+4,jp+3,-1,0,0)].s1 = diu2_i_j_k;
    		diu[FTNREF3D(i,jm+1,k,ip+4,jp+3,-1,0,0)].s4 = diu5_i_j_k;
    		if (k< km) {
    		    			diu[FTNREF3D(i,jm+1,k,ip+4,jp+3,-1,0,0)].s7 = diu8_i_j_k;
    		    		}
    	}

    	if (j < jm+1 && i==im) {
    		diu[FTNREF3D(im+1,j,k,ip+4,jp+3,-1,0,0)].s3 = diu4_i_j_k;
    	}


    	}

    	if (i==im  && j < jm+1) {
    		diu[FTNREF3D(im+1,j,k,ip+4,jp+3,-1,0,0)].s6 = diu7_i_j_k;
    	}


//! --les

    	if (i==im) {
    		diu[FTNREF3D(im+1,j,k,ip+4,jp+3,-1,0,0)].s1 = diu2_i_j_k;
    		diu[FTNREF3D(im+1,j,k,ip+4,jp+3,-1,0,0)].s2 = diu3_i_j_k;
    	}


    	if (j==jm) {
    		diu[FTNREF3D(i,0,k,ip+4,jp+3,-1,0,0)].s3 = diu4_i_j_k;
    		diu[FTNREF3D(i,0,k,ip+4,jp+3,-1,0,0)].s5 = diu6_i_j_k;
    	}

    	if (i < im+1 && j<jm+1 && k<km+1) {
        float covx1 = (dx1[i+2]*cov1_i_j_k+dx1[i+1]*cov1_ip1_j_k) /(dx1[i+1]+dx1[i+2]);
        float covy1 = (cov2_i_j_k+cov2_i_jp1_k)/2.0F;
        float covz1 = (cov3_i_j_k+cov3_i_j_kp1)/2.0F;
        float covc = covx1+covy1+covz1;
        float dfu1_i_j_k = 2.0F*(-diu1_i_j_k+diu1_ip1_j_k)/(dx1[i+1]+dx1[i+2]) + (-diu2_i_j_k+diu2_i_jp1_k)/dy1[j] +   (-diu3_i_j_k+diu3_i_j_kp1)/dzn[k+1];
        float df = vn*dfu1_i_j_k;
        float f_i_j_k = (-covc+df);

        covx1 = (cov4_i_j_k+cov4_ip1_j_k)/2.0F;
        covy1 = (dx1[j+1]*cov5_i_j_k+dx1[j]*cov5_i_jp1_k) /(dx1[j]+dx1[j+1]);
        covz1 = (cov6_i_j_k+cov6_i_j_kp1)/2.0F;
        covc = covx1+covy1+covz1;
        float dfv1_i_j_k = (-diu4_i_j_k+diu4_ip1_j_k)/dx1[i+1] +2.0F*(-diu5_i_j_k+diu5_i_jp1_k)/(dy1[j]+dy1[j+1]) +(-diu6_i_j_k+diu6_i_j_kp1)/dzn[k+1];
        float dg = vn*dfv1_i_j_k;
        float g_i_j_k = (-covc+dg); // FIXME: could we use fgh[i,j,k] instead?

        // what happens if we would set this for k=km? Apparently, nothing!
//        if (k!=km) {
            covx1 = (cov7_i_j_k+cov7_ip1_j_k)/2.0F;
            covy1 = (cov8_i_j_k+cov8_i_jp1_k)/2.0F;
            covz1 = (dzn[k+1+1]*cov9_i_j_k+dzn[k+1]*cov9_i_j_kp1) /(dzn[k+1]+dzn[k+1+1]);
            covc = covx1+covy1+covz1;
            float dfw1_i_j_k= (-diu7_i_j_k+diu7_ip1_j_k)/dx1[i+1] +(-diu8_i_j_k+diu8_i_jp1_k)/dy1[j] +(-diu9_i_j_k+diu9_i_j_kp1)/dzs[k+1];
            float dh = vn*dfw1_i_j_k;
            float h_i_j_k = (-covc+dh);
//        }
            fgh[FTNREF3Du0(i,j,k,im,jm) ]= (float4)(f_i_j_k,g_i_j_k,h_i_j_k,0.0F);

    	}
      } // if idx in range

} // END of velfg_kernel

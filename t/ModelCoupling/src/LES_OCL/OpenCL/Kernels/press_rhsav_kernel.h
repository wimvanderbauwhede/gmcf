
void bondfgc_ (__global float4 *fgh,const int im,const int jm,const int km,int i,int j, int k) ;

void press_rhsav_kernel (
#ifdef COMBINED_KERNEL
		  __local float* rhsavs_th,
		  __local float* areas_th,
#endif
		__global float4* uvw,
		__global float4* fgh,
        __global float *rhs,
		__global float *dx1,__global float *dy1,__global float *dzn,
        __global float* chunks_num,
        __global float* chunks_denom,
        const float dt,
		const unsigned int im,
		const unsigned int jm,
		const unsigned int km
		        );

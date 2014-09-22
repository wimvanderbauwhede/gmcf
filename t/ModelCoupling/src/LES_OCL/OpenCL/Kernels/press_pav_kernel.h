
void press_pav_kernel (
#ifdef COMBINED_KERNEL
		__local float* pavs_th,
		__local float* pcos_th,
#endif
        __global float2* p2,
        __global float *dx1,__global float *dy1,__global float *dzn,
		__global float *chunks_num,
		__global float *chunks_denom,
        const unsigned int im,
        const unsigned int jm,
        const unsigned int km

		);
